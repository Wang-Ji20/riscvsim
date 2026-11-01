# Design: floating point

How riscvsim computes floating-point results that are correct to the last
bit, in every rounding mode, with the right exception flags. Module:
`Fpu`.

## The problem

Most decimal numbers can't be stored exactly in binary floating point,
so every operation *rounds*. RISC-V lets a program choose among five
*rounding modes* (to nearest, toward zero, down, up, and to nearest with
ties away from zero). After each operation it also records *exception
flags*: was the result inexact (NX)? Did it overflow (OF), underflow (UF),
divide by zero (DZ), or do something invalid (NV)?

OCaml, like most languages, offers only one rounding mode (to nearest,
ties to even) and no access to the flags. So riscvsim must work out the
other modes and the flags itself.

## The key idea: know the exact error

Suppose the host computes `0.1 + 0.2` and rounds to nearest, giving `r`.
If we also knew whether the *exact* sum is above or below `r`, every
other mode would follow:

```
        exact sum
            |
  ----+-----x--+--------+----   representable numbers
     lo        r        hi
```

- **Round down** gives `lo` if the exact value is below `r`, otherwise `r`.
- **Round up** gives `r` if the exact value is below `r`, otherwise `hi`.
- **Toward zero** behaves like down for positive results and like up for
  negative ones.
- **Ties away from zero** differs from nearest only if the exact value
  is exactly halfway.
- **NX** is set whenever the exact value isn't `r`.

So `Fpu` describes every result as a small record:

```ocaml
type approx = { r : float;     (* the round-to-nearest result *)
                err : int;     (* sign of (exact - r): -1, 0 or +1 *)
                tie : bool;    (* exactly halfway between r and a neighbour? *)
                scale : int }  (* see "overflow" below *)
```

The rest of `Fpu` is about computing `err` and `tie` exactly.

## Computing the error exactly

There are classic tricks, called *error-free transforms*, that compute
the rounding error of an operation exactly using ordinary floating-point
arithmetic:

- **Addition: TwoSum** (Knuth). Six additions give both `s = a + b`
  (rounded) and the exact error `e`, with `a + b = s + e` exactly.
- **Fused multiply-add: ErrFma** (Boldo and Muller, 2011). A short
  sequence gives the exact error of `a × b + c` as two numbers.
- **Products, quotients and square roots: exact comparisons.** To
  compare the exact result with a candidate `v`, we rewrite the question
  so it needs only additions and exact products. For example, `a / b > v`
  is the same question as `a − b × v > 0` (if `b > 0`). The products are
  split exactly (TwoProduct), and the sum of all the pieces is
  accumulated without any loss using Shewchuk's *floating-point
  expansions*. The sign of that exact sum answers the question.

These tricks fail if an intermediate value becomes too small (a
*subnormal* number) or too large. So operands are first scaled by a power
of two into a safe range; scaling by a power of two is exact.

## The fast path

Expansions are lists, so they're slow. Usually they aren't needed: when
every value involved lies between 2^-900 and 2^900, far from both ends
of the double range, a single fused multiply-add gives the error
exactly:

| Operation | Exact error from one FMA | Sign of (exact − rounded) |
|---|---|---|
| `r = a × b` | `a × b − r` | its sign |
| `r = a / b` | `a − r × b` | its sign, times the sign of `b` |
| `r = √a` | `a − r × r` | its sign |

`Fpu.mul`, `div` and `sqrt` take this path when the values are in that
range, and fall back to `mul_exact`, `div_exact` and `sqrt_exact`
otherwise. A square root is never exactly halfway between two doubles,
so its fast path never reports a tie. A property test runs both paths on
20,000 inputs clustered around the range limits and requires identical
answers. Widening the range to 2^-1060 makes it fail within a few
hundred cases.

FMA itself checks its common case (no tiny or huge intermediates)
before doing any scaling.

## Single precision

32-bit floats are computed using 64-bit arithmetic. Rounding twice (first
to 64 bits, then to 32) can be wrong in rare cases, so we use **round to
odd** for the first step. Round to odd truncates and then sets the last
bit if anything was lost. It's a known theorem that round-to-odd at 53
bits followed by any rounding to 24 bits gives the correctly rounded
result, because 53 ≥ 24 + 2.

## Overflow

A result too large to represent overflows. Under IEEE 754, overflow is
decided as if the exponent had no limit, which depends on the rounding
mode. Rounding toward zero, a value just above the largest float gives
the largest float with only NX set, not infinity.

To get this right, an overflowing operation is recomputed in a
scaled-down domain (that's the `scale` field), rounded there, and scaled
back. If scaling back overflows, the standard's table decides between
infinity and the largest finite number for each mode.

## Smaller rules that matter

- **Signed zeros.** `x + (−x)` is `+0`, except when rounding down, where
  it is `−0`.
- **Sticky terms.** When one addend is far below half an ulp of the
  result (for example a tiny product in `a × b + c`), it can't change the
  rounded value. It can only decide the direction of the error, or break
  a tie. `with_sticky` handles exactly that case.
- **NaN handling.** Results that are NaN are always the *canonical* NaN.
  A single-precision value in a 64-bit register must be *NaN-boxed*;
  otherwise it reads as NaN.

## How we know it's right

- Property tests check, on thousands of random inputs, that round-down,
  round-to-nearest and round-up bracket the exact value and lie one ulp
  apart, with NX exactly when they differ. FMA is checked against exact
  integer arithmetic.
- The riscv-arch-test compliance suite's 170 double-precision tests pass
  bit for bit, flags included.
- Random programs are compared against Spike, which uses the Berkeley
  SoftFloat library. This found three subtle FMA bugs (a tiny product, a
  negligible addend, and an overflowing sum) that no fixed test had
  caught. Each is now a regression test.

## Trade-offs

This approach is pure OCaml and needs no C code, but it costs many
operations per floating-point instruction. The fast path cut the cost
of the `fp` benchmark by about 3×, but floating-point code still runs
about 4× slower than integer code. Most of what remains is glue
(closures per format, tuples for result and flags), not arithmetic. Reading
the host CPU's exception flags through a C binding would be faster
still, at the price of C code and host-specific behaviour.
