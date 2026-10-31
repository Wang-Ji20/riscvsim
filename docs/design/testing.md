# Design: testing strategy

How we know riscvsim is right, and how the tests are organised.

## The principle: compare against oracles

A simulator can be wrong in ways nobody would think to test. The defence
is to compare it with something independent, an *oracle*, on as many
inputs as possible. When the two disagree, one of them has a bug, and so
far it has always been riscvsim.

| Oracle | What it checks |
|--------|----------------|
| clang's assembler | our instruction encodings, byte for byte |
| the host's glibc and C compiler | C programs: same output on riscvsim as natively |
| the host's OCaml runtime | OCaml bytecode: same output on riscvsim as natively |
| reference signatures (riscv-arch-test) | instruction results and FP flags |
| Spike, the reference simulator | complete processor state after random programs |
| exact integer arithmetic | floating-point rounding and fused multiply-add |

## The layers

### `dune test`: always run

- **Unit tests** for specific behaviours: `x0` is always zero, traps are
  reported, single-precision values are NaN-boxed, assembler error
  messages, the ELF round trip, HTIF exits.
- **Property-based tests** (QCheck2), each on thousands of random
  inputs, for example:
  - decoding an encoded instruction gives it back,
  - the decoder accepts only words that re-encode to themselves,
  - expanding a compressed instruction gives an equivalent one,
  - assembling the disassembly gives the same instruction,
  - `li` produces any 64-bit value,
  - multiply and divide agree with independent reference
    implementations,
  - rounding modes bracket the exact result,
  - random programs give identical results with and without
    compression.

  When a property fails, QCheck *shrinks* the input to a minimal example.
  The first real bug found this way shrank to the single instruction
  `andi s0, s0, 0`.
- **Exhaustive:** all 65,536 16-bit values through the compressed
  decoder.
- **Golden:** the ten `examples/` programs must print exactly their
  `.expected` files, built three ways (plain, compressed, and through an
  ELF file).
- **clang cross-check:** about 10,000 random instructions encoded by
  clang and by riscvsim must be identical.
- **Three-way C programs:** each `guest/tests/*.c` program is built
  against rvlibc and against glibc, run on riscvsim, and compared with a
  native build on the host.

### Extra suites: run when changing the ISA, FPU or Linux emulation

- **`@conformance`:** the official riscv-tests (150 programs that check
  themselves) and riscv-arch-test 2.7.4 (265 programs compared with
  reference signatures).
- **`@spike`:** random programs of a few hundred instructions each,
  covering the whole instruction set, started from awkward initial values
  (NaNs, subnormals, badly boxed floats). The final registers and flags
  must match Spike's. On a mismatch, a binary search over program
  prefixes finds the first instruction where the two disagree and prints
  its inputs, so a failure in a 300-instruction program becomes a
  one-line bug report.
- **`@ocaml`:** OCaml programs on a cross-compiled `ocamlrun`, compared
  with the host.

Every suite that needs outside tools or files skips cleanly when they're
missing, so `dune test` works on a fresh machine.

## Bugs the strategy caught

| Bug | Found by |
|-----|----------|
| `c.andi` encoded as `c.srai` | property test, shrunk to one instruction |
| two compression choices differing from clang | clang cross-check |
| FMA ignored directed rounding | riscv-tests `rv64ud-fmadd` |
| subnormal results lost NX/UF | riscv-arch-test |
| overflow flag ignored the rounding mode | riscv-arch-test |
| `x + (−x)` gave `+0` when rounding down | riscv-arch-test |
| three FMA flag corner cases | Spike random testing |
| short file reads | OCaml runtime on riscvsim |

## Rules

- Every bug fix comes with a regression test. Floating-point cases found
  by Spike go into the table `spike_fpu_regressions` in
  `test/test_main.ml` before they are fixed.
- New logic gets a property test wherever a property can be stated.
- A test that needs external tools must skip, never fail, when they are
  missing.
