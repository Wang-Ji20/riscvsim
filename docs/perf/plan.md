# Optimization plan

An ordered list of improvements. The order puts measurement first, then
the changes with the best gain for their risk. Every expected gain below
is a guess; the profile decides.

## Where the time goes today

The current interpreter was written for clarity. For every guest
instruction it:

1. **Looks up the memory page** holding the instruction in a hash table,
   then reads 2 or 4 bytes.
2. **Decodes** the bits into an `Instr.t` value. That allocates memory,
   and it is repeated every time the instruction runs.
3. **Matches** on the instruction kind and computes the result with
   `Int64` operations. Registers live in an `int64 array`, and in OCaml
   such an array stores each value as a separate heap object, so every
   register write allocates.
4. **Checks** fuel, tracing and exit conditions.

So a simple `add` costs a hash lookup, several allocations and many
branches. The [baseline](reports/2026-11-01-baseline.md) measured 5.7
MIPS and about 70 words allocated per instruction. Half the time goes to
step 2: the decoder searches its opcode tables with OCaml's generic
comparison. So the decode cache (O4) is done before O2 and O3.

## The steps

| # | Step | Expected gain | Risk | Milestone |
|---|------|---------------|------|-----------|
| O1 | Benchmarks, harness, `--stats` | none (measurement) | none | — |
| O2 | Unboxed registers | 2–3× | low | P1 |
| O3 | Software TLB for memory pages | 1.5–2× | low | P1 |
| O4 | Decode cache | about 2× | medium | P1 |
| O5 | Pre-compiled closures | 1.5–2× | medium | P2 |
| O6 | Block-at-a-time execution | about 1.3× | medium | P2 |
| O7 | Compiler settings (flambda) | 1.1–1.5× | low | P2 |
| O8 | Faster floating point | large for FP code | medium | P2 |
| O9 | Just-in-time compiler | 3–10× | high | P3 |

### O1: Measure

Build what [the method](method.md) describes: the benchmark set, the
harness, and the built-in statistics. Record the baseline. Nothing gets
faster, but every later step depends on this.

### O2: Unboxed registers

Store registers so that reading and writing them doesn't allocate, for
example in a `Bytes` buffer accessed with `Bytes.get_int64_le`, which the
OCaml compiler turns into a plain memory access. The goal is zero
allocation per integer instruction.

### O3: Software TLB

Most accesses hit the same few pages. Keep a small cache, indexed by
address bits, of recently used pages, and fall back to the hash table
only on a miss. Hardware CPUs do the same with a TLB (translation
lookaside buffer). This structure also prepares for virtual memory
(roadmap Phase 3).

### O4: Decode cache

Decode each instruction once. Keep, per code page, an array of decoded
instructions indexed by offset. A store into a code page, or a
`fence.i` instruction, invalidates that page's cache. This matters for
self-modifying code and for the riscv-tests `fence_i` test.

### O5: Pre-compiled closures

Instead of matching on the instruction kind on every execution, turn
each decoded instruction into an OCaml function that already knows its
registers and immediate value: for `add a0, a1, a2`, a function that only
adds. This is called *closure compilation* or *threaded code*. It removes
most of the dispatch work.

### O6: Block-at-a-time execution

Group straight-line instructions into *basic blocks* (sequences that end
at a branch). Do the per-instruction bookkeeping (fuel, exit checks, and
later interrupts) once per block instead of once per instruction.

### O7: Compiler settings

Try the flambda optimizer (`-O3`), and unchecked array access in the one
or two hottest functions, each with a comment explaining why it's safe.
Measure whether each flag pays off.

### O8: Faster floating point

The exact-rounding machinery in `Fpu` (see
[the floating-point design](../design/floating-point.md)) is correct but
costly. Most operations are exact or use round-to-nearest, so add a fast
path for those and keep the slow path only for directed rounding and
corner cases. Another option is reading the host CPU's own exception
flags through a small C binding. Either way, `@spike` and `@conformance`
must stay green.

### O9: Just-in-time compiler

Translate hot blocks into host machine code at run time. This is how
QEMU reaches thousands of MIPS. It's a large project, worth starting only
if P2 is reached and P3 is still needed.

## Checkpoints

After each step: run the benchmarks, write the numbers into the commit
message, and append them to `results.csv`. When a milestone target is
met, write its report and update the targets and estimates in
[the README](README.md).
