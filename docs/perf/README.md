# Performance: goals and targets

Correctness comes first in riscvsim, but speed decides what we can
actually run. This folder holds the performance requirements, the way we
measure, and the plan for getting faster.

- **This page:** why speed matters, what we measure, and the targets.
- **[Method](method.md):** benchmarks, profiling, data collection,
  analysis and reporting.
- **[Optimization plan](plan.md):** the ordered list of improvements, with
  expected gains and risks.

## Why speed matters

The roadmap's goals are measured in *instructions*, so they translate
directly into waiting time. These are rough estimates, to be replaced by
measurements as we reach each one:

| Workload | Guest instructions |
|----------|--------------------|
| OCaml stress test (`guest/ocaml/stress.ml`), measured | 4 × 10⁸ |
| Boot a minimal Linux to a shell | about 5 × 10⁸ |
| Boot a full distribution (Debian) | about 10¹⁰ |
| Build the OCaml compiler inside the guest | about 10¹² |

At the measured **6 MIPS** of today's interpreter:

| Workload | Time at 6 MIPS |
|----------|----------------|
| OCaml stress test | 65 s |
| Minimal Linux boot | about 1.5 min |
| Debian boot | about 28 min |
| OCaml build in the guest | about 2 days |

The first two are fine for development. A half-hour boot makes a
distribution painful to work with, and a two-day compiler build makes
Phases 9 and 10 of the roadmap impractical.

## What we measure

| Metric | Meaning | Why |
|--------|---------|-----|
| **MIPS** | Guest instructions per second of wall time | The headline number |
| **Host instructions per guest instruction** | Host CPU instructions spent per simulated one | Explains MIPS; independent of clock speed |
| **Allocation per guest instruction** | Memory the OCaml runtime allocates per simulated instruction | Garbage collection is often an interpreter's hidden cost |
| **Peak memory** | Resident set size of the riscvsim process | Must stay close to the guest's own memory use |
| **Start-up time** | Time to load a program and run its first instruction | Matters for test suites with many small programs |
| **Determinism** | Same instruction count on every run | Required for reproducible debugging |

## Targets

Each milestone unlocks a roadmap phase:

| Milestone | Target | Minimal boot | Debian boot | OCaml build | Unlocks |
|-----------|--------|--------------|-------------|-------------|---------|
| Today | 6 MIPS | 1.5 min | 28 min | 2 days | Phases 0–5 |
| **P1: clean interpreter** | **50 MIPS** | 10 s | 3.5 min | 6 h | Phase 6 (Linux boot), comfortably |
| **P2: fast interpreter** | **200 MIPS** | 2.5 s | 50 s | 1.4 h | Phases 7–9 (distribution, toolchain) |
| **P3: compiler (JIT)** | **1000+ MIPS** | 0.5 s | 10 s | 17 min | Phase 10 (nested simulation) |

Secondary targets, from P1 onwards:

- **Allocation:** zero words allocated per instruction in the steady
  state of the integer path.
- **Host instructions per guest instruction:** at most 40 at P1 and at
  most 12 at P2.
- **Peak memory:** at most the guest's memory plus 50%.
- **Start-up:** under 50 ms for a small static program.
- **Determinism:** always. Optimizations must never change results or
  instruction counts.

These numbers are for one core of a recent desktop CPU (around 4 GHz).
They are deliberately round. When a measurement shows an estimate above
was wrong by more than 2×, update the tables.

## Rules for performance work

1. **Measure first.** No optimization without a profile showing where the
   time goes.
2. **One change per commit,** with before-and-after numbers in the commit
   message.
3. **Correctness gates stay green:** `dune test`, `@conformance`, `@spike`
   and `@ocaml` must all pass. A faster wrong answer is worthless.
4. **Keep the code readable.** Unsafe tricks (for example unchecked array
   access) are allowed only in measured hot spots, with a comment saying
   why they are safe there.
