# Performance method

How we produce trustworthy performance numbers, find out where the time
goes, and report the results.

## 1. Benchmarks

A benchmark is a guest program with a known, fixed amount of work. We use
several kinds, because an interpreter can be fast at one thing and slow
at another.

| Kind | Examples | What it isolates |
|------|----------|------------------|
| **Micro** (assembly) | integer loop, memory streaming, branches, FP arithmetic, compressed code, syscalls | One part of the simulator at a time |
| **Kernels** (C, built with rvlibc) | matrix multiply, sorting, hashing, string processing, `malloc` churn | Realistic mixes of instructions |
| **Runtime** | the OCaml stress test on `ocamlrun` | A large, real program |
| **System** (later) | Linux boot to a shell | The final target |

Rules for benchmarks:

- Each one prints a checksum, so a "fast" result can't come from wrong
  execution.
- Each one is deterministic: the same number of guest instructions on
  every run.
- They live in `bench/` with our own sources. External programs (such as
  CoreMark) may be used as extra inputs, kept outside the repository like
  the conformance suites.

## 2. Collecting data

A harness (`bench/run.exe`, to be built) runs every benchmark and records
one row per run:

- the guest instruction count (from the simulator's `instret` counter)
- wall-clock time, and from these two, MIPS
- OCaml allocation, from `Gc.quick_stat` before and after the run
- peak memory (resident set size)
- host CPU counters when `perf stat` is available: cycles, instructions,
  branch misses, cache misses
- metadata: git commit, host CPU model, OCaml version, compiler flags,
  date

Each benchmark runs **once to warm up, then 5 times**. We report the
**median** and the spread (minimum and maximum). Results are written as
CSV for tools and as a Markdown table for people.

To keep measurements stable: close other heavy programs, use a fixed CPU
frequency if possible, and compare only numbers taken on the same machine.

## 3. Profiling: finding where the time goes

Use these from cheapest to most detailed.

1. **Built-in statistics** (`riscvsim run --stats`, to be added). The
   simulator counts executed instructions by kind, memory page lookups,
   decode work and syscalls. It's cheap enough to leave on, and it answers
   "what is the guest doing?".
2. **`perf stat`.** Host CPU counters for the whole run. Dividing host
   instructions by guest instructions gives the overall cost of one
   simulated instruction.
3. **`perf record` with call graphs** (`perf record --call-graph dwarf`),
   then `perf report` or a flame graph. This shows which OCaml functions
   use the time. Build with `-g` for useful function names.
4. **Allocation and GC.** `OCAMLRUNPARAM=v=0x400` prints GC totals at
   exit. Comparing `Gc.quick_stat` before and after a run gives words
   allocated per guest instruction. OCaml 5's runtime events (the `olly`
   tool) show GC pauses over time.

A typical investigation goes: check the built-in stats (is the guest
doing something unusual?), then `perf stat` (are we spending many host
instructions per guest instruction?), then `perf record` (which function
is it?), then the allocation numbers (is the garbage collector involved?).

## 4. Analysing results

- **Compare against a baseline:** the previous commit, or the last
  milestone report.
- **Noise:** a change counts only if the medians differ by more than 3%
  *and* the min–max ranges don't overlap. Anything smaller is reported
  as "no change".
- **Attribute every gain or loss.** `perf diff` between two profiles
  shows which functions got faster or slower. If a gain can't be
  explained, be suspicious: it may come from a benchmark that stopped
  doing its work.
- **Check the checksums.** A benchmark whose checksum changed is a
  correctness bug, not a performance result.

## 5. Reporting

- **Per optimization:** the commit message carries a before-and-after
  table (MIPS for each benchmark and the geometric mean).
- **History:** each harness run appends to `docs/perf/results.csv`, so
  trends stay visible.
- **Per milestone:** a short report in `docs/perf/reports/` saying which
  target was reached, the final numbers, what worked, what didn't, and
  the updated workload estimates from [the README](README.md).
