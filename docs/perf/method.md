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

Today's set is the micro benchmarks in `bench/micro/`. Each `.s` file
has a `.expected` file holding its output, which was checked against an
independent model of the same computation (a few lines of Python):

| Benchmark | Work | Instructions |
|-----------|------|-------------:|
| `int_loop` | xorshift random numbers, summed | 22.5 M |
| `memory` | 300 passes over a 64 KiB array | 17.2 M |
| `branches` | a branch on a pseudo-random bit | 25.5 M |
| `fp` | convert, fused multiply-add, divide, square root | 18.0 M |
| `calls` | recursive Fibonacci, fib(29) | 18.3 M |
| `syscalls` | 300,000 `getpid` calls | 1.5 M |

`int_loop` and `calls` also run assembled with compressed instructions
(`int_loop/rvc`, `calls/rvc`). C kernels and the OCaml stress test are
still to be added.

## 2. Collecting data

The harness `bench/run.exe` runs every benchmark and records:

- the guest instruction count (from the simulator's `instret` counter)
- wall-clock time, and from these two, MIPS
- OCaml allocation per guest instruction, from `Gc.minor_words` before
  and after the run
- peak memory (resident set size, `VmHWM` in `/proc/self/status`)
- metadata: date, git commit, host CPU model, OCaml version

Each run happens in a forked child process, so it starts with a fresh
heap and its peak memory belongs to that run alone. Host CPU counters
are not collected automatically; use `perf stat` by hand (section 3).

```
dune build --profile release @bench    # print the table
dune exec --profile release bench/run.exe -- --csv docs/perf/results.csv
dune exec --profile release bench/run.exe -- --filter calls --runs 3
```

Each benchmark runs **once to warm up, then 5 times**. We report the
**median** and the spread (minimum and maximum). Results are written as
CSV for tools and as a Markdown table for people.

**Build in release mode.** In its default (dev) profile, dune compiles
with `-opaque`, which turns off inlining across modules, for example of
`Memory.page_no` into `Hart`. That changes the numbers a lot, so
benchmarks always use `--profile release`.

To keep measurements stable: close other heavy programs, use a fixed CPU
frequency if possible, and compare only numbers taken on the same machine.

**Compare by alternating.** Some machines, including laptops and WSL2,
change speed by 30% or more over minutes. A "before" number from an hour
ago is then useless. Instead, build the harness for both versions and run
them alternately, `old new old new`:

```
dune build --profile release bench/run.exe && cp _build/default/bench/run.exe /tmp/run_new.exe
git stash && dune build --profile release bench/run.exe && cp _build/default/bench/run.exe /tmp/run_old.exe && git stash pop
for v in old new old new; do /tmp/run_$v.exe --dir bench/micro; done
```

## 3. Profiling: finding where the time goes

Use these from cheapest to most detailed.

1. **Built-in statistics.** `riscvsim run --stats` prints the
   instruction count, run time, MIPS and allocation per instruction. It
   costs nothing. `riscvsim run --profile` counts executions per
   instruction address and prints the most executed mnemonics and
   instructions. It slows the run down, and it answers "what is the
   guest doing?".
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
