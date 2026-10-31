# riscvsim

riscvsim is a RISC-V computer simulated in software, written in OCaml.
It runs programs built for 64-bit RISC-V Linux on an ordinary PC, one
instruction at a time. It includes its own assembler, disassembler and
small C library.

The long-term goal: boot RISC-V Linux on riscvsim, then build and run
riscvsim inside that simulated machine. See the [roadmap](ROADMAP.md).

## What works today

- **The instruction set of current application-class RISC-V chips:**
  RV64GC (integer, multiply/divide, atomics, single and double floating
  point, compressed instructions) plus the Zba/Zbb/Zbs bit-manipulation
  extensions.
- **Real programs:** static Linux executables built with glibc, C
  programs on our own C library, and OCaml's bytecode interpreter
  running OCaml programs, all with output identical to a native run.
- **Verified against independent references:**

  | Check | Result |
  |-------|--------|
  | Official riscv-tests | 150 / 150 |
  | riscv-arch-test 2.7.4 compliance suite | 265 / 265 |
  | Random programs vs the Spike reference simulator | identical across millions of instructions |
  | Instruction encodings vs clang | byte-identical |

## Quick start

You need OCaml 5.4 with `dune`, `cmdliner` and `qcheck-core`.

```
$ dune build
$ dune exec -- riscvsim run examples/fib.s
0: 0
10: 55
...
90: 2880067194370816120
$ dune test
```

## Documentation

Start with **[docs/README.md](docs/README.md)**. It contains:

- the [user manual](docs/user-manual.md): commands, writing assembly,
  running C and OCaml programs, testing,
- the [architecture](docs/architecture.md): how the parts fit together,
- [design documents](docs/README.md#reading-order) for each subsystem,
- the [performance plan](docs/perf/README.md),
- a [glossary](docs/glossary.md) of every technical term.

Work in progress is tracked in [PLAN.md](PLAN.md). Contributor rules,
for humans and AI agents alike, are in [AGENTS.md](AGENTS.md).
