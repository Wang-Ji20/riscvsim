# riscvsim documentation

riscvsim is a program that pretends to be a RISC-V computer. You give it
a program written for a RISC-V processor, and it runs that program on
your ordinary computer, one instruction at a time. The long-term goal is
to run a whole operating system (Linux) this way, and then build riscvsim
itself inside that simulated machine.

These documents explain how to use riscvsim, how it is built, and why.
They assume general computer knowledge but no background in RISC-V,
OCaml or emulators. When a word is unfamiliar, look it up in the
[glossary](glossary.md).

## Reading order

1. **[User manual](user-manual.md)**: install, build, run programs, run
   the tests.
2. **[Architecture](architecture.md)**: the big picture, meaning the main
   parts and how a program flows through them.
3. **Design documents**: one per subsystem, explaining the decisions
   behind it:
   - [Instructions and their encodings](design/instructions.md)
   - [Floating point](design/floating-point.md)
   - [The assembler](design/assembler.md)
   - [Running Linux programs](design/linux-emulation.md)
   - [rvlibc, our C library](design/rvlibc.md)
   - [Testing strategy](design/testing.md)
4. **[Performance](perf/README.md)**: goals, how we measure, and the plan
   to make riscvsim fast enough to boot Linux.

## Other documents in the repository

- [`README.md`](../README.md): a short project overview.
- [`PLAN.md`](../PLAN.md): the checklist of current work.
- [`ROADMAP.md`](../ROADMAP.md): the long-term plan, from today's
  simulator to running it inside simulated Linux.
- [`AGENTS.md`](../AGENTS.md): the working rules for contributors,
  human or AI.
