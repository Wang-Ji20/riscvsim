# Working agreement for agents

Rules for any coding agent (Claude Code and others) working on riscvsim.
They record what the maintainer and the agent have agreed on.

## Git workflow

- **Agents commit only to `dev`.** Never commit to `main`, merge into it,
  or rewrite its history. The maintainer reviews `dev` and merges it.
- **Agent commits are unsigned and made under the agent's own name:**

  ```
  git -c user.name="Claude" -c user.email="noreply@anthropic.com" commit --no-gpg-sign
  ```

  The maintainer's GPG key signs only the maintainer's own commits and
  merges. An agent never uses it or triggers its passphrase prompt.
- **Every commit is atomic.** It holds one logical change, builds, and
  passes `dune test` on its own.
- **Every commit spans at most 500 lines**, counted as insertions plus
  deletions in `git diff --cached --shortstat`. Split larger work into a
  sequence of atomic commits. If a change truly can't be split, ask first.
- Commit messages: an imperative summary line, then a body that says why.
  End with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>` when
  Claude wrote the change.

## Code

- Write modern, readable, functional, modular code that follows
  clean-code principles: small focused functions, clear names, no
  god-functions. Breaking a rule is fine when there's a reason; say why in
  a comment or the commit message.
- OCaml should be idiomatic and fluent: ADTs and exhaustive matches,
  `result`/`option` over sentinel values, and table-driven code where it
  keeps one source of truth (for example `Opcode`, the syscall table).
- Guest C code (`guest/`) is modern, freestanding C11.
- Run `dune fmt` before committing.
- After getting something to work, do a cleanup pass before calling it
  done.

## Self-contained repository

- The repo builds and tests without external projects. Nothing third-party
  is vendored.
- Outside code (glibc, Spike, riscv-tests, riscv-arch-test, the OCaml
  sources, clang) may be used as a **reference or test oracle only**. It
  lives outside the repo, by default in `../riscv-deps/`, and every test
  that needs it skips cleanly when it's missing.
- Building or running downloaded third-party code needs the maintainer's
  explicit permission.

## Testing

- `dune test` must pass before every commit. It runs the unit and QCheck
  property tests, the clang encoding cross-check, and the three-way guest
  program comparison (rvlibc and glibc on riscvsim vs the native host).
- Use property-based tests (QCheck2) for new logic wherever a property can
  be stated, alongside example and golden tests.
- Prefer differential testing against an independent oracle: clang, the
  host's libc or OCaml runtime, or reference signatures.
- When changing the ISA, FPU or Linux emulation, also run
  `dune build @conformance` (riscv-tests, riscv-arch-test) and
  `dune build @ocaml` (OCaml bytecode on riscvsim).
- Every bug fixed gets a regression test.

## Planning

- `PLAN.md` is the live checklist for current work. Update it as tasks
  land.
- `ROADMAP.md` holds the long-term plan: boot RISC-V Linux on riscvsim,
  then build and run riscvsim inside it. Mark items there as they are
  finished.
- Scope: RV64GC + Zba/Zbb/Zbs, as a functional (not cycle-accurate)
  simulator. Bigger scope changes go into the roadmap first.
