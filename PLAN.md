# RISC-V Simulator in OCaml — Plan

Goal: a self-contained **RV64GC** user-mode simulator, written in idiomatic
OCaml, with its own assembler, so `.s` programs run without an external
toolchain.

## Target ISA

Application-class chips that compete with mainstream x86 cores (SiFive
P550/P670/P870, T-Head C910/C920, SpacemiT K1, Tenstorrent Ascalon) implement
the RVA22/RVA23 profiles. Their shared baseline, and what Linux distributions
compile for, is **RV64GC**. We implement:

| Extension | Contents |
|-----------|----------|
| RV64I | 64-bit base integer ISA, including the `*W` word ops, `ld`/`sd`/`lwu` |
| M | multiply/divide, including 128-bit `mulh*` and the `*W` forms |
| A | `lr`/`sc` and AMOs (`.w`/`.d`, aq/rl bits) |
| F, D | single/double floating point, NaN-boxing, `fcsr` flags and rounding modes |
| C | 16-bit compressed instructions (decode, plus compression in the assembler) |
| Zicsr, Zifencei | CSR instructions (`fcsr`, `cycle`, `time`, `instret`), `fence.i` |
| Zba, Zbb, Zbs | bit manipulation (mandatory in RVA22/RVA23) |

Out of scope: V (vector), privileged modes, virtual memory, and timing models
(caches, pipelines, branch prediction). This is a functional ISA simulator, in
the same category as Spike or `qemu-user`.

## Design decisions

- **Typed core**: instructions are an ADT (`Instr.t`). Encoder, decoder,
  compressor and executor are exhaustive matches over it.
- **Single source of truth for encodings**: `Opcode` holds every
  opcode/funct table. `Encode` reads it forwards and `Decode` reads it
  backwards.
- **Real fetch/decode/execute**: programs live in memory as machine code. The
  CPU fetches 16 or 32 bits depending on the low bits, so
  `decode (encode i) = i` is a checked property.
- **Values**: integer registers are `int64`. FP registers are raw `int64` bit
  patterns, with singles NaN-boxed as the spec requires.
- **Memory**: sparse 4 KiB pages in a `Hashtbl`, little-endian.
- **Assembler**: hand-written lexer and parser, then two passes. Pseudo-instructions
  expand into deferred `symtab -> pc -> Instr.t` closures. `li` uses the LLVM
  constant-materialisation algorithm. Errors are a `result` with line numbers.
- **ELF**: write ELF64 executables from the assembler, load ELF64 executables
  (for example from a real `riscv64-*-gcc`), and read `.o` sections (used to
  cross-check against clang).
- **Syscalls**: the Linux RV64 ABI (`read`, `write`, `exit`, `exit_group`,
  `brk`, ...) plus RARS-style debug calls (print int/float/string/char).

## Tasks

- [x] 1. Project skeleton (dune, lib/bin/test/examples)
- [x] 2. `Reg`/`Freg`: integer and FP register names (ABI + numeric)
- [x] 3. `Instr`: instruction ADT + disassembler (pretty printer)
- [x] 4. `Opcode` + `Encode`: 32-bit encodings with range checks
- [x] 5. `Decode`: 32-bit decoding
- [x] 6. `Compressed`: RVC decode (expand) and compress
- [x] 7. `Memory`: sparse little-endian memory
- [x] 8. `Fpu`: IEEE helpers (NaN-boxing, canonical NaN, conversions, flags, fclass)
- [x] 9. `Cpu`: state, CSRs, LR/SC reservation, `step`/`run`, traps, syscalls
- [x] 10. `Lexer`/`Parser`: operands, `%hi`/`%lo`/`%pcrel_*`, directives
- [x] 11. `Assembler`: two passes, pseudo-instructions, `li` for 64-bit values, RVC
- [x] 12. `Elf`: write/load executables, read object sections
- [x] 13. CLI: `riscvsim run | asm | disasm` with `--trace`, `--regs`, `--rvc`
- [x] 14. 10 sample programs + expected output
- [x] 15. Tests:
      - QCheck properties: `decode (encode i) = i` for random instructions,
        RVC `expand (compress i)` is equivalent to `i`, a disassemble/reassemble
        round trip, `li` materialises any int64, ALU ops agree with a reference
        model, memory load-after-store, and plain vs RVC runs of random
        programs behave the same
      - unit tests: semantics edge cases, assembler errors
      - golden tests: examples run plain, with RVC, and via ELF
- [x] 16. Cross-check encodings against clang `-target riscv64 -march=rv64gc_zba_zbb_zbs`
- [x] 17. README

## Sample programs

| # | File | Exercises |
|---|------|-----------|
| 1 | `hello.s` | Linux `write`/`exit` syscalls, `.data` strings |
| 2 | `arith.s` | 64-bit ALU, `li` of 64-bit constants, `*W` sign extension |
| 3 | `fib.s` | loops; fib(90) needs 64 bits |
| 4 | `factorial.s` | recursion, stack frames with `sd`/`ld`, `call`/`ret` |
| 5 | `bubble_sort.s` | doubleword arrays, nested loops |
| 6 | `sieve.s` | byte memory (`lbu`/`sb`), `.space` |
| 7 | `muldiv.s` | M edge cases: div by 0, overflow, 128-bit `mulh*`, `divw` |
| 8 | `atomics.s` | A: `amoadd`, `amomax`, `lr`/`sc` compare-and-swap loop |
| 9 | `float.s` | F/D: Newton's sqrt, `fmadd`, conversions, compares, `fcsr` flags |
| 10 | `bitmanip.s` | Zba/Zbb/Zbs: `clz`, `cpop`, `rev8`, `sh2add`, `bset`; CSR counters |

## Status (2026-10-31)

Tasks 1–17 are done, and `dune test` passes:

- 16 QCheck2 properties, run with several random seeds
- exhaustive RVC sweep: 65,536 parcels
- golden tests: 10 examples × 3 modes (plain, RVC, via ELF)
- assembler diagnostics and semantics unit tests
- clang differential test: 5,957 random 32-bit instructions and 4,000 RVC
  candidates, all byte-identical

Bugs found by the tests: a `c.andi` encoding error (found by QCheck, shrunk
to `andi s0, s0, 0`), plus two RVC choices that differed from clang's
(`c.addi` vs `c.addi16sp`, and commuted `or`/`and`/`xor`/`addw`). All are
fixed.

Known limitations:

- binary64 FMA honours only RNE.
- Label-relative branches aren't compressed (no relaxation).
- Speed is about 6 MIPS. See ROADMAP Phase 8.

The next steps are in ROADMAP.md, starting with Phase 0.4–0.7 (official
riscv-tests, then musl/OCaml binaries in user mode).
