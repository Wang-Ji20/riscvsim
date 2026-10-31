# riscvsim

An **RV64GC** user-mode RISC-V simulator, assembler and disassembler in OCaml.
This is the ISA baseline of application-class chips such as SiFive's
P-series, T-Head C910 and SpacemiT K1: RV64 I+M+A+F+D+C, Zicsr and
Zifencei, plus the Zba/Zbb/Zbs bit-manipulation extensions from the
RVA22/RVA23 profiles.

```
$ dune exec -- riscvsim run examples/fib.s
0: 0
10: 55
...
90: 2880067194370816120

$ dune exec -- riscvsim asm --rvc examples/factorial.s -o fact.elf   # real ELF64 executable
$ dune exec -- riscvsim run fact.elf
$ dune exec -- riscvsim disasm --rvc examples/hello.s
_start:
     10000:  00001597  auipc a1, 0x1
     10004:  00058593  addi a1, a1, 0
     10008:      82ae  add t0, zero, a1
.L1$1:
     1000a:  0002c303  lbu t1, 0(t0)
...
```

## Commands

| Command | What it does |
|---------|--------------|
| `riscvsim run FILE` | Assemble a `.s` file (or load an ELF) and run it. `--trace` prints each instruction, `--regs` dumps registers, `--fuel N` limits the instruction count. Exits with the program's exit code. |
| `riscvsim asm FILE -o OUT` | Assemble to an ELF64 executable. `--list` prints a listing. |
| `riscvsim disasm FILE` | Disassemble a `.s` file or ELF executable. |

All three accept `--rvc` to compress eligible instructions to 16 bits.

## Layout

```
lib/
  reg.ml freg.ml       register names (one functor, two instances)
  instr.ml             the instruction ADT + disassembler, mnemonic tables
  opcode.ml            every opcode/funct table: the single source of truth
  encode.ml decode.ml  32-bit codec, which reads opcode.ml both ways
  compressed.ml        RVC: expand 16-bit parcels, compress instructions
  fpu.ml               IEEE-754: all 5 rounding modes, fflags, NaN-boxing
  memory.ml            sparse little-endian 64-bit address space
  cpu.ml               hart state, execute, CSRs, LR/SC; ecall goes to a hook
  os.ml                Linux process: initial stack, fds, brk/mmap, syscall table
  lexer.ml parser.ml   GNU-style assembly syntax
  assembler.ml         two-pass assembler, pseudo-instructions, li, RVC
  elf.ml image.ml      ELF64 writer/loader, program images
  disasm.ml            mixed 16/32-bit disassembly
bin/main.ml            cmdliner CLI
examples/              10 sample programs + expected output
guest/libc/            rvlibc: freestanding C11 libc for guest programs
guest/tests/           C programs for the three-way rvlibc/glibc/host test
guest/ocamlrun/        cross-build script for OCaml's bytecode interpreter
test/                  QCheck properties, unit and golden tests, clang cross-check,
                       guest programs
test/conformance/      riscv-tests environment, arch-test model, suite runner
```

## Design notes

- **Typed instructions.** `Instr.t` is a variant built from small enums
  (`branch_op`, `reg_op`, `fmt`, `rm`, ...). The encoder, decoder,
  compressor and executor are exhaustive pattern matches over it.
- **Real fetch/decode/execute.** Programs live in memory as machine code.
  The CPU fetches a 16-bit parcel and decides from its low bits whether
  the instruction is compressed.
- **Correct floating point in every rounding mode.** The host only rounds to
  nearest-even. `Fpu` describes each exact result by its RNE value, the sign
  of the error and whether it's a tie, then steps to the correct neighbour
  for RTZ, RDN, RUP and RMM. The error comes from:
  - TwoSum for addition
  - Boldo and Muller's ErrFma for fused multiply-add
  - exact sign tests with Shewchuk expansions for products, quotients and
    roots, which stay exact in the subnormal range
  Overflowing results are rounded in a scaled domain (unbounded exponent),
  so OF, UF and NX follow IEEE 754 to the letter, including the sign of
  exact-zero sums under RDN. Single precision rounds to odd in binary64,
  then to binary32. riscv-arch-test's 170 D-extension tests pass
  bit-for-bit, flags included.
- **The assembler's `li`** uses LLVM's algorithm (lui/addiw, then a
  recursive shift-and-add for 64-bit values). `la`, `call`, `tail` and
  `ld rd, sym` use `auipc` pairs, and `%hi`, `%lo`, `%pcrel_hi` and
  `%pcrel_lo` are supported. Errors carry line numbers, and all of them are
  reported, not just the first.
- **RVC in the assembler.** An instruction is compressed when its operands
  are known in pass 1. Label-relative branches stay 32-bit, because that
  avoids relaxation.

## Running real Linux programs

`riscvsim run prog [args...]` runs a static RV64 Linux executable the way
`qemu-user` does. `Os` builds the Linux initial stack (argc, argv, envp,
auxv with `AT_PHDR`, `AT_RANDOM`, ...) and implements about 60 syscalls
as a table of small handlers: files backed by the host filesystem, `brk`,
`mmap`, `mremap`, time, `uname`, `getrandom`, and more. `--strace` logs
each call by name, and the host environment is passed through.

```
$ riscv64-suse-linux-gcc -static -O2 hello.c -o hello   # glibc 2.44
$ riscvsim run hello world
```

Note that guest programs can read and write host files, as with
`qemu-user`.

Images built by riscvsim's assembler also get RARS-style debug calls:
print_int (1), print_float (2), print_double (3), print_string (4),
exit (10), print_char (11), print_hex (34), print_unsigned (36). They're
enabled per image, because 34 and 36 collide with Linux's `mkdirat` and
`symlinkat`.

## rvlibc: a small libc of our own

`guest/libc` is a freestanding C11 libc of about 900 lines, in the spirit
of musl. It has a crt0 that reads the initial stack, raw `ecall` syscall
wrappers, strings, a size-class `malloc` (with `brk`, plus `mmap` for
large blocks), `qsort`, buffered stdio, and an exact `printf`: doubles are
expanded to their full decimal value with a small bignum, so `%f`/`%e`/`%g`
match glibc digit for digit.

## Tests

```
dune test
```

- **QCheck2 properties**, each with a shrinking counterexample:
  - `decode (encode i) = i` for random instructions of every format
  - the decoder is strict: every word it accepts re-encodes to itself
  - RVC `expand (compress i)` is equivalent to `i`
  - assembling the disassembly gives back the same instruction
  - `li` materialises any int64 in 8 instructions or fewer
  - `mulh*` match a 16-bit-limb reference, and div/rem satisfy their identities
  - `*W` ops match `Int32` arithmetic, and Zbb/Zbs match naive references
  - memory: load after store returns the stored bytes, across page boundaries
  - FP: RNE matches the host, directed modes bracket the exact result one ulp
    apart (with NX exactly when they differ), and singles via round-to-odd
    match host double-then-single rounding
  - random programs give identical final state assembled with and without RVC
- **Exhaustive**: all 65,536 16-bit parcels.
- **Golden**: the 10 examples, each run plain, with RVC, and through an ELF
  write/read round trip.
- **Differential**: about 6,000 random instructions and 4,000 RVC candidates
  assembled by clang (`-march=rv64gc_zba_zbb_zbs`) must be byte-identical
  to ours. This step is skipped if clang isn't installed.
- **Three-way guest programs** (`guest/tests/*.c`): each program is built
  against rvlibc and against static glibc for riscv64, run on riscvsim,
  and must print exactly what a native x86-64 build prints, with the same
  exit status.

### OCaml on riscvsim

```
guest/ocamlrun/build.sh ../riscv-deps/ocaml-5.4.1 /tmp/ocamlrun.rv   # once
dune build @ocaml
```

OCaml 5.4.1's bytecode interpreter, cross-compiled for riscv64 (static
glibc), runs on riscvsim. Each `guest/ocaml/*.ml` is compiled by the host's
`ocamlc` and must give the same output and exit status under the host's
`ocamlrun` and under riscvsim. The stress test covers the GC, float
printing, exceptions, deep recursion, file I/O, `Format` and effect
handlers.

### Conformance suites

```
dune build @conformance     # about 30 s; needs riscv64-suse-linux-gcc
```

| Suite | Result |
|-------|--------|
| riscv-tests: rv64u{i,m,a,f,d,c} and rv64uzba/zbb/zbs | 150 / 150 |
| riscv-arch-test 2.7.4: I, M, C, D, Zifencei (vs reference signatures) | 265 / 265 (2 skipped: a toolchain incompatibility, and one test that needs M-mode traps) |

The suites aren't vendored. Set `RISCV_TESTS` and `RISCV_ARCH_TEST`, or
check them out under `../riscv-deps/`.

During development the property tests caught a wrong `c.andi` funct2 field.
QCheck shrank the failure to `andi s0, s0, 0`, and the exhaustive sweep and
the random-program property flagged the same bug. The clang cross-check
found two places where our compression choices were legal but different
from clang's: `c.addi` vs `c.addi16sp`, and commuting `or`/`and`/`xor`/`addw`.
Both now match clang.

## Roadmap

See [PLAN.md](PLAN.md) for what was built and [ROADMAP.md](ROADMAP.md) for
the long-term goal: booting RISC-V Linux in the simulator, then building and
running riscvsim inside it.
