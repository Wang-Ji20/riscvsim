# User manual

This manual shows how to build riscvsim, run programs on it, write your
own RISC-V assembly, and run the tests. Terms in *italics* are explained
in the [glossary](glossary.md).

## 1. What riscvsim does

riscvsim imitates a 64-bit RISC-V computer. It can:

- **assemble** RISC-V *assembly language* into machine code,
- **run** that machine code, or a Linux program compiled for RISC-V, as a
  *user-mode* Linux process,
- **disassemble** machine code back into readable instructions.

It supports the *instruction set* used by today's application-class
RISC-V chips: RV64GC plus the Zba, Zbb and Zbs bit-manipulation
*extensions*.

## 2. Building

You need **OCaml 5.4** with `dune`, `cmdliner` and `qcheck-core`
(available through `opam`). Then:

```
dune build          # builds the riscvsim command
dune test           # runs the main test suite (about a minute)
```

The command is built at `_build/default/bin/main.exe`, and
`dune exec -- riscvsim ...` runs it. The examples below write just
`riscvsim`.

Optional tools unlock extra tests (see section 8): a RISC-V cross
compiler (`riscv64-suse-linux-gcc` on openSUSE), `clang`, and the Spike
simulator.

## 3. A first program

The repository ships ten sample programs in `examples/`:

```
$ riscvsim run examples/hello.s
Hello from RV64GC!
$ riscvsim run examples/fib.s
0: 0
10: 55
...
90: 2880067194370816120
```

`riscvsim run` accepts either an assembly file (`.s`), which it assembles
first, or an *ELF* executable.

## 4. Commands

### `riscvsim run FILE [ARGS...]`

Runs a program as a Linux process. Any `ARGS` are passed to the program,
and so is your environment.

| Option | Effect |
|--------|--------|
| `--trace`, `-t` | Print every executed instruction to stderr |
| `--regs`, `-r` | Print all registers when the program stops |
| `--strace` | Print every *syscall* with its arguments and result |
| `--fuel N` | Stop after N instructions (see *fuel*) |
| `--rvc`, `-c` | Use 16-bit *compressed* instructions when assembling |
| `--signature FILE` | After the run, save the test *signature* to FILE |
| `--stats` | Print the instruction count, time, speed (*MIPS*) and allocation |
| `--profile` | Print the most executed mnemonics and instructions (slower) |

The exit status is the program's own exit status. Two values are special:

- **134**: the program hit a *trap* (for example an illegal instruction).
  riscvsim prints its name, cause code and *tval*, for example
  `illegal instruction (cause 2, tval 0x0) at pc 0x10000`, then the
  registers.
- **124**: the instruction limit set by `--fuel` was reached.

### `riscvsim asm FILE [-o OUT]`

Assembles FILE into an ELF executable (by default FILE with `.elf`
instead of `.s`). `--list`/`-l` also prints a listing showing each
address, its machine code and the instruction.

### `riscvsim disasm FILE`

Prints the code of an assembly file or ELF executable as
instructions:

```
$ riscvsim disasm --rvc examples/hello.s
_start:
     10000:  00001597  auipc a1, 0x1
     10004:  00058593  addi a1, a1, 0
     10008:      82ae  add t0, zero, a1
```

Four-digit codes are 16-bit compressed instructions.

## 5. Writing assembly

A program is a text file with one instruction or directive per line.
`#` starts a comment. Execution starts at the label `_start`, or `main`
if there is no `_start`.

```
    .data
msg:    .asciz "hi\n"          # a zero-terminated string

    .text
_start:
    li    a0, 1                # file descriptor 1 = standard output
    la    a1, msg              # address of the string
    li    a2, 3                # length
    li    a7, 64               # syscall number for write
    ecall
    li    a0, 0                # exit status
    li    a7, 93               # syscall number for exit
    ecall
```

**Registers** can be written by number (`x10`, `f10`) or by their usual
names: `zero`, `ra` (return address), `sp` (stack pointer), `a0`–`a7`
(arguments and results), `t0`–`t6` (temporaries), `s0`–`s11` (saved), and
`fa0`, `ft0`, `fs0` and so on for floating point.

**Sections and data.** `.text` holds code. `.data`, `.rodata` and `.bss`
hold data. Data directives: `.byte`, `.half`, `.word`, `.dword` (1, 2, 4
and 8 bytes), `.float`, `.double`, `.ascii`, `.asciz`, `.space N`, and
`.align N` (align to 2ᴺ bytes). `.equ NAME, VALUE` defines a constant.

**Labels.** `name:` marks an address. Numbered labels such as `1:` can be
repeated; `1b` means "the nearest `1:` before here" and `1f` "the
nearest after".

**Pseudo-instructions** expand into real ones. The most useful:

| Write | Meaning |
|-------|---------|
| `li rd, value` | load any 64-bit constant |
| `la rd, label` | load an address |
| `mv rd, rs` / `not` / `neg` | copy / bitwise not / negate |
| `j label`, `call label`, `ret` | jump, call a function, return |
| `beqz`, `bnez`, `bgt`, `ble`, ... | branches on comparisons |
| `ld rd, label` | load from a labelled address |
| `csrr rd, csr`, `frflags rd`, ... | read control registers |

**Debug calls.** Programs built by riscvsim's own assembler can also use
simple print calls, which keeps examples short. Put the number in `a7`
and the value in `a0` (or `fa0`), then `ecall`:

| `a7` | Prints |
|------|--------|
| 1 | `a0` as a signed integer |
| 2 / 3 | `fa0` as a float / double |
| 4 | the string at address `a0` |
| 11 | `a0` as a character |
| 34 / 36 | `a0` in hexadecimal / as unsigned |
| 10 | (exits with status 0) |

Errors are reported with line numbers, all of them at once:

```
bad.s:2: error: addi a0, a0, 9999: immediate 9999 out of range [-2048, 2047]
bad.s:3: error: undefined symbol missing
```

## 6. Running C programs

Any static RISC-V Linux executable runs directly:

```
$ riscv64-suse-linux-gcc -static -O2 hello.c -o hello
$ riscvsim run hello world
```

riscvsim emulates about 60 Linux system calls: files, memory (`brk`,
`mmap`), time, process information and more. Programs see your real
files, just as with `qemu-user`, so be careful with programs that delete
or overwrite things. `--strace` shows every call the program makes.

You can also link against **rvlibc**, riscvsim's own small C library;
see [its design document](design/rvlibc.md) for the build command.

## 7. Running OCaml programs

OCaml's bytecode interpreter can be cross-compiled once:

```
guest/ocamlrun/build.sh PATH/TO/ocaml-5.4.1-source /tmp/ocamlrun.rv
```

Then compile a program on your machine and run its bytecode on riscvsim:

```
$ ocamlc -o hello.byte hello.ml
$ riscvsim run /tmp/ocamlrun.rv hello.byte
```

The OCaml source must be the same version as your `ocamlc`.

## 8. Testing

| Command | What it runs | Needs |
|---------|--------------|-------|
| `dune test` | unit and property tests; the clang encoding check; C programs on rvlibc and glibc vs your machine | clang and the cross compiler are optional |
| `dune build @conformance` | the official riscv-tests and riscv-arch-test suites | cross compiler; suites in `../riscv-deps/` |
| `dune build @spike` | random programs compared against Spike | cross compiler; Spike |
| `dune build @ocaml` | OCaml bytecode compared against your machine | a built `/tmp/ocamlrun.rv` |

`dune build --profile release @bench` runs the benchmarks and prints
their speed (see [the performance method](perf/method.md)). The release
profile matters: the default one turns off optimizations across modules.

Every test whose tools are missing prints "skipping" instead of failing.
Paths can be overridden with environment variables: `RISCV_TESTS`,
`RISCV_ARCH_TEST`, `SPIKE`, `OCAMLRUN_RISCV`. The Spike test also
accepts `SPIKE_DIFF_COUNT`, `SPIKE_DIFF_LENGTH` and `SPIKE_DIFF_SEED`.

## 9. Memory layout

Useful when reading traces:

| Address | Contents |
|---------|----------|
| `0x10000` | program code (`.text`), followed by data sections, each page-aligned |
| `0x20_0000_0000` and up | memory handed out by `mmap` |
| below `0x3f_ffff_f000` | the stack, growing downwards |

Memory that was never written reads as zero.
