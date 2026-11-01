# Architecture

This document gives the big picture: the main parts of riscvsim, what
each is responsible for, and how a program travels through them. The
[design documents](README.md#reading-order) then go into each part.

## The idea in one paragraph

A real processor repeats one loop forever: *fetch* the next instruction
from memory, *decode* its bits to find out what it means, *execute* it,
and move on. riscvsim does exactly that in software. Around this loop
sit the tools that get programs into memory (an assembler and an ELF
loader) and a small stand-in for the operating system that answers the
program's requests (system calls).

## The parts

```
                 assembly text (.s)                   ELF file
                        |                                |
                        v                                |
              +-------------------+                      |
              |  Lexer -> Parser  |   text to statements |
              +-------------------+                      |
                        |                                |
                        v                                v
              +-------------------+   Image    +-------------------+
              |     Assembler     | ---------> |   Elf (read/write)|
              +-------------------+            +-------------------+
                        |  Image: segments, entry, symbols   |
                        +----------------+-------------------+
                                         v
                              +---------------------+
                              |  Os (Linux process) |  stack, files, memory,
                              +---------------------+  syscalls, HTIF
                                         |
                                         v
       +--------------------------------------------------------------+
       |  Hart: fetch -> decode -> execute, registers, CSRs, atomics  |
       |                                                              |
       |   Decode / Compressed    Fpu (IEEE 754)    Decode_cache      |
       +--------------------------------------------------------------+
                                         |
                                         v
       +--------------------------------------------------------------+
       |  Bus: RAM (Memory, sparse pages) and devices (Device)        |
       +--------------------------------------------------------------+
```

The parts, from the bottom up:

- **`Instr`** defines what an instruction *is*, as an OCaml data type.
  Everything else speaks in terms of it.
- **`Opcode`, `Encode`, `Decode`, `Compressed`** convert instructions
  to and from bits: 32-bit instructions, and the 16-bit compressed ones.
- **`Memory`** is the simulated memory: a sparse 64-bit address space,
  allocated in 4 KiB pages on first write.
- **`Machine`** groups harts, their shared bus and a virtual clock, and
  runs the harts in turn. `Os` uses a machine with one hart.
- **`Bus`** connects the hart to RAM and to memory-mapped *devices*
  (`Device`). In user mode all addresses are RAM; in system mode other
  addresses fault (see [the bus](design/system-bus.md)).
- **`Snapshot`** saves a whole machine to a string and restores it.
- **`Decode_cache`** remembers decoded instructions per memory page, so
  each one is decoded once. A write to a cached page throws that page's
  entries away (see [instructions](design/instructions.md)).
- **`Trap`** lists the RISC-V *traps* (see the glossary) with their
  cause codes. The CPU raises them as an OCaml exception.
- **`Fpu`** computes floating-point results exactly as the IEEE 754
  standard requires, including all rounding modes and exception flags.
- **`Hart`** (*hardware thread*, one processor core) holds the processor
  state (32 integer registers, 32 FP registers, the program counter, a
  few control registers) and executes instructions. When the program
  executes `ecall`, the hart asks its *environment*: either an emulated
  one, which handles the call in OCaml, or none, in which case `ecall`
  traps as on real hardware.
- **`Os`** is that environment: a Linux process. It builds the initial
  stack (arguments, environment), keeps a table of open files backed by
  real host files, manages the program's memory, and implements the
  system calls.
- **`Lexer`, `Parser`, `Assembler`** turn assembly text into an `Image`.
- **`Elf`** reads and writes ELF executables, converting them to and from
  an `Image`.
- **`Image`** is the common format for a loadable program: memory
  segments, the entry address, and symbol names.
- **`Disasm`** prints machine code as text.
- **`Stats`** measures runs (speed, allocation) and profiles them (which
  instructions run most), for [performance work](perf/README.md).
- **`bin/main.ml`** is the command-line tool that ties them together.

## The life of a program

Take `riscvsim run hello.s`:

1. **Parse.** The lexer splits each line into tokens; the parser groups
   them into statements: labels, directives such as `.data`, and
   instructions with their operands.
2. **Assemble.** The assembler makes two passes. The first decides where
   everything goes and records label addresses; the second fills in
   addresses and encodes each instruction into bits. The result is an
   `Image`.
3. **Load.** `Os.boot` creates a `Hart`, copies the image's segments into
   `Memory`, builds the stack with argc, argv, the environment and the
   *auxiliary vector*, and points the program counter at the entry
   address.
4. **Run.** `Os.run` drives the CPU loop:
   - read 16 bits at the program counter; if the low two bits aren't
     `11`, it's a compressed instruction, so expand it; otherwise read 16
     more bits and decode a 32-bit instruction. The `Decode_cache` keeps
     the result, so the next time this address runs, the decoding is
     skipped,
   - execute it, updating registers and memory,
   - advance the program counter by 2 or 4, unless the instruction
     jumped.
5. **System calls.** When the program executes `ecall`, the CPU calls
   `Os`, which looks up the syscall number (register `a7`) in its table,
   performs the request (for example writing bytes to standard output),
   and puts the result in `a0`.
6. **Stop.** The program calls `exit`, which ends the loop with its exit
   status. A trap (such as an illegal instruction) or running out of
   *fuel* also ends it.

Running an ELF file skips steps 1 and 2: `Elf.read` produces the image
directly.

## Principles that shape the code

- **Types describe instructions.** `Instr.t` makes impossible
  instructions unrepresentable, and the compiler checks that the encoder,
  decoder, compressor and executor each handle every case.
- **One source of truth.** Each table of facts appears once. The opcode
  numbers in `Opcode` drive both encoding and decoding, and the mnemonic
  tables in `Instr` drive both the assembler and the disassembler, so
  they can't drift apart.
- **Real machine code.** Programs live in memory as bits, and the CPU
  decodes them as hardware would. Self-modifying code and compressed
  instructions work for that reason.
- **Small, swappable pieces.** The hart doesn't know about Linux. Its
  environment is either `Emulated handler` (`Os` installs one) or
  `Trapping`. That is what lets a system-mode machine run the guest's
  own operating system without touching the hart.
- **Trust, but verify against oracles.** Every important behaviour is
  checked against something independent: clang, glibc, the host OCaml
  runtime, reference signatures, or Spike. See
  [testing](design/testing.md).

## Repository layout

```
lib/             the simulator library (all modules above)
bin/             the riscvsim command
examples/        sample assembly programs with expected output
guest/libc/      rvlibc, a small C library for guest programs
guest/tests/     C test programs
guest/ocaml/     OCaml test programs
guest/ocamlrun/  script that cross-builds OCaml's bytecode interpreter
test/            the test suites (see the user manual, section 8)
bench/           benchmarks and their harness (see docs/perf)
docs/            this documentation
```

## Where it's heading

Today riscvsim simulates one processor in user mode. The
[roadmap](../ROADMAP.md) adds, in order: privilege levels and traps,
virtual memory, timers and devices, firmware, and finally a Linux boot.
The architecture is ready for that: `Os` becomes one environment among
others, and `Memory` will sit behind a bus that devices can attach to.
