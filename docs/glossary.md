# Glossary

Plain-language definitions of the terms used across the documentation.

**ABI (Application Binary Interface).** The conventions compiled programs
follow so that they can work together: which register holds a function's
first argument, how the stack is laid out, how to ask the operating
system for something. riscvsim follows the standard RISC-V Linux ABI.

**Assembler.** A program that turns human-readable instructions such as
`add a0, a1, a2` into the numbers (machine code) a processor executes.
riscvsim includes its own.

**Assembly language.** The human-readable form of machine instructions,
with one line per instruction. Files usually end in `.s` or `.S`.

**Boxing.** Storing a value in its own small block of heap memory and
passing around a pointer to it. OCaml boxes 64-bit integers (`int64`)
whenever they are stored in an array or record field, or returned from
a function that isn't inlined. Each box costs an allocation, so
riscvsim's hot path avoids them. The opposite is *unboxed*: the value
sits directly in a machine register or in a byte buffer.

**Bytecode.** Instructions for a *virtual* machine rather than a real
processor. OCaml can compile programs to bytecode, which a program called
`ocamlrun` then interprets.

**C library (libc).** The standard set of functions every C program
relies on: `printf`, `malloc`, `fopen` and so on. glibc and musl are
well-known ones; rvlibc is riscvsim's own small one.

**CSR (Control and Status Register).** A special register for settings
and status rather than data: for example the current rounding mode, or a
counter of executed instructions.

**Differential testing.** Running the same input through two independent
implementations and checking that they agree. If they disagree, at least
one has a bug.

**ELF.** The standard file format for executable programs on Linux. It
says which bytes to load where in memory and where execution starts.

**Emulator / simulator.** A program that imitates a different computer.
riscvsim is a *functional* simulator: it gets every result right, but
doesn't model timing (how many clock cycles things take).

**Encoding.** How an instruction is represented as bits. In RISC-V most
instructions are 32 bits long; the compressed ones are 16 bits.

**Exception flags.** Five sticky bits the processor sets when a
floating-point operation is unusual: invalid (NV), divide by zero (DZ),
overflow (OF), underflow (UF), inexact (NX).

**Extension.** An optional part of the RISC-V instruction set. Letters
name them: M (multiply/divide), A (atomics), F and D (single- and
double-precision floating point), C (compressed instructions). "G" is
shorthand for IMAFD plus two small extensions (Zicsr, Zifencei). The "Zb"
extensions (Zba, Zbb, Zbs) add bit-manipulation instructions.

**Floating point (IEEE 754).** The standard way computers store real
numbers approximately, as a sign, an exponent and a fraction. *binary32*
(single precision) has 24 bits of precision and *binary64* (double) has 53.

**fuel.** A limit on how many instructions riscvsim will run before it
stops, so that a looping program can't hang a test.

**Access fault.** A trap raised when a program reads, writes or runs
code at an address where there is neither RAM nor a device.

**Bus.** The path between a processor and memory and devices. riscvsim's
`Bus` decides, for each address, whether it leads to RAM, to a device, or
to nothing (an access fault).

**Device (memory-mapped).** Hardware such as a serial port or a timer
that a program controls by reading and writing special addresses, as
if they were memory.

**Hart (hardware thread).** RISC-V's word for one processor: a set of
registers and a program counter executing one instruction stream. A
multi-core chip has several harts. riscvsim's `Hart` module simulates one.

**HTIF (Host-Target Interface).** A simple convention used by RISC-V test
programs: writing a value to a memory location called `tohost` asks the
simulator to stop.

**Inlining.** A compiler optimization that copies a small function's
body into the place it's called, instead of calling it. Besides saving
the call, it lets the compiler keep values unboxed across what used to
be a function boundary.

**Instruction set architecture (ISA).** The contract between software and
a processor: the list of instructions, what each does, and how each is
encoded. RISC-V is an ISA; specific chips implement it.

**MIPS (millions of instructions per second).** How fast a simulator is:
how many simulated instructions it runs per second of real time.

**NaN (Not a Number).** A special floating-point value meaning "no
meaningful result", produced by operations such as 0/0.

**NaN-boxing.** RISC-V keeps a 32-bit float in a 64-bit register by
setting the upper 32 bits to all ones. A value without that pattern
counts as NaN.

**Oracle.** In testing, a trusted source of correct answers to compare
against: another simulator, a reference file, or the real hardware.

**Page.** A fixed-size block of memory (4 KiB here). riscvsim allocates
simulated memory one page at a time, only when it's used.

**Property-based testing.** Instead of writing individual test cases, you
state a rule that must always hold (for example "decoding an encoded
instruction gives back the original") and the test library checks it on
thousands of random inputs. riscvsim uses the QCheck library.

**Pseudo-instruction.** An assembly-language convenience that the
assembler expands into one or more real instructions. `li a0, 5` (load
immediate) is one.

**Register.** A small, fast storage slot inside the processor. RISC-V has
32 integer registers (x0–x31, x0 always reads as zero) and 32
floating-point registers (f0–f31), each 64 bits wide here.

**RISC-V.** An open, royalty-free instruction set architecture. **RV64**
means its 64-bit variant.

**Rounding mode.** How a floating-point result that can't be represented
exactly is rounded: to nearest (ties to even, RNE), toward zero (RTZ),
down (RDN), up (RUP), or to nearest with ties away from zero (RMM).

**RVC.** The RISC-V compressed extension ("C"): 16-bit versions of common
instructions that make programs about a quarter smaller.

**Signature.** In compliance tests, a block of memory where a test writes
its results. Comparing it with a reference signature shows whether the
implementation is correct.

**Spike.** The official RISC-V reference simulator, used as an oracle.

**Subnormal number.** A floating-point number so close to zero that it
has fewer bits of precision than usual. Subnormals are a common source of
subtle bugs.

**Syscall (system call).** A request from a program to the operating
system, such as "write these bytes to a file". On RISC-V a program makes
one with the `ecall` instruction.

**ulp (unit in the last place).** The gap between a floating-point number
and the next one. "Off by one ulp" means the result is the neighbouring
representable number.

**Trap.** An event that stops an instruction from completing normally
and hands control elsewhere: an illegal instruction, a misaligned atomic
access, `ebreak`, `ecall`, and later page faults. Each kind has a number,
the *cause*, and a value, *tval*, that says more (the bad address, or the
bits of the illegal instruction). A trapping instruction changes nothing:
registers, memory and the program counter are as they were before it.

**User mode.** The restricted mode ordinary programs run in. riscvsim
currently simulates only user mode and handles the operating system's
part (syscalls) itself, as `qemu-user` does.
