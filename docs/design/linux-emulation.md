# Design: running Linux programs

How riscvsim runs programs compiled for RISC-V Linux without running
Linux itself. Module: `Os`.

## User-mode emulation

A program doesn't talk to hardware directly. It asks the operating
system to do things: open a file, write to the screen, get memory. Each
request is a *system call*. riscvsim simulates the processor running the
program, and when the program makes a system call, riscvsim performs it
on the real host system and hands back the answer. The program can't
tell that no Linux kernel is involved.

This is called *user-mode emulation*; `qemu-user` works the same way. It
gives us real programs (C with glibc, the OCaml runtime) long before the
roadmap reaches a real Linux boot.

## Starting a process

Linux starts a program with a specific memory layout, and C libraries
depend on it. `Os.boot` recreates it:

```
high addresses
   "PATH=/usr/bin"  "hello"  ...   strings: environment and arguments
   16 random bytes                 for stack-protection canaries
   auxiliary vector                (key, value) pairs: page size, where
                                   the program headers are, ...
   NULL
   envp[0] envp[1] ...             pointers to the environment strings
   NULL
   argv[0] argv[1] ...             pointers to the argument strings
   argc                         <- stack pointer at start
low addresses
```

The *auxiliary vector* matters: glibc uses it to find its own program
headers (`AT_PHDR`) and its randomness (`AT_RANDOM`). For that reason the
ELF loader records where the program headers are mapped.

## System calls

When the program executes `ecall`, register `a7` holds the call number,
`a0`–`a5` hold the arguments, and the result goes back in `a0`. Errors
are returned as negative numbers (`-2` means "no such file").

In `Os`, each system call is a small named function, and a table maps
numbers to them:

```ocaml
let linux_calls =
  [ (56, ("openat", sys_openat)); (63, ("read", sys_read));
    (64, ("write", sys_write)); (214, ("brk", sys_brk)); ... ]
```

The table makes the supported set easy to see, and it gives `--strace`
its names. Handlers report failure by raising an exception carrying the
error number, so they stay short and linear. The dispatcher turns the
exception into the negative return value.

About 60 calls are implemented, in groups:

- **Files:** `openat`, `read`, `write`, `lseek`, `close`, `fstat`,
  `newfstatat`, `getcwd`, `unlinkat`, `readlinkat`, ... Guest file
  descriptors map to host ones, so programs read and write real files.
- **Memory:** `brk` (grow the heap) and `mmap`/`munmap`/`mremap`.
  Simulated memory is sparse and starts out zero, so "allocating" mostly
  means handing out fresh addresses.
- **Process and time:** `exit`, `uname`, `clock_gettime`, `getrandom`,
  resource limits, ...
- **Answered but deliberately unsupported:** `rseq` and
  `riscv_hwprobe` return "not implemented", and glibc falls back
  gracefully.

Unknown calls return "not implemented" rather than stopping the program.

## Three details that mattered

- **Full reads.** On Linux, reading a regular file returns everything
  asked for, up to the end of the file. OCaml's `Unix.read` returns at
  most 64 KiB per call. OCaml's own bytecode loader took the short read
  as a damaged file, so `Os` loops until the request is complete.
- **No terminals.** `ioctl` always answers "not a terminal". C libraries
  then buffer output fully, as when output goes to a pipe, which makes
  test output deterministic.
- **Deterministic randomness.** `getrandom` returns a fixed
  pseudo-random sequence, so runs are reproducible.

## Bare-metal test programs (HTIF)

Some test programs, such as the ones built for Spike, have no operating
system at all. They stop by writing a value to a memory location named
`tohost`. `Os.run` checks that location every 10,000 instructions, as
Spike does, and ends the run when it's set.

## Limits

- One thread only. `clone` isn't supported yet, so programs that start
  threads (including multi-domain OCaml) don't run.
- No signals are delivered.
- Programs can read and write any host file the user can. That's the
  `qemu-user` model, convenient but not a sandbox.

These limits go away when riscvsim boots a real Linux kernel (roadmap
Phases 2–6); `Os` then becomes the "no kernel" option for quick tests.
