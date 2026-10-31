# Design: rvlibc, our C library

rvlibc is a small C library for programs that run on riscvsim. It lives
in `guest/libc/`: about 1,200 lines of freestanding C11 with no
dependencies.

## Why write one

glibc works on riscvsim, but it is large, and its start-up code uses
dozens of system calls. When something goes wrong, the cause could be
anywhere in it. rvlibc is small enough to read in an afternoon, so a C
program built against it exercises riscvsim's Linux emulation in a way
we fully understand. Comparing the two libraries also catches bugs that
only one of them reveals.

## What's inside

| File | Contents |
|------|----------|
| `crt0.S` | `_start`: sets the global pointer, passes the initial stack to C |
| `core.c` | process start-up and `exit`, system call wrappers, strings, `strtol`, `qsort`, `malloc` |
| `stdio.c` | buffered `FILE` streams and the `printf` family |
| `include/` | the headers: `stdio.h`, `stdlib.h`, `string.h`, `unistd.h`, ... |

System calls are made directly with the `ecall` instruction, through one
inline function that puts the number in `a7` and the arguments in
`a0`–`a5`. Each wrapper turns a negative result into `errno` and `-1`, as
C programs expect.

## Start-up

The program starts at `_start` with the stack laid out as Linux leaves
it (see [Linux emulation](linux-emulation.md)). `crt0.S` hands the stack
pointer to `__libc_start_main`, which finds `argc`, `argv` and the
environment, calls `main`, and passes its result to `exit`. `exit` runs
the `atexit` handlers and flushes all output.

## Memory allocation

`malloc` uses *size classes*: every request is rounded up to a power of
two (32 bytes to 128 KiB), and each class keeps a list of free blocks.
Freeing a block puts it back on its list. New blocks come from the heap,
which grows with `brk`. Requests above 128 KiB get their own `mmap`
region, returned to the system on `free`. A 16-byte header in front of
each block records its class.

It's simple and fast, at the cost of some wasted space: a 33-byte request
uses a 64-byte block.

## Exact `printf`

Printing floating-point numbers correctly is famously hard. `printf("%.3f",
x)` must print the decimal value of `x` rounded to 3 digits, as if
computed exactly.

rvlibc does it the straightforward way. A double is a whole number times
a power of two, so its decimal expansion is finite (up to about 1,100
digits). A small arbitrary-precision integer type (*bignum*) produces
that full expansion. The digit string is then rounded half-to-even and
formatted as `%f`, `%e` or `%g`. The output matches glibc digit for
digit, including subnormals and `%.60e`.

## Building a program with rvlibc

```
GCC_INC=$(riscv64-suse-linux-gcc -print-file-name=include)
riscv64-suse-linux-gcc -O2 -static -nostdlib -nostdinc -ffreestanding \
    -fno-tree-loop-distribute-patterns \
    -isystem "$GCC_INC" -isystem guest/libc/include \
    guest/libc/crt0.S guest/libc/core.c guest/libc/stdio.c \
    myprog.c -lgcc -o myprog
```

`-nostdinc` keeps glibc's headers out, so only rvlibc's headers and the
compiler's own (`stddef.h`, `stdint.h`, `stdarg.h`) are used.
`-fno-tree-loop-distribute-patterns` stops GCC from turning the loop
inside `memset` into a call to `memset` itself.

## Limits

No threads, no locales, no `libm` (math functions), no wide characters,
and a subset of stdio (no `scanf`). It's enough for the test programs and
benchmarks, and it grows when a program needs more.

## How it's tested

Every program in `guest/tests/` is built against rvlibc and against
glibc, both are run on riscvsim, and their output must match a native
build of the same program on the host. See [testing](testing.md).
