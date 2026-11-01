# calls.s: naive recursive Fibonacci, fib(29).
# Measures calls, returns and stack loads/stores.

    .text
_start:
    li    sp, 0x3fff0000 - 16   # keep the stack independent of argv/env
    li    a0, 29
    call  fib
    li    a7, 1                 # print_int
    ecall
    li    a0, '\n'
    li    a7, 11
    ecall
    li    a7, 10
    ecall

fib:                            # a0 = fib(a0)
    li    t0, 2
    blt   a0, t0, 1f
    addi  sp, sp, -16
    sd    ra, 8(sp)
    sd    s0, 0(sp)
    mv    s0, a0
    addi  a0, a0, -1
    call  fib
    mv    t1, a0
    addi  a0, s0, -2
    mv    s0, t1
    call  fib
    add   a0, a0, s0
    ld    s0, 0(sp)
    ld    ra, 8(sp)
    addi  sp, sp, 16
1:  ret
