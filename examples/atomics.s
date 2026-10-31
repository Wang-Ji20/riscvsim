# atomics.s: the A extension. AMOs return the old value, LR/SC builds a
# compare-and-swap loop, and the .w forms sign-extend.

    .data
    .align 3
counter: .dword 0
maxval:  .dword 10
word:    .word 0xfffffffe

    .text
_start:
    la    s0, counter

    # five atomic increments by 5; amoadd returns the old value
    li    t0, 5
    li    t1, 5
1:  amoadd.d.aqrl zero, t1, (s0)
    addi  t0, t0, -1
    bnez  t0, 1b
    ld    a0, 0(s0)             # 25
    call  print_line

    li    t1, 100
    amoswap.d a0, t1, (s0)      # old value 25; counter = 100
    call  print_line

    # atomic increment with LR/SC: retry until sc succeeds (a0 = 0)
cas:
    lr.d  t0, (s0)
    addi  t0, t0, 1
    sc.d  t1, t0, (s0)
    bnez  t1, cas
    ld    a0, 0(s0)             # 101
    call  print_line

    # sc without a reservation fails (writes 1) and leaves memory alone
    li    t0, 7
    sc.d  a0, t0, (s0)          # 1
    call  print_line

    la    s1, maxval
    li    t0, 3
    amomax.d a0, t0, (s1)       # old 10; max(10, 3) = 10
    li    t0, 42
    amomax.d a0, t0, (s1)       # old 10; now 42
    ld    a0, 0(s1)             # 42
    call  print_line
    li    t0, -1
    amominu.d zero, t0, (s1)    # unsigned min(42, 2^64-1) = 42
    ld    a0, 0(s1)
    call  print_line

    la    s2, word
    lr.w  a0, (s2)              # 0xfffffffe sign-extends to -2
    call  print_line
    li    t0, 3
    amoadd.w a0, t0, (s2)       # returns -2; word = 1
    lw    a0, 0(s2)             # 1
    call  print_line
    li    t0, -5
    amomin.w a0, t0, (s2)       # signed 32-bit min(1, -5): old 1
    lw    a0, 0(s2)             # -5
    call  print_line

    li    a7, 10
    ecall

print_line:
    li    a7, 1
    ecall
    li    a0, '\n'
    li    a7, 11
    ecall
    ret
