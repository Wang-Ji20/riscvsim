# factorial.s: recursive factorial with real stack frames (sd/ld of ra and
# saved registers). Prints n! for n = 0..20; 20! is the largest that fits
# in a signed 64-bit integer.

    .text
_start:
    li    s0, 0
    li    s1, 21
1:  mv    a0, s0
    call  fact
    li    a7, 1
    ecall
    li    a0, '\n'
    li    a7, 11
    ecall
    addi  s0, s0, 1
    blt   s0, s1, 1b
    li    a0, 0
    li    a7, 93
    ecall

# a0 = a0!   (recursive: n! = n * (n-1)!)
fact:
    addi  sp, sp, -16
    sd    ra, 8(sp)
    sd    s0, 0(sp)
    mv    s0, a0
    li    a0, 1
    blez  s0, 2f            # 0! = 1
    addi  a0, s0, -1
    call  fact
    mul   a0, a0, s0
2:  ld    s0, 0(sp)
    ld    ra, 8(sp)
    addi  sp, sp, 16
    ret
