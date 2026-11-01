# memory.s: streams over a 64 KiB array of doublewords, 300 times.
# Measures loads and stores that hit a small, hot set of pages.

    .equ  N, 8192               # doublewords in the array
    .equ  PASSES, 300

    .bss
    .align 3
array: .space 65536          # N * 8 bytes

    .text
_start:
    la    s0, array
    li    t0, 0                 # a[i] = i
    li    t1, N
    mv    t2, s0
1:  sd    t0, 0(t2)
    addi  t2, t2, 8
    addi  t0, t0, 1
    bne   t0, t1, 1b

    li    s1, 0                 # pass
    li    s2, 0                 # sum
    li    s3, PASSES
    li    t3, 65536             # N * 8
    add   s4, s0, t3            # end of array
pass:
    mv    t2, s0
2:  ld    t0, 0(t2)             # a[i] = 3 * a[i] + pass
    sh1add t0, t0, t0
    add   t0, t0, s1
    sd    t0, 0(t2)
    add   s2, s2, t0
    addi  t2, t2, 8
    bne   t2, s4, 2b
    addi  s1, s1, 1
    bne   s1, s3, pass

    mv    a0, s2
    li    a7, 34                # print_hex
    ecall
    li    a0, '\n'
    li    a7, 11
    ecall
    li    a7, 10
    ecall
