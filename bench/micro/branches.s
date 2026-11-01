# branches.s: a data-dependent branch on a pseudo-random bit.
# Measures conditional branches, half of them taken, and 64-bit multiply.

    .text
_start:
    li    s0, 3000000           # iterations
    li    s1, 1                 # LCG state
    li    s3, 6364136223846793005
    li    s4, 1442695040888963407
    li    s2, 0                 # how often the bit was set
    li    s5, 0                 # xor of the values when it was clear
loop:
    mul   s1, s1, s3
    add   s1, s1, s4
    srli  t0, s1, 33
    andi  t1, t0, 1
    beqz  t1, 1f
    addi  s2, s2, 1
    j     2f
1:  xor   s5, s5, t0
2:  addi  s0, s0, -1
    bnez  s0, loop
    mv    a0, s2
    li    a7, 1                 # print_int
    ecall
    li    a0, ' '
    li    a7, 11
    ecall
    mv    a0, s5
    li    a7, 34                # print_hex
    ecall
    li    a0, '\n'
    li    a7, 11
    ecall
    li    a7, 10
    ecall
