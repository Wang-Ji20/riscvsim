# int_loop.s: a tight integer loop (xorshift64 random numbers, summed).
# Measures the cost of plain ALU instructions and a backward branch.

    .text
_start:
    li    s0, 2500000           # iterations
    li    s1, 0x9E3779B97F4A7C15 # xorshift state
    li    s2, 0                 # sum
loop:
    slli  t0, s1, 13
    xor   s1, s1, t0
    srli  t0, s1, 7
    xor   s1, s1, t0
    slli  t0, s1, 17
    xor   s1, s1, t0
    add   s2, s2, s1
    addi  s0, s0, -1
    bnez  s0, loop
    mv    a0, s2
    li    a7, 34                # print_hex
    ecall
    li    a0, '\n'
    li    a7, 11
    ecall
    li    a7, 10
    ecall
