# sieve.s: Sieve of Eratosthenes over a byte array in .bss.
# Prints the primes below 100 and how many there are.

    .equ LIMIT, 100

    .bss
composite: .space LIMIT          # zero-initialised: 0 = maybe prime

    .text
_start:
    la    s0, composite
    li    s1, LIMIT
    li    t0, 2                  # p
mark_outer:
    mul   t1, t0, t0             # start at p*p
    bge   t1, s1, collect
    add   t2, s0, t0
    lbu   t3, 0(t2)
    bnez  t3, next_p             # p already composite
    li    t4, 1
mark:
    bge   t1, s1, next_p
    add   t2, s0, t1
    sb    t4, 0(t2)
    add   t1, t1, t0
    j     mark
next_p:
    addi  t0, t0, 1
    j     mark_outer

collect:
    li    s2, 0                  # count
    li    t0, 2
scan:
    bge   t0, s1, report
    add   t2, s0, t0
    lbu   t3, 0(t2)
    bnez  t3, 1f
    addi  s2, s2, 1
    mv    a0, t0
    li    a7, 1
    ecall
    li    a0, ' '
    li    a7, 11
    ecall
1:  addi  t0, t0, 1
    j     scan

report:
    li    a0, '\n'
    li    a7, 11
    ecall
    la    a0, count_msg
    li    a7, 4                  # print_string
    ecall
    mv    a0, s2
    li    a7, 1
    ecall
    li    a0, '\n'
    li    a7, 11
    ecall
    li    a7, 10
    ecall

    .section .rodata
count_msg: .asciz "count: "
