# syscalls.s: 300,000 getpid system calls.
# Measures the cost of entering the Linux emulation (Os).

    .text
_start:
    li    s0, 300000
    li    s1, 0
loop:
    li    a7, 172               # getpid
    ecall
    add   s1, s1, a0
    addi  s0, s0, -1
    bnez  s0, loop
    mv    a0, s1
    li    a7, 1                 # print_int
    ecall
    li    a0, '\n'
    li    a7, 11
    ecall
    li    a7, 10
    ecall
