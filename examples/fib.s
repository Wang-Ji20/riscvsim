# fib.s: iterative Fibonacci. fib(90) = 2880067194370816120 needs all 64 bits.
# Prints fib(n) for n = 0, 10, 20, ..., 90.

    .text
_start:
    li    s0, 0                 # n
    li    s1, 100               # limit
loop:
    mv    a0, s0
    call  fib
    mv    s2, a0
    mv    a0, s0                # "fib(n) = value"
    li    a7, 1
    ecall
    li    a0, ':'
    li    a7, 11
    ecall
    li    a0, ' '
    ecall
    mv    a0, s2
    li    a7, 1
    ecall
    li    a0, '\n'
    li    a7, 11
    ecall
    addi  s0, s0, 10
    blt   s0, s1, loop
    li    a7, 10
    ecall

# a0 = fib(a0), iteratively
fib:
    li    t0, 0                 # fib(i)
    li    t1, 1                 # fib(i+1)
1:  beqz  a0, 2f
    add   t2, t0, t1
    mv    t0, t1
    mv    t1, t2
    addi  a0, a0, -1
    j     1b
2:  mv    a0, t0
    ret
