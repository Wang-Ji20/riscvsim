# hello.s: "Hello, World" using only Linux syscalls (write, exit).
# Computes the string length at run time, like strlen().

    .section .rodata
msg:
    .asciz "Hello from RV64GC!\n"

    .text
    .globl _start
_start:
    la    a1, msg          # buffer
    mv    t0, a1
1:  lbu   t1, 0(t0)        # strlen
    beqz  t1, 2f
    addi  t0, t0, 1
    j     1b
2:  sub   a2, t0, a1       # length
    li    a0, 1            # fd = stdout
    li    a7, 64           # SYS_write
    ecall
    li    a0, 0
    li    a7, 93           # SYS_exit
    ecall
