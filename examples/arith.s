# arith.s: 64-bit integer arithmetic, 64-bit li constants, and the
# sign-extending *W instructions of RV64.

    .data
word:   .word 0xffffffff

    .text
_start:
    li    s0, 0x123456789abcdef0
    li    s1, -42

    mv    a0, s0                 # 1311768467463790320
    call  print_line
    add   a0, s0, s1             # 1311768467463790278
    call  print_line
    li    t0, 1000
    mul   a0, s1, t0             # -42000
    call  print_line
    li    t0, 0x7fffffff
    addiw a0, t0, 1              # wraps to -2147483648 (sign-extended)
    call  print_line
    srli  a0, s1, 60             # logical: 15
    call  print_line
    srai  a0, s1, 1              # arithmetic: -21
    call  print_line
    slt   a0, s1, s0             # signed -42 < big: 1
    call  print_line
    sltu  a0, s1, s0             # unsigned 2^64-42 < big: 0
    call  print_line
    li    t0, 0xff00
    xori  a0, t0, 0x0ff          # 65535
    call  print_line
    la    t0, word
    lw    a0, 0(t0)              # sign-extends: -1
    call  print_line
    lwu   a0, 0(t0)              # zero-extends: 4294967295
    call  print_line
    li    a0, 0x8000000000000000 # INT64_MIN
    call  print_line
    li    t0, 3
    sllw  a0, s1, t0             # (-42 << 3) as 32-bit: -336
    call  print_line
    li    t0, 1
    slli  t0, t0, 31
    srlw  a0, t0, zero           # bit 31 set: sign-extends to -2147483648
    call  print_line

    li    a7, 10                 # exit(0)
    ecall

# print a0 as a signed decimal, then a newline
print_line:
    li    a7, 1
    ecall
    li    a0, '\n'
    li    a7, 11
    ecall
    ret
