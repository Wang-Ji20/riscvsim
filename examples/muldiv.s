# muldiv.s: M-extension corner cases. RISC-V division never traps:
#   x / 0 = -1, x % 0 = x, INT_MIN / -1 = INT_MIN, INT_MIN % -1 = 0.
# Also the high halves of 128-bit products (mulh, mulhu, mulhsu).

    .text
_start:
    li    s0, 1234
    li    s1, 0x8000000000000000   # INT64_MIN
    li    s2, -1

    div   a0, s0, zero      # -1
    call  print_line
    rem   a0, s0, zero      # 1234
    call  print_line
    div   a0, s1, s2        # INT64_MIN (overflow)
    call  print_line
    rem   a0, s1, s2        # 0
    call  print_line
    divu  a0, s0, zero      # all ones
    call  print_hex
    li    t0, -7
    li    t1, 2
    div   a0, t0, t1        # rounds toward zero: -3
    call  print_line
    rem   a0, t0, t1        # sign follows the dividend: -1
    call  print_line
    mulh  a0, s2, t1        # high(-1 * 2) = -1
    call  print_line
    mulhu a0, s2, s2        # high((2^64-1)^2) = 0xfffffffffffffffe
    call  print_hex
    mulhsu a0, s2, t1       # high(-1 * 2u) = -1
    call  print_line
    li    t0, 0x100000000
    li    t1, 0x300000000
    mulhu a0, t0, t1        # 2^32 * 3*2^32 = 3 * 2^64: high = 3
    call  print_line
    li    t0, 0x80000000
    li    t1, -1
    divw  a0, t0, t1        # 32-bit INT_MIN / -1 = -2147483648
    call  print_line
    remw  a0, t0, t1        # 0
    call  print_line
    li    t0, 0x10000
    mulw  a0, t0, t0        # 2^32 truncated to 32 bits = 0
    call  print_line
    li    t0, -1
    li    t1, 2
    divuw a0, t0, t1        # 0xffffffff / 2 = 0x7fffffff
    call  print_line

    li    a7, 10
    ecall

print_line:
    li    a7, 1
    ecall
    j     newline
print_hex:
    li    a7, 34
    ecall
newline:
    li    a0, '\n'
    li    a7, 11
    ecall
    ret
