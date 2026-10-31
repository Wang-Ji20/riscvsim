# bitmanip.s: Zba/Zbb/Zbs (mandatory in the RVA22/RVA23 profiles), plus
# reading the instret counter CSR.

    .data
    .align 3
table:  .dword 11, 22, 33, 44, 55

    .text
_start:
    li    t0, 1
    clz   a0, t0                # 63
    call  print_int
    li    t0, 0x100
    ctz   a0, t0                # 8
    call  print_int
    li    t0, 0xff00ff
    cpop  a0, t0                # 16
    call  print_int
    li    t0, 0x0102030405060708
    rev8  a0, t0                # byte swap
    call  print_hex
    li    t0, 0x0000010000100000
    orc.b a0, t0                # nonzero bytes become 0xff
    call  print_hex
    li    t0, 0x80
    sext.b a0, t0               # -128
    call  print_int
    li    t0, -1
    zext.h a0, t0               # 65535
    call  print_int
    li    t0, 0xf0
    li    t1, 0x3c
    andn  a0, t0, t1            # 0xf0 & ~0x3c = 0xc0 = 192
    call  print_int
    li    t0, -5
    li    t1, 3
    min   a0, t0, t1            # -5
    call  print_int
    minu  a0, t0, t1            # 3
    call  print_int
    li    t0, 0x8000000000000001
    rori  a0, t0, 4             # 0x1800000000000000
    call  print_hex
    li    t1, 1
    rol   a0, t0, t1            # 3
    call  print_int

    # Zba: sh3add computes &table[i] in one instruction
    la    s0, table
    li    t0, 3
    sh3add t1, t0, s0
    ld    a0, 0(t1)             # table[3] = 44
    call  print_int
    li    t0, -1                # add.uw uses only the low 32 bits
    add.uw a0, t0, zero         # 4294967295
    call  print_int

    # Zbs: single-bit operations
    li    t0, 0
    bseti a0, t0, 40            # 2^40 = 1099511627776
    call  print_int
    li    t0, 0xff
    bclri a0, t0, 0             # 254
    call  print_int
    binvi a0, t0, 8             # 511
    call  print_int
    li    t1, 7
    bext  a0, t0, t1            # bit 7 of 0xff = 1
    call  print_int

    # instret: four instructions between the reads, so the difference is 4
    rdinstret s1
    nop
    nop
    nop
    rdinstret s2
    sub   a0, s2, s1
    call  print_int

    li    a7, 10
    ecall

print_int:
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
