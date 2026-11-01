# fp.s: double-precision arithmetic: convert, fused multiply-add, divide,
# square root. Measures the floating-point unit (Fpu).

    .section .rodata
    .align 3
one:   .double 1.0
half:  .double 0.5

    .text
_start:
    li    s0, 2000000           # i, counting down
    fld   fs0, one, t0
    fld   fs1, half, t0
    fmv.d.x fs2, zero           # s1 = 0
    fmv.d.x fs3, zero           # s2 = 0
loop:
    fcvt.d.l ft0, s0            # i
    fmadd.d ft1, ft0, fs1, fs0  # y = i * 0.5 + 1
    fdiv.d ft2, fs0, ft1        # 1 / y
    fsqrt.d ft3, ft1            # sqrt y
    fmadd.d fs2, ft2, ft3, fs2  # s1 += sqrt y / y
    fmul.d ft4, ft2, ft2
    fadd.d fs3, fs3, ft4        # s2 += 1 / y^2
    addi  s0, s0, -1
    bnez  s0, loop
    fmv.x.d a0, fs2
    li    a7, 34                # print_hex (exact bits)
    ecall
    li    a0, ' '
    li    a7, 11
    ecall
    fmv.x.d a0, fs3
    li    a7, 34
    ecall
    li    a0, '\n'
    li    a7, 11
    ecall
    li    a7, 10
    ecall
