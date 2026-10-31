# float.s: the F and D extensions. Newton's method vs. fsqrt, fused multiply-add,
# conversions under each rounding mode, exception flags in fcsr,
# single precision, and fclass.

    .section .rodata
    .align 3
two:     .double 2.0
half:    .double 0.5
mtwo5:   .double -2.5
big:     .double 1e10
tenth:   .float 0.1
fifth:   .float 0.2

    .text
_start:
    # sqrt(2) by Newton's method: x <- (x + 2/x) / 2, six iterations
    fld   fs0, two, t0
    fld   fs1, half, t0
    fmv.d fa0, fs0              # x = 2
    li    t1, 6
1:  fdiv.d ft0, fs0, fa0
    fadd.d ft0, fa0, ft0
    fmul.d fa0, ft0, fs1
    addi  t1, t1, -1
    bnez  t1, 1b
    fmv.d fs3, fa0
    call  print_double          # 1.414213562373095
    fsqrt.d fa0, fs0            # correctly rounded sqrt
    call  print_double          # 1.4142135623730951
    feq.d a0, fs3, fa0          # not equal: 0
    call  print_int
    fmv.x.d t0, fa0             # distance in ulps = difference of bit patterns
    fmv.x.d t1, fs3
    sub   a0, t0, t1            # Newton lands 1 ulp low
    call  print_int

    # fused multiply-add: 2 * 2 + 0.5 = 4.5 (one rounding)
    fmadd.d fa0, fs0, fs0, fs1
    call  print_double

    # -2.5 to integer under each static rounding mode
    fld   fs2, mtwo5, t0
    fcvt.l.d a0, fs2, rne       # -2 (ties to even)
    call  print_int
    fcvt.l.d a0, fs2, rtz       # -2
    call  print_int
    fcvt.l.d a0, fs2, rdn       # -3
    call  print_int
    fcvt.l.d a0, fs2, rup       # -2
    call  print_int
    fcvt.l.d a0, fs2, rmm       # -3 (ties away from zero)
    call  print_int

    # out-of-range conversion saturates and raises NV (0x10)
    csrw  fflags, zero
    fld   ft2, big, t0
    fcvt.w.d a0, ft2, rtz       # 2147483647
    call  print_int
    frflags a0                  # 16
    call  print_int

    # 1 / 0 = inf and raises DZ (0x08)
    csrw  fflags, zero
    li    t0, 1
    fcvt.d.l ft3, t0
    fmv.d.x ft4, zero
    fdiv.d fa0, ft3, ft4
    call  print_double          # inf
    frflags a0                  # 8
    call  print_int

    # single precision: 0.1f + 0.2f, and 1/3 as a float
    flw   ft5, tenth, t0
    flw   ft6, fifth, t0
    fadd.s fa0, ft5, ft6
    li    a7, 2                 # print_float
    ecall
    call  newline
    li    t0, 1
    li    t1, 3
    fcvt.s.w ft7, t0
    fcvt.s.w ft8, t1
    fdiv.s fa0, ft7, ft8        # 0.33333334
    li    a7, 2
    ecall
    call  newline

    # fclass: -0.0 sets bit 3
    fneg.d ft9, ft4
    fclass.d a0, ft9            # 8
    call  print_int

    # harmonic sum 1 + 1/2 + ... + 1/10, with fcvt.d.w each step
    fmv.d.x fa0, zero
    li    t0, 1
    li    t1, 11
2:  fcvt.d.w ft0, t0
    fdiv.d ft0, ft3, ft0
    fadd.d fa0, fa0, ft0
    addi  t0, t0, 1
    blt   t0, t1, 2b
    call  print_double          # 2.9289682539682538

    li    a7, 10
    ecall

print_double:
    li    a7, 3
    ecall
    j     newline
print_int:
    li    a7, 1
    ecall
newline:
    li    a0, '\n'
    li    a7, 11
    ecall
    ret
