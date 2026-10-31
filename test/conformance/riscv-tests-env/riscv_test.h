/* riscv-tests environment for riscvsim: plain Linux user mode.

   The stock "p" environment boots in machine mode, installs a trap
   handler and reports through HTIF. Here a test is an ordinary process:
   it starts at _start in user mode with the FPU usable, and reports its
   result through exit(2). Status 0 means pass; otherwise the status is
   the number of the failing test case (TESTNUM). */

#ifndef RISCVSIM_RISCV_TEST_H
#define RISCVSIM_RISCV_TEST_H

#define RVTEST_RV64U .macro init; .endm
#define RVTEST_RV64UF .macro init; .endm
#define RVTEST_RV32U .macro init; .endm
#define RVTEST_RV32UF .macro init; .endm

#define TESTNUM gp

#define RVTEST_CODE_BEGIN \
    .text;                \
    .globl _start;        \
_start:                   \
    init;

#define RVTEST_CODE_END unimp

#define RVTEST_PASS   \
    li a0, 0;         \
    li a7, 93;        \
    ecall

#define RVTEST_FAIL   \
    mv a0, TESTNUM;   \
    li a7, 93;        \
    ecall

#define RVTEST_DATA_BEGIN .data; .align 4;
#define RVTEST_DATA_END

#endif
