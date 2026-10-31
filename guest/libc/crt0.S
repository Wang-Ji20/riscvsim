/* Process entry point. The kernel (here: riscvsim's Os module) leaves
   sp -> argc, argv..., NULL, envp..., NULL, auxv... */
    .section .text._start
    .globl _start
_start:
    .option push
    .option norelax
    la      gp, __global_pointer$
    .option pop
    mv      a0, sp              /* pointer to argc */
    andi    sp, sp, -16
    call    __libc_start_main   /* never returns */
1:  j       1b
