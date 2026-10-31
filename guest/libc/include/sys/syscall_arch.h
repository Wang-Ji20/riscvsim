/* Raw RV64 Linux system calls: number in a7, arguments in a0..a5,
   result (or -errno) in a0. */
#ifndef _SYS_SYSCALL_ARCH_H
#define _SYS_SYSCALL_ARCH_H

static inline long __syscall6(long n, long a, long b, long c, long d, long e, long f)
{
    register long a7 __asm__("a7") = n;
    register long a0 __asm__("a0") = a;
    register long a1 __asm__("a1") = b;
    register long a2 __asm__("a2") = c;
    register long a3 __asm__("a3") = d;
    register long a4 __asm__("a4") = e;
    register long a5 __asm__("a5") = f;
    __asm__ volatile("ecall"
                     : "+r"(a0)
                     : "r"(a7), "r"(a1), "r"(a2), "r"(a3), "r"(a4), "r"(a5)
                     : "memory");
    return a0;
}

#define __syscall0(n) __syscall6(n, 0, 0, 0, 0, 0, 0)
#define __syscall1(n, a) __syscall6(n, (long)(a), 0, 0, 0, 0, 0)
#define __syscall2(n, a, b) __syscall6(n, (long)(a), (long)(b), 0, 0, 0, 0)
#define __syscall3(n, a, b, c) __syscall6(n, (long)(a), (long)(b), (long)(c), 0, 0, 0)

enum {
    SYS_getcwd = 17, SYS_openat = 56, SYS_close = 57, SYS_lseek = 62,
    SYS_read = 63, SYS_write = 64, SYS_unlinkat = 35, SYS_exit_group = 94,
    SYS_getpid = 172, SYS_brk = 214, SYS_munmap = 215, SYS_mmap = 222,
    SYS_clock_gettime = 113,
};

#endif
