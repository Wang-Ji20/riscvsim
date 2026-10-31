/* rvlibc core: process startup, system call wrappers, strings, and a
   small malloc. Freestanding: depends only on compiler-provided headers. */

#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/syscall_arch.h>
#include <time.h>
#include <unistd.h>

int errno;
static char **environ_;

/* ---------------------------------------------------------------- */
/* Startup                                                           */

int main(int argc, char **argv, char **envp);
void __stdio_flush_all(void);

#define MAX_ATEXIT 32
static void (*atexit_fns[MAX_ATEXIT])(void);
static int atexit_count;

int atexit(void (*f)(void))
{
    if (atexit_count == MAX_ATEXIT) return -1;
    atexit_fns[atexit_count++] = f;
    return 0;
}

_Noreturn void _exit(int status)
{
    for (;;) __syscall1(SYS_exit_group, status);
}

_Noreturn void exit(int status)
{
    while (atexit_count > 0) atexit_fns[--atexit_count]();
    __stdio_flush_all();
    _exit(status);
}

_Noreturn void abort(void)
{
    static const char msg[] = "abort()\n";
    __syscall3(SYS_write, 2, msg, sizeof msg - 1);
    _exit(134);
}

/* Called from crt0 with a pointer to argc on the initial stack. */
_Noreturn void __libc_start_main(long *sp)
{
    int argc = (int)sp[0];
    char **argv = (char **)(sp + 1);
    char **envp = argv + argc + 1;
    environ_ = envp;
    exit(main(argc, argv, envp));
}

char *getenv(const char *name)
{
    size_t n = strlen(name);
    for (char **e = environ_; e && *e; e++)
        if (strncmp(*e, name, n) == 0 && (*e)[n] == '=') return *e + n + 1;
    return NULL;
}

/* ---------------------------------------------------------------- */
/* System call wrappers: map -errno results onto errno.             */

static long ret(long r)
{
    if (r < 0 && r > -4096) {
        errno = (int)-r;
        return -1;
    }
    return r;
}

#define AT_FDCWD (-100)

ssize_t read(int fd, void *buf, size_t n) { return ret(__syscall3(SYS_read, fd, buf, n)); }
ssize_t write(int fd, const void *buf, size_t n) { return ret(__syscall3(SYS_write, fd, buf, n)); }
int close(int fd) { return (int)ret(__syscall1(SYS_close, fd)); }
off_t lseek(int fd, off_t off, int whence) { return ret(__syscall3(SYS_lseek, fd, off, whence)); }
int unlink(const char *path) { return (int)ret(__syscall3(SYS_unlinkat, AT_FDCWD, path, 0)); }
int getpid(void) { return (int)__syscall0(SYS_getpid); }

int open(const char *path, int flags, ...)
{
    va_list ap;
    va_start(ap, flags);
    int mode = (flags & O_CREAT) ? va_arg(ap, int) : 0;
    va_end(ap);
    return (int)ret(__syscall6(SYS_openat, AT_FDCWD, (long)path, flags, mode, 0, 0));
}

char *getcwd(char *buf, size_t size)
{
    return ret(__syscall2(SYS_getcwd, buf, size)) < 0 ? NULL : buf;
}

int clock_gettime(int clk, struct timespec *ts)
{
    return (int)ret(__syscall2(SYS_clock_gettime, clk, ts));
}

time_t time(time_t *t)
{
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    if (t) *t = ts.tv_sec;
    return ts.tv_sec;
}

/* ---------------------------------------------------------------- */
/* Strings and memory                                                */

void *memcpy(void *restrict d, const void *restrict s, size_t n)
{
    unsigned char *dp = d;
    const unsigned char *sp = s;
    while (n--) *dp++ = *sp++;
    return d;
}

void *memmove(void *d, const void *s, size_t n)
{
    unsigned char *dp = d;
    const unsigned char *sp = s;
    if (dp < sp)
        while (n--) *dp++ = *sp++;
    else
        while (n--) dp[n] = sp[n];
    return d;
}

void *memset(void *d, int c, size_t n)
{
    unsigned char *p = d;
    while (n--) *p++ = (unsigned char)c;
    return d;
}

int memcmp(const void *a, const void *b, size_t n)
{
    const unsigned char *x = a, *y = b;
    for (; n; n--, x++, y++)
        if (*x != *y) return *x - *y;
    return 0;
}

void *memchr(const void *s, int c, size_t n)
{
    const unsigned char *p = s;
    for (; n; n--, p++)
        if (*p == (unsigned char)c) return (void *)p;
    return NULL;
}

size_t strlen(const char *s)
{
    const char *p = s;
    while (*p) p++;
    return (size_t)(p - s);
}

int strcmp(const char *a, const char *b)
{
    while (*a && *a == *b) a++, b++;
    return (unsigned char)*a - (unsigned char)*b;
}

int strncmp(const char *a, const char *b, size_t n)
{
    for (; n; n--, a++, b++) {
        if (*a != *b) return (unsigned char)*a - (unsigned char)*b;
        if (!*a) return 0;
    }
    return 0;
}

char *strcpy(char *restrict d, const char *restrict s)
{
    char *r = d;
    while ((*d++ = *s++)) {}
    return r;
}

char *strncpy(char *restrict d, const char *restrict s, size_t n)
{
    size_t i = 0;
    for (; i < n && s[i]; i++) d[i] = s[i];
    for (; i < n; i++) d[i] = 0;
    return d;
}

char *strcat(char *restrict d, const char *restrict s)
{
    strcpy(d + strlen(d), s);
    return d;
}

char *strchr(const char *s, int c)
{
    for (;; s++) {
        if (*s == (char)c) return (char *)s;
        if (!*s) return NULL;
    }
}

char *strrchr(const char *s, int c)
{
    const char *last = NULL;
    for (;; s++) {
        if (*s == (char)c) last = s;
        if (!*s) return (char *)last;
    }
}

char *strstr(const char *h, const char *n)
{
    size_t len = strlen(n);
    for (; *h; h++)
        if (strncmp(h, n, len) == 0) return (char *)h;
    return len == 0 ? (char *)h : NULL;
}

char *strdup(const char *s)
{
    size_t n = strlen(s) + 1;
    char *d = malloc(n);
    return d ? memcpy(d, s, n) : NULL;
}

/* ---------------------------------------------------------------- */
/* Numbers                                                           */

int abs(int x) { return x < 0 ? -x : x; }
long labs(long x) { return x < 0 ? -x : x; }

static int digit_value(char c)
{
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'z') return c - 'a' + 10;
    if (c >= 'A' && c <= 'Z') return c - 'A' + 10;
    return 99;
}

unsigned long strtoul(const char *restrict s, char **restrict end, int base)
{
    while (*s == ' ' || *s == '\t' || *s == '\n') s++;
    int neg = 0;
    if (*s == '+' || *s == '-') neg = *s++ == '-';
    if ((base == 0 || base == 16) && s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
        s += 2;
        base = 16;
    } else if (base == 0)
        base = s[0] == '0' ? 8 : 10;
    unsigned long v = 0;
    for (int d; (d = digit_value(*s)) < base; s++) v = v * (unsigned long)base + (unsigned long)d;
    if (end) *end = (char *)s;
    return neg ? -v : v;
}

long strtol(const char *restrict s, char **restrict end, int base)
{
    return (long)strtoul(s, end, base);
}

int atoi(const char *s) { return (int)strtol(s, NULL, 10); }

/* Insertion sort for small inputs, otherwise a simple quicksort. */
static void swap_bytes(unsigned char *a, unsigned char *b, size_t size)
{
    while (size--) {
        unsigned char t = *a;
        *a++ = *b;
        *b++ = t;
    }
}

void qsort(void *base, size_t n, size_t size, int (*cmp)(const void *, const void *))
{
    unsigned char *a = base;
    while (n > 8) {
        swap_bytes(a, a + (n / 2) * size, size); /* middle element as pivot */
        size_t last = 0;
        for (size_t i = 1; i < n; i++)
            if (cmp(a + i * size, a) < 0) swap_bytes(a + ++last * size, a + i * size, size);
        swap_bytes(a, a + last * size, size);
        qsort(a, last, size, cmp); /* recurse on the left, loop on the right */
        a += (last + 1) * size;
        n -= last + 1;
    }
    for (size_t i = 1; i < n; i++)
        for (size_t j = i; j > 0 && cmp(a + (j - 1) * size, a + j * size) > 0; j--)
            swap_bytes(a + (j - 1) * size, a + j * size, size);
}

/* ---------------------------------------------------------------- */
/* malloc: segregated free lists of power-of-two size classes, carved
   from brk. Large blocks (> 128 KiB) get their own mmap. Every block has
   a 16-byte header holding its class (or mapping size), so free() knows
   where it goes. */

#define HDR 16
#define NCLASSES 14 /* 32 B .. 128 KiB */
#define LARGE (1UL << 17)

struct block { struct block *next; };
static struct block *free_lists[NCLASSES];
static char *heap_end, *heap_top;

static void *sbrk_(size_t n)
{
    if (!heap_end) heap_end = heap_top = (char *)__syscall1(SYS_brk, 0);
    if (heap_top + n > heap_end) {
        size_t grow = (n + 0xffff) & ~0xffffUL;
        char *want = heap_end + grow;
        if ((char *)__syscall1(SYS_brk, want) != want) return NULL;
        heap_end = want;
    }
    void *p = heap_top;
    heap_top += n;
    return p;
}

static int class_of(size_t n)
{
    int c = 0;
    size_t sz = 32;
    while (sz < n + HDR) sz <<= 1, c++;
    return c;
}

void *malloc(size_t n)
{
    if (n + HDR > LARGE) {
        size_t len = (n + HDR + 4095) & ~4095UL;
        long p = __syscall6(SYS_mmap, 0, (long)len, 3 /* RW */, 0x22 /* PRIVATE|ANON */, -1, 0);
        if (p < 0 && p > -4096) return NULL;
        *(size_t *)p = len;
        ((size_t *)p)[1] = (size_t)-1; /* marks an mmap'ed block */
        return (char *)p + HDR;
    }
    int c = class_of(n);
    struct block *b = free_lists[c];
    if (b) {
        free_lists[c] = b->next;
    } else {
        b = sbrk_((size_t)32 << c);
        if (!b) {
            errno = ENOMEM;
            return NULL;
        }
    }
    ((size_t *)b)[0] = (size_t)c;
    ((size_t *)b)[1] = 0;
    return (char *)b + HDR;
}

void free(void *p)
{
    if (!p) return;
    size_t *h = (size_t *)((char *)p - HDR);
    if (h[1] == (size_t)-1) {
        __syscall2(SYS_munmap, h, h[0]);
        return;
    }
    struct block *b = (struct block *)h;
    b->next = free_lists[h[0]];
    free_lists[h[0]] = b;
}

static size_t usable(void *p)
{
    size_t *h = (size_t *)((char *)p - HDR);
    return h[1] == (size_t)-1 ? h[0] - HDR : ((size_t)32 << h[0]) - HDR;
}

void *calloc(size_t m, size_t n)
{
    if (n && m > (size_t)-1 / n) return NULL;
    void *p = malloc(m * n);
    return p ? memset(p, 0, m * n) : NULL;
}

void *realloc(void *p, size_t n)
{
    if (!p) return malloc(n);
    if (n <= usable(p)) return p;
    void *q = malloc(n);
    if (q) {
        memcpy(q, p, usable(p));
        free(p);
    }
    return q;
}
