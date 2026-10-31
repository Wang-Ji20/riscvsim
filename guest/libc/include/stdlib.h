#ifndef _STDLIB_H
#define _STDLIB_H
#include <stddef.h>
#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1
void *malloc(size_t n);
void *calloc(size_t m, size_t n);
void *realloc(void *p, size_t n);
void free(void *p);
_Noreturn void exit(int status);
_Noreturn void abort(void);
int atexit(void (*f)(void));
int atoi(const char *s);
long strtol(const char *restrict s, char **restrict end, int base);
unsigned long strtoul(const char *restrict s, char **restrict end, int base);
char *getenv(const char *name);
int abs(int x);
long labs(long x);
void qsort(void *base, size_t n, size_t size, int (*cmp)(const void *, const void *));
#endif
