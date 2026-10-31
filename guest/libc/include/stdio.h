#ifndef _STDIO_H
#define _STDIO_H
#include <stddef.h>
#include <stdarg.h>
#define EOF (-1)
#define BUFSIZ 1024
typedef struct FILE FILE;
extern FILE *const stdin, *const stdout, *const stderr;
FILE *fopen(const char *restrict path, const char *restrict mode);
int fclose(FILE *f);
int fflush(FILE *f);
int fputc(int c, FILE *f);
int putc(int c, FILE *f);
int putchar(int c);
int fputs(const char *restrict s, FILE *restrict f);
int puts(const char *s);
int fgetc(FILE *f);
int getc(FILE *f);
int getchar(void);
char *fgets(char *restrict s, int n, FILE *restrict f);
size_t fread(void *restrict p, size_t size, size_t n, FILE *restrict f);
size_t fwrite(const void *restrict p, size_t size, size_t n, FILE *restrict f);
int printf(const char *restrict fmt, ...);
int fprintf(FILE *restrict f, const char *restrict fmt, ...);
int sprintf(char *restrict s, const char *restrict fmt, ...);
int snprintf(char *restrict s, size_t n, const char *restrict fmt, ...);
int vfprintf(FILE *restrict f, const char *restrict fmt, va_list ap);
int vsnprintf(char *restrict s, size_t n, const char *restrict fmt, va_list ap);
#endif
