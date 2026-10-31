/* rvlibc stdio: buffered FILE streams and printf.

   Floating-point formatting is exact. A double is a dyadic rational, so we
   expand it to its complete decimal representation with a small bignum
   and round that digit string half-to-even. The output therefore matches
   glibc digit for digit. */

#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define signbit(x) __builtin_signbit(x)

struct FILE {
    int fd;
    int writing;     /* buffer holds pending output */
    int unbuffered;
    int eof, err;
    size_t len, pos; /* buffered bytes; read position */
    unsigned char buf[BUFSIZ];
};

static FILE std_files[3] = {
    {.fd = 0}, {.fd = 1, .writing = 1}, {.fd = 2, .writing = 1, .unbuffered = 1}};
FILE *const stdin = &std_files[0];
FILE *const stdout = &std_files[1];
FILE *const stderr = &std_files[2];

#define MAX_OPEN 16
static FILE *open_files[MAX_OPEN];

/* ---------------------------------------------------------------- */
/* Streams                                                           */

static int write_all(int fd, const unsigned char *p, size_t n)
{
    while (n) {
        ssize_t w = write(fd, p, n);
        if (w <= 0) return -1;
        p += w;
        n -= (size_t)w;
    }
    return 0;
}

int fflush(FILE *f)
{
    if (!f) {
        for (int i = 0; i < 3; i++) fflush(&std_files[i]);
        for (int i = 0; i < MAX_OPEN; i++)
            if (open_files[i]) fflush(open_files[i]);
        return 0;
    }
    if (f->writing && f->len) {
        int r = write_all(f->fd, f->buf, f->len);
        f->len = 0;
        if (r < 0) {
            f->err = 1;
            return EOF;
        }
    }
    return 0;
}

void __stdio_flush_all(void) { fflush(NULL); }

FILE *fopen(const char *restrict path, const char *restrict mode)
{
    int flags;
    switch (mode[0]) {
    case 'r': flags = strchr(mode, '+') ? O_RDWR : O_RDONLY; break;
    case 'w': flags = (strchr(mode, '+') ? O_RDWR : O_WRONLY) | O_CREAT | O_TRUNC; break;
    case 'a': flags = (strchr(mode, '+') ? O_RDWR : O_WRONLY) | O_CREAT | O_APPEND; break;
    default: errno = EINVAL; return NULL;
    }
    int slot = 0;
    while (slot < MAX_OPEN && open_files[slot]) slot++;
    if (slot == MAX_OPEN) return NULL;
    int fd = open(path, flags, 0644);
    if (fd < 0) return NULL;
    FILE *f = calloc(1, sizeof *f);
    if (!f) {
        close(fd);
        return NULL;
    }
    f->fd = fd;
    f->writing = mode[0] != 'r';
    open_files[slot] = f;
    return f;
}

int fclose(FILE *f)
{
    int r = fflush(f);
    close(f->fd);
    for (int i = 0; i < MAX_OPEN; i++)
        if (open_files[i] == f) open_files[i] = NULL;
    if (f < std_files || f >= std_files + 3) free(f);
    return r;
}

size_t fwrite(const void *restrict p, size_t size, size_t n, FILE *restrict f)
{
    const unsigned char *s = p;
    size_t total = size * n;
    if (!f->writing) {
        f->writing = 1;
        f->len = f->pos = 0;
    }
    if (f->unbuffered) {
        if (write_all(f->fd, s, total) < 0) {
            f->err = 1;
            return 0;
        }
        return n;
    }
    for (size_t i = 0; i < total; i++) {
        if (f->len == BUFSIZ && fflush(f) == EOF) return i / (size ? size : 1);
        f->buf[f->len++] = s[i];
    }
    return n;
}

int fputc(int c, FILE *f)
{
    unsigned char ch = (unsigned char)c;
    return fwrite(&ch, 1, 1, f) == 1 ? ch : EOF;
}

int putc(int c, FILE *f) { return fputc(c, f); }
int putchar(int c) { return fputc(c, stdout); }

int fputs(const char *restrict s, FILE *restrict f)
{
    size_t n = strlen(s);
    return fwrite(s, 1, n, f) == n ? 0 : EOF;
}

int puts(const char *s) { return fputs(s, stdout) == EOF || putchar('\n') == EOF ? EOF : 0; }

int fgetc(FILE *f)
{
    if (f->writing) {
        fflush(f);
        f->writing = 0;
        f->len = f->pos = 0;
    }
    if (f->pos == f->len) {
        if (f == stdin) fflush(stdout);
        ssize_t r = read(f->fd, f->buf, BUFSIZ);
        if (r <= 0) {
            if (r < 0) f->err = 1; else f->eof = 1;
            return EOF;
        }
        f->len = (size_t)r;
        f->pos = 0;
    }
    return f->buf[f->pos++];
}

int getc(FILE *f) { return fgetc(f); }
int getchar(void) { return fgetc(stdin); }

char *fgets(char *restrict s, int n, FILE *restrict f)
{
    int i = 0;
    while (i < n - 1) {
        int c = fgetc(f);
        if (c == EOF) break;
        s[i++] = (char)c;
        if (c == '\n') break;
    }
    if (i == 0) return NULL;
    s[i] = 0;
    return s;
}

size_t fread(void *restrict p, size_t size, size_t n, FILE *restrict f)
{
    unsigned char *d = p;
    size_t total = size * n, i = 0;
    for (; i < total; i++) {
        int c = fgetc(f);
        if (c == EOF) break;
        d[i] = (unsigned char)c;
    }
    return size ? i / size : 0;
}

/* ---------------------------------------------------------------- */
/* Exact decimal expansion of a double                               */

/* Little-endian base-2^32 bignum, enough for 2^1024 and for fractions
   with 1074 bits. */
#define LIMBS 40
typedef struct { uint32_t d[LIMBS]; int n; } big;

static void big_mul_small(big *b, uint32_t m, uint32_t add)
{
    uint64_t carry = add;
    for (int i = 0; i < b->n; i++) {
        uint64_t v = (uint64_t)b->d[i] * m + carry;
        b->d[i] = (uint32_t)v;
        carry = v >> 32;
    }
    if (carry) b->d[b->n++] = (uint32_t)carry;
}

static uint32_t big_div_small(big *b, uint32_t m)
{
    uint64_t rem = 0;
    for (int i = b->n - 1; i >= 0; i--) {
        uint64_t v = (rem << 32) | b->d[i];
        b->d[i] = (uint32_t)(v / m);
        rem = v % m;
    }
    while (b->n && !b->d[b->n - 1]) b->n--;
    return (uint32_t)rem;
}

static void big_shl(big *b, int k)
{
    int words = k / 32, bits = k % 32;
    for (int i = b->n - 1 + words + 1; i >= 0; i--) {
        uint64_t hi = (i - words >= 0 && i - words < b->n) ? b->d[i - words] : 0;
        uint64_t lo = (i - words - 1 >= 0 && i - words - 1 < b->n) ? b->d[i - words - 1] : 0;
        b->d[i] = bits ? (uint32_t)((hi << bits) | (lo >> (32 - bits))) : (uint32_t)hi;
    }
    b->n += words + 1;
    while (b->n && !b->d[b->n - 1]) b->n--;
}

/* Expand |x| (finite) into integer digits and fraction digits.
   Returns the digit count; *int_digits says how many precede the point. */
static int expand_double(double x, char *out, int *int_digits)
{
    union { double d; uint64_t u; } v = {.d = x};
    int exp = (int)((v.u >> 52) & 0x7ff);
    uint64_t mant = v.u & ((1ULL << 52) - 1);
    if (exp) mant |= 1ULL << 52; else exp = 1;
    int e = exp - 1075; /* value = mant * 2^e */
    big ip = {{0}, 0}, fp = {{0}, 0};
    int frac_bits = e < 0 ? -e : 0;
    /* integer part */
    if (e >= 0) {
        ip.d[0] = (uint32_t)mant; ip.d[1] = (uint32_t)(mant >> 32); ip.n = 2;
        while (ip.n && !ip.d[ip.n - 1]) ip.n--;
        big_shl(&ip, e);
    } else if (frac_bits < 64) {
        uint64_t i = mant >> frac_bits;
        ip.d[0] = (uint32_t)i; ip.d[1] = (uint32_t)(i >> 32); ip.n = 2;
        while (ip.n && !ip.d[ip.n - 1]) ip.n--;
    }
    char tmp[400];
    int nt = 0;
    do {
        uint32_t r = big_div_small(&ip, 1000000000u);
        for (int k = 0; k < 9; k++) { tmp[nt++] = (char)('0' + r % 10); r /= 10; }
    } while (ip.n);
    while (nt > 1 && tmp[nt - 1] == '0') nt--;
    int n = 0;
    for (int i = nt - 1; i >= 0; i--) out[n++] = tmp[i];
    *int_digits = n;
    /* fraction part: frac = (mant mod 2^frac_bits) / 2^frac_bits */
    if (frac_bits > 0) {
        if (frac_bits < 64) mant &= (1ULL << frac_bits) - 1;
        fp.d[0] = (uint32_t)mant; fp.d[1] = (uint32_t)(mant >> 32); fp.n = 2;
        while (fp.n && !fp.d[fp.n - 1]) fp.n--;
        while (fp.n) {
            big_mul_small(&fp, 10, 0);
            /* digit = fp >> frac_bits; then keep the low frac_bits bits */
            int w = frac_bits / 32, b = frac_bits % 32;
            uint64_t lo = w < fp.n ? fp.d[w] : 0, hi = w + 1 < fp.n ? fp.d[w + 1] : 0;
            uint32_t digit = (uint32_t)(((hi << 32) | lo) >> b);
            out[n++] = (char)('0' + digit);
            if (w < fp.n) fp.d[w] &= b ? (1u << b) - 1 : 0;
            for (int i = w + 1; i < fp.n; i++) fp.d[i] = 0;
            while (fp.n && !fp.d[fp.n - 1]) fp.n--;
        }
    }
    return n;
}

/* Round the digit string to keep `keep` digits (half-to-even on the exact
   tail). Returns 1 if rounding carried into a new leading digit. */
static int round_digits(char *d, int n, int keep)
{
    if (keep >= n) {
        for (int i = n; i < keep; i++) d[i] = '0';
        return 0;
    }
    if (keep < 0) return 0;
    int up;
    if (d[keep] > '5') up = 1;
    else if (d[keep] < '5') up = 0;
    else {
        up = 0;
        for (int i = keep + 1; i < n; i++)
            if (d[i] != '0') { up = 1; break; }
        if (!up) up = keep > 0 ? (d[keep - 1] - '0') & 1 : 0; /* tie: to even */
    }
    if (!up) return 0;
    for (int i = keep - 1; i >= 0; i--) {
        if (d[i] < '9') { d[i]++; return 0; }
        d[i] = '0';
    }
    memmove(d + 1, d, (size_t)keep);
    d[0] = '1';
    return 1;
}

/* ---------------------------------------------------------------- */
/* printf                                                            */

typedef struct { char *buf; size_t cap, len; FILE *f; } sink;

static void emit(sink *s, const char *p, size_t n)
{
    if (s->f) fwrite(p, 1, n, s->f);
    else
        for (size_t i = 0; i < n; i++, s->len++)
            if (s->len + 1 < s->cap) s->buf[s->len] = p[i];
    if (s->f) s->len += n;
}

static void pad(sink *s, char c, int n)
{
    for (; n > 0; n--) emit(s, &c, 1);
}

enum { F_LEFT = 1, F_PLUS = 2, F_SPACE = 4, F_ALT = 8, F_ZERO = 16 };

/* Emit sign/prefix + body with width and padding rules. */
static void field(sink *s, int flags, int width, const char *prefix, const char *body, size_t len)
{
    size_t plen = strlen(prefix);
    int padn = width - (int)(plen + len);
    if (!(flags & F_LEFT) && !(flags & F_ZERO)) pad(s, ' ', padn);
    emit(s, prefix, plen);
    if (!(flags & F_LEFT) && (flags & F_ZERO)) pad(s, '0', padn);
    emit(s, body, len);
    if (flags & F_LEFT) pad(s, ' ', padn);
}

static void fmt_int(sink *s, unsigned long long v, int neg, int base, int upper, int flags,
                    int width, int prec)
{
    char tmp[70];
    int n = 0;
    const char *digits = upper ? "0123456789ABCDEF" : "0123456789abcdef";
    while (v) { tmp[n++] = digits[v % (unsigned)base]; v /= (unsigned)base; }
    char body[140];
    int len = 0;
    int zeros = (prec >= 0 ? prec : 1) - n;
    if ((flags & F_ALT) && base == 8 && zeros <= 0) zeros = 1;
    for (; zeros > 0; zeros--) body[len++] = '0';
    while (n) body[len++] = tmp[--n];
    const char *prefix = neg ? "-" : (flags & F_PLUS) ? "+" : (flags & F_SPACE) ? " " : "";
    if ((flags & F_ALT) && base == 16 && len && !(len == 1 && body[0] == '0'))
        prefix = upper ? "0X" : "0x";
    if (prec >= 0) flags &= ~F_ZERO;
    field(s, flags, width, prefix, body, (size_t)len);
}

/* A finite |x| as an exact digit string: digits[0..n) with the decimal
   point after int_digits, plus the decimal exponent of the first
   significant digit. */
typedef struct {
    char digits[1500];
    int n, int_digits, lead, exp10, is_zero;
} decimal;

static void to_decimal(double magnitude, decimal *d)
{
    d->n = expand_double(magnitude, d->digits, &d->int_digits);
    d->lead = 0;
    while (d->lead < d->n - 1 && d->digits[d->lead] == '0') d->lead++;
    d->is_zero = magnitude == 0;
    d->exp10 = d->is_zero ? 0 : d->int_digits - 1 - d->lead;
}

/* %f body: rounds d in place to prec fraction digits. */
static int format_fixed(decimal *d, int prec, int alt, char *out)
{
    int len = 0;
    int carry = round_digits(d->digits, d->n, d->int_digits + prec);
    int int_len = d->int_digits + carry;
    for (int i = 0; i < int_len; i++) out[len++] = d->digits[i];
    if (int_len == 0) out[len++] = '0';
    if (prec > 0 || alt) out[len++] = '.';
    for (int i = 0; i < prec; i++) out[len++] = d->digits[int_len + i];
    return len;
}

/* %e body: rounds d in place to prec+1 significant digits. */
static int format_exponent(decimal *d, int prec, int alt, int upper, char *out)
{
    char *m = d->digits + d->lead;
    int exp10 = d->exp10 + round_digits(m, d->is_zero ? 1 : d->n - d->lead, prec + 1);
    int len = 0;
    out[len++] = m[0];
    if (prec > 0 || alt) out[len++] = '.';
    for (int i = 1; i <= prec; i++) out[len++] = m[i];
    out[len++] = upper ? 'E' : 'e';
    out[len++] = exp10 < 0 ? '-' : '+';
    int e = exp10 < 0 ? -exp10 : exp10;
    if (e >= 100) out[len++] = (char)('0' + e / 100);
    out[len++] = (char)('0' + (e / 10) % 10);
    out[len++] = (char)('0' + e % 10);
    return len;
}

/* %g picks %e or %f from the exponent the %e form would have (C11 7.21.6.1). */
static int g_uses_fixed(const decimal *d, int precision, int *fixed_prec)
{
    decimal rounded = *d;
    char *m = rounded.digits + rounded.lead;
    int x = d->is_zero ? 0 : d->exp10 + round_digits(m, d->n - d->lead, precision);
    *fixed_prec = precision - 1 - x;
    return precision > x && x >= -4;
}

/* Drop trailing fraction zeros (and a bare '.') before any exponent. */
static int strip_trailing_zeros(char *body, int len)
{
    int mantissa_end = len;
    for (int i = 0; i < len; i++)
        if (body[i] == 'e' || body[i] == 'E') { mantissa_end = i; break; }
    if (!memchr(body, '.', (size_t)mantissa_end)) return len;
    int cut = mantissa_end;
    while (body[cut - 1] == '0') cut--;
    if (body[cut - 1] == '.') cut--;
    memmove(body + cut, body + mantissa_end, (size_t)(len - mantissa_end));
    return len - (mantissa_end - cut);
}

static void fmt_float(sink *s, double x, char conv, int flags, int width, int prec)
{
    int upper = conv == 'F' || conv == 'E' || conv == 'G';
    int negative = signbit(x);
    const char *prefix = negative ? "-" : (flags & F_PLUS) ? "+" : (flags & F_SPACE) ? " " : "";
    if (x != x || x - x != 0) { /* NaN or infinity */
        const char *text = x != x ? (upper ? "NAN" : "nan") : (upper ? "INF" : "inf");
        field(s, flags & ~F_ZERO, width, prefix, text, 3);
        return;
    }
    if (prec < 0) prec = 6;
    static decimal d;
    static char body[1600];
    to_decimal(negative ? -x : x, &d);
    int alt = flags & F_ALT, len;
    switch (conv | 0x20) {
    case 'f': len = format_fixed(&d, prec, alt, body); break;
    case 'e': len = format_exponent(&d, prec, alt, upper, body); break;
    default: { /* g */
        int precision = prec == 0 ? 1 : prec, fixed_prec;
        len = g_uses_fixed(&d, precision, &fixed_prec) ? format_fixed(&d, fixed_prec, alt, body)
                                                       : format_exponent(&d, precision - 1, alt, upper, body);
        if (!alt) len = strip_trailing_zeros(body, len);
    }
    }
    field(s, flags, width, prefix, body, (size_t)len);
}

static int vformat(sink *s, const char *fmt, va_list ap)
{
    for (const char *p = fmt; *p; p++) {
        if (*p != '%') {
            const char *q = p;
            while (*q && *q != '%') q++;
            emit(s, p, (size_t)(q - p));
            p = q - 1;
            continue;
        }
        p++;
        int flags = 0, width = 0, prec = -1;
        for (;; p++) {
            if (*p == '-') flags |= F_LEFT;
            else if (*p == '+') flags |= F_PLUS;
            else if (*p == ' ') flags |= F_SPACE;
            else if (*p == '#') flags |= F_ALT;
            else if (*p == '0') flags |= F_ZERO;
            else break;
        }
        if (*p == '*') {
            width = va_arg(ap, int);
            if (width < 0) { flags |= F_LEFT; width = -width; }
            p++;
        } else
            while (*p >= '0' && *p <= '9') width = width * 10 + (*p++ - '0');
        if (*p == '.') {
            p++;
            prec = 0;
            if (*p == '*') { prec = va_arg(ap, int); p++; }
            else while (*p >= '0' && *p <= '9') prec = prec * 10 + (*p++ - '0');
        }
        if (flags & F_LEFT) flags &= ~F_ZERO;
        int lng = 0; /* 0 int, 1 long, 2 long long, -1 short, -2 char */
        for (;; p++) {
            if (*p == 'l') lng++;
            else if (*p == 'h') lng--;
            else if (*p == 'z' || *p == 'j' || *p == 't') lng = 1;
            else break;
        }
        switch (*p) {
        case 'd': case 'i': {
            long long v = lng >= 1 ? va_arg(ap, long) : va_arg(ap, int);
            if (lng == -1) v = (short)v;
            if (lng == -2) v = (signed char)v;
            unsigned long long mag = v < 0 ? 0ULL - (unsigned long long)v : (unsigned long long)v;
            fmt_int(s, mag, v < 0, 10, 0, flags, width, prec);
            break;
        }
        case 'u': case 'x': case 'X': case 'o': {
            unsigned long long v = lng >= 1 ? va_arg(ap, unsigned long) : va_arg(ap, unsigned);
            if (lng == -1) v = (unsigned short)v;
            if (lng == -2) v = (unsigned char)v;
            int base = *p == 'u' ? 10 : *p == 'o' ? 8 : 16;
            fmt_int(s, v, 0, base, *p == 'X', flags & ~(F_PLUS | F_SPACE), width, prec);
            break;
        }
        case 'p': {
            void *v = va_arg(ap, void *);
            if (!v) field(s, flags, width, "", "(nil)", 5);
            else fmt_int(s, (uintptr_t)v, 0, 16, 0, flags | F_ALT, width, prec);
            break;
        }
        case 'c': {
            char c = (char)va_arg(ap, int);
            field(s, flags & ~F_ZERO, width, "", &c, 1);
            break;
        }
        case 's': {
            const char *str = va_arg(ap, const char *);
            if (!str) str = "(null)";
            size_t len = strlen(str);
            if (prec >= 0 && (size_t)prec < len) len = (size_t)prec;
            field(s, flags & ~F_ZERO, width, "", str, len);
            break;
        }
        case 'f': case 'F': case 'e': case 'E': case 'g': case 'G':
            fmt_float(s, va_arg(ap, double), *p, flags, width, prec);
            break;
        case '%': emit(s, "%", 1); break;
        default: emit(s, p - 1, 2); break;
        }
    }
    return (int)s->len;
}

int vfprintf(FILE *restrict f, const char *restrict fmt, va_list ap)
{
    sink s = {0, 0, 0, f};
    return vformat(&s, fmt, ap);
}

int vsnprintf(char *restrict buf, size_t n, const char *restrict fmt, va_list ap)
{
    sink s = {buf, n, 0, 0};
    int r = vformat(&s, fmt, ap);
    if (n) buf[s.len < n ? s.len : n - 1] = 0;
    return r;
}

int printf(const char *restrict fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    int r = vfprintf(stdout, fmt, ap);
    va_end(ap);
    return r;
}

int fprintf(FILE *restrict f, const char *restrict fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    int r = vfprintf(f, fmt, ap);
    va_end(ap);
    return r;
}

int snprintf(char *restrict s, size_t n, const char *restrict fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    int r = vsnprintf(s, n, fmt, ap);
    va_end(ap);
    return r;
}

int sprintf(char *restrict s, const char *restrict fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    int r = vsnprintf(s, (size_t)-1 >> 1, fmt, ap);
    va_end(ap);
    return r;
}
