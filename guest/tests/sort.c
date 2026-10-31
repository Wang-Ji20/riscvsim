/* qsort, strings and recursion. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int by_value(const void *a, const void *b)
{
    long x = *(const long *)a, y = *(const long *)b;
    return (x > y) - (x < y);
}

static int by_name(const void *a, const void *b)
{
    return strcmp(*(const char *const *)a, *(const char *const *)b);
}

static long ackermann(long m, long n)
{
    if (m == 0) return n + 1;
    if (n == 0) return ackermann(m - 1, 1);
    return ackermann(m - 1, ackermann(m, n - 1));
}

int main(void)
{
    long v[1000];
    unsigned long long s = 12345;
    for (int i = 0; i < 1000; i++) {
        s = s * 6364136223846793005ULL + 1442695040888963407ULL;
        v[i] = (long)(s >> 33) - (1L << 30);
    }
    qsort(v, 1000, sizeof v[0], by_value);
    int sorted = 1;
    for (int i = 1; i < 1000; i++) sorted &= v[i - 1] <= v[i];
    printf("sorted=%d min=%ld median=%ld max=%ld\n", sorted, v[0], v[500], v[999]);

    const char *names[] = {"sifive", "thead", "spacemit", "tenstorrent", "andes", "starfive", "esperanto"};
    qsort(names, 7, sizeof names[0], by_name);
    for (int i = 0; i < 7; i++) printf("%s%s", names[i], i < 6 ? ", " : "\n");

    char buf[64];
    strcpy(buf, "risc");
    strcat(buf, "-v");
    printf("%s len=%zu strstr=%s strchr=%s cmp=%d\n", buf, strlen(buf), strstr(buf, "-v"), strchr(buf, 's'),
           strcmp("abc", "abd") < 0);
    printf("ackermann(2, 3)=%ld ackermann(3, 5)=%ld\n", ackermann(2, 3), ackermann(3, 5));
    printf("strtol: %ld %ld %ld\n", strtol("  -1234", NULL, 10), strtol("0x7f", NULL, 0), strtol("777", NULL, 8));
    return 0;
}
