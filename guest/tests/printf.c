/* printf conversions, including exact floating-point formatting. */
#include <stdio.h>

int main(void)
{
    printf("[%d] [%5d] [%-5d] [%05d] [%+d] [% d] [%.3d]\n", 42, 42, 42, 42, 42, 42, 7);
    printf("[%ld] [%lu] [%lld]\n", -9223372036854775807L - 1, 18446744073709551615UL, 1234567890123LL);
    printf("[%x] [%X] [%#x] [%o] [%#o] [%08lx]\n", 255u, 255u, 255u, 8u, 8u, 0xdeadbeefUL);
    printf("[%hhd] [%hd] [%hhu]\n", 300, 70000, 300);
    printf("[%s] [%10s] [%-10s] [%.3s] [%c%c]\n", "ok", "right", "left", "truncate", 'o', 'k');
    printf("[%*d] [%-*d] [%.*f]\n", 6, 1, 6, 2, 2, 3.14159);
    printf("[%%] [%5.1f%%]\n", 99.5);

    const double xs[] = {0.0, -0.0, 1.0, -2.5, 0.1, 1.0 / 3, 2.0 / 3, 123456.789,
                         1e-5, 1e-300, 4.9e-324, 1.7976931348623157e308, 1e21, 0.5, 1.5, 2.5,
                         1e15, 299792458.0, 6.02214076e23};
    for (unsigned i = 0; i < sizeof xs / sizeof xs[0]; i++) {
        double x = xs[i];
        printf("%.17g | %f | %.0f | %.3f | %e | %.2E | %g | %G | %#.3g | %12.4e|\n", x, x, x, x, x, x,
               x, x, x, x);
    }
    printf("%.40f\n", 0.1);
    printf("%.60e\n", 4.9e-324);
    printf("%f %e %g\n", 1.0 / 0.0, -1.0 / 0.0, 1e308 * 10);
    char buf[16];
    int n = snprintf(buf, sizeof buf, "%s-%d-%s", "truncated", 12345, "output");
    printf("snprintf -> %d \"%s\"\n", n, buf);
    return 0;
}
