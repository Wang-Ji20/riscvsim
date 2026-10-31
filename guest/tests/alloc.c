/* malloc/realloc/free under a pseudo-random workload; prints a checksum. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static unsigned long long state = 88172645463325252ULL;

static unsigned long long next(void)
{
    state ^= state << 13;
    state ^= state >> 7;
    state ^= state << 17;
    return state;
}

int main(void)
{
    enum { SLOTS = 512, ROUNDS = 20000 };
    static unsigned char *slot[SLOTS];
    static size_t size[SLOTS];
    unsigned long long checksum = 0;
    for (int r = 0; r < ROUNDS; r++) {
        int k = (int)(next() % SLOTS);
        if (slot[k] && next() % 3 == 0) {
            for (size_t i = 0; i < size[k]; i++)
                if (slot[k][i] != (unsigned char)(k + i)) {
                    printf("corruption in slot %d\n", k);
                    return 1;
                }
            free(slot[k]);
            slot[k] = NULL;
        } else {
            size_t n = (size_t)(next() % (r % 100 == 0 ? 300000 : 2000)) + 1;
            unsigned char *p = slot[k] ? realloc(slot[k], n) : malloc(n);
            if (!p) { printf("out of memory\n"); return 1; }
            size_t keep = slot[k] ? (size[k] < n ? size[k] : n) : 0;
            for (size_t i = keep; i < n; i++) p[i] = (unsigned char)(k + i);
            slot[k] = p;
            size[k] = n;
        }
    }
    for (int k = 0; k < SLOTS; k++)
        for (size_t i = 0; i < size[k] && slot[k]; i++) checksum = checksum * 31 + slot[k][i];
    int *zeros = calloc(1000, sizeof *zeros);
    int sum = 0;
    for (int i = 0; i < 1000; i++) sum += zeros[i];
    printf("checksum %llu, calloc sum %d\n", checksum, sum);
    return 0;
}
