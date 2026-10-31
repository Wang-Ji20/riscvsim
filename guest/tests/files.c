/* File I/O through the host filesystem: write, read back, seek, unlink. */
#include <stdio.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char **argv)
{
    const char *path = argc > 1 ? argv[1] : "riscvsim-files-test.txt";
    FILE *f = fopen(path, "w");
    if (!f) { printf("cannot create %s\n", path); return 1; }
    for (int i = 1; i <= 5; i++) fprintf(f, "line %d: %d squared is %d\n", i, i, i * i);
    fclose(f);

    f = fopen(path, "r");
    char line[128];
    int lines = 0;
    size_t bytes = 0;
    while (fgets(line, sizeof line, f)) {
        lines++;
        bytes += strlen(line);
        if (lines == 3) printf("third line: %s", line);
    }
    fclose(f);
    printf("%d lines, %zu bytes\n", lines, bytes);

    f = fopen(path, "a");
    fputs("appended\n", f);
    fclose(f);
    f = fopen(path, "r");
    char all[512];
    size_t n = fread(all, 1, sizeof all - 1, f);
    all[n] = 0;
    fclose(f);
    printf("after append: %zu bytes, last line: %s", n, all + n - strlen("appended\n"));

    int unlinked = unlink(path);
    FILE *again = fopen(path, "r");
    printf("unlink: %d, reopen: %s\n", unlinked, again ? "exists" : "gone");
    return 0;
}
