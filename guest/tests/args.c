/* argv, envp, getenv and the exit status. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv)
{
    printf("argc=%d\n", argc);
    for (int i = 1; i < argc; i++) printf("argv[%d]=\"%s\" (%zu chars)\n", i, argv[i], strlen(argv[i]));
    const char *home = getenv("RISCVSIM_TEST");
    printf("RISCVSIM_TEST=%s\n", home ? home : "(unset)");
    return argc;
}
