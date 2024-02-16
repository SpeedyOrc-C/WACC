#include <unistd.h>
#include <stdio.h>

int main(void) {
    printf("%p", (void *)1131796);
    fflush(0);
    write(1, "\n", 2);
    return 0;
}