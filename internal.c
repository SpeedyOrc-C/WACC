#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

void error_out_of_bounds(int32_t index)
{
    printf("Error: Array index %d is out of bounds.\n", index);
    exit(-1);
}

void error_bad_char(int8_t c)
{
    printf("Error: %d is not an ASCII character.\n", (int) c);
    exit(-1);
}

void error_overflow(void)
{
    printf("Error: Overflowed or underflowed integer.\n");
    exit(-1);
}

void error_divide_zero(void)
{
    printf("Error: Divide by zero.\n");
    exit(-1);
}

void error_null(void)
{
    printf("Error: Try to access a null pointer.\n");
    exit(-1);
}

void error_pair_null(void)
{
    printf("Error: Try to access a null pair.\n");
    exit(-1);
}

void error_free_null(void)
{
    printf("Error: Try to free a null pointer.\n");
    exit(-1);
}

void error_out_of_memory(void)
{
    printf("Error: Out of memory.\n");
    exit(-1);
}

void print_string(const char* s)
{
    printf("%s", s);
    fflush(stdout);
}

void print_char(const char c)
{
    putchar(c);
    fflush(stdout);
}

void print_int(const int32_t i)
{
    printf("%i", i);
    fflush(stdout);
}

void print_bool(const char b)
{
    printf("%s", b ? "true" : "false");
    fflush(stdout);
}

void print_line_break(void)
{
    puts("");
    fflush(stdout);
}

void print_pointer(void* p)
{
    if (p == NULL)
        printf("(nil)");
    else
        printf("%p", p);

    fflush(stdout);
}

int64_t seek_array_element1(int64_t array, int32_t index)
{
    if (index < 0)
        error_out_of_bounds(index);

    return array + index;
}

int64_t seek_array_element4(int64_t array, int32_t index)
{
    if (index < 0)
        error_out_of_bounds(index);

    return array + (index << 2);
}

int64_t seek_array_element8(int64_t array, int32_t index)
{
    if (index < 0)
        error_out_of_bounds(index);

    return array + (index << 3);
}

int32_t read_int(void)
{
    int i;
    scanf("%d", &i);
    return (int32_t) i;
}

int8_t read_char(void)
{
    char c;
    scanf("%c", &c);
    return (int8_t) c;
}
