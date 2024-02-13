#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <memory.h>
#include "WACC.h"

WACC_Array *new_WACC_Array(int length, char element_size, void* elements) {
    WACC_Array *array = malloc(sizeof(WACC_Array));
    array->length = length;
    array->element_size = element_size;
    array->elements = malloc(length * element_size);
    memcpy(array->elements, elements, length * element_size);
    return array;
}

void *WACC_Array_index_check(WACC_Array *array, WACC_Int index) {
    if (index < 0 || index >= array->length) {
        printf("Runtime error: index out of range.\n");
        printf("Array length: %d, index: %d.\n", array->length, index);
        exit(-1);
    }
    return ((char *)array->elements) + array->element_size * index;
}

void del_WACC_Array(WACC_Array *array) {
    free(array->elements);
    free(array);
}

WACC_Pair* new_WACC_Pair(void* fst, void* snd) {
    WACC_Pair *pair = malloc(sizeof(WACC_Pair));
    pair->fst = fst;
    pair->snd = snd;
    return pair;
}

void del_WACC_Pair(void *p) {
    if (p == NULL) {
        printf("Runtime error: Cannot free a null pointer.\n");
        exit(-1);
    }
    free(p);
}

void WACC_print_Int(WACC_Int i) {
    printf("%d", i);
}

void WACC_print_Bool(WACC_Bool b) {
    printf(b ? "true" : "false");
}

void WACC_print_Char(WACC_Char c) {
    putchar(c);
}

void WACC_print_String(WACC_Array *s) {
    printf("%s", (char *) s->elements);
}

void WACC_print_Address(void *p) {
    if (p == NULL)
        printf("(nil)");
    else
        printf("%p", p);
}

void WACC_println_Int(WACC_Int i) {
    WACC_print_Int(i);
    putchar('\n');
}

void WACC_println_Bool(WACC_Bool b) {
    WACC_print_Bool(b);
    putchar('\n');
}

void WACC_println_Char(WACC_Char c) {
    WACC_print_Char(c);
    putchar('\n');
}

void WACC_println_String(WACC_Array *s) {
    WACC_print_String(s);
    putchar('\n');
}

void WACC_println_Address(void *p) {
    WACC_print_Address(p);
    putchar('\n');
}
