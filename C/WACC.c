#include <stdint.h>
#include <stdlib.h>
#include "WACC.h"

WACC_Array *new_WACC_Array(int length, char element_size) {
    WACC_Array *array = malloc(sizeof(WACC_Array));
    array->length = length;
    array->element_size = element_size;
    array->elements = malloc(length * element_size);
    return array;
}

void del_WACC_Array(WACC_Array *array) {
    free(array->elements);
    free(array);
}

WACC_PairI32I32* new_WACC_PairI32I32(int32_t a, int32_t b)
{
    WACC_PairI32I32 *pair = malloc(sizeof(WACC_PairI32I32));
    pair->a = a;
    pair->b = b;
    return pair;
}

WACC_PairI32I8* new_WACC_PairI32I8(int32_t a, int8_t b)
{
    WACC_PairI32I8 *pair = malloc(sizeof(WACC_PairI32I8));
    pair->a = a;
    pair->b = b;
    return pair;
}

WACC_PairI32Ptr* new_WACC_PairI32Ptr(int32_t a, void *b)
{
    WACC_PairI32Ptr *pair = malloc(sizeof(WACC_PairI32Ptr));
    pair->a = a;
    pair->b = b;
    return pair;
}

WACC_PairI8I32* new_WACC_PairI8I32(int8_t a, int32_t b)
{
    WACC_PairI8I32 *pair = malloc(sizeof(WACC_PairI8I32));
    pair->a = a;
    pair->b = b;
    return pair;
}

WACC_PairI8I8* new_WACC_PairI8I8(int8_t a, int8_t b)
{
    WACC_PairI8I8 *pair = malloc(sizeof(WACC_PairI8I8));
    pair->a = a;
    pair->b = b;
    return pair;
}

WACC_PairI8Ptr* new_WACC_PairI8Ptr(int8_t a, void *b)
{
    WACC_PairI8Ptr *pair = malloc(sizeof(WACC_PairI8Ptr));
    pair->a = a;
    pair->b = b;
    return pair;
}

WACC_PairPtrI32* new_WACC_PairPtrI32(void *a, int32_t b)
{
    WACC_PairPtrI32 *pair = malloc(sizeof(WACC_PairPtrI32));
    pair->a = a;
    pair->b = b;
    return pair;
}

WACC_PairPtrI8* new_WACC_PairPtrI8(void *a, int8_t b)
{
    WACC_PairPtrI8 *pair = malloc(sizeof(WACC_PairPtrI8));
    pair->a = a;
    pair->b = b;
    return pair;
}

WACC_PairPtrPtr* new_WACC_PairPtrPtr(void *a, void *b)
{
    WACC_PairPtrPtr *pair = malloc(sizeof(WACC_PairPtrPtr));
    pair->a = a;
    pair->b = b;
    return pair;
}
