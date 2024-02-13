#include <stdint.h>
#include <stdbool.h>

typedef int32_t WACC_Int;
typedef int8_t WACC_Bool;
typedef int8_t WACC_Char;

typedef struct { int length; char element_size; void *elements; } WACC_Array;
WACC_Array* new_WACC_Array(int length, char element_size);
void del_WACC_Array(WACC_Array *array);

typedef struct { int32_t a; int32_t b; } WACC_PairI32I32;
WACC_PairI32I32* new_WACC_PairI32I32(int32_t a, int32_t b);

typedef struct { int32_t a; int8_t b; } WACC_PairI32I8;
WACC_PairI32I8* new_WACC_PairI32I8(int32_t a, int8_t b);

typedef struct { int32_t a; void *b; } WACC_PairI32Ptr;
WACC_PairI32Ptr* new_WACC_PairI32Ptr(int32_t a, void *b);

typedef struct { int8_t a; int32_t b; } WACC_PairI8I32;
WACC_PairI8I32* new_WACC_PairI8I32(int8_t a, int32_t b);

typedef struct { int8_t a; int8_t b; } WACC_PairI8I8;
WACC_PairI8I8* new_WACC_PairI8I8(int8_t a, int8_t b);

typedef struct { int8_t a; void *b; } WACC_PairI8Ptr;
WACC_PairI8Ptr* new_WACC_PairI8Ptr(int8_t a, void *b);

typedef struct { void *a; int32_t b; } WACC_PairPtrI32;
WACC_PairPtrI32* new_WACC_PairPtrI32(void *a, int32_t b);

typedef struct { void *a; int8_t b; } WACC_PairPtrI8;
WACC_PairPtrI8* new_WACC_PairPtrI8(void *a, int8_t b);

typedef struct { void *a; void *b; } WACC_PairPtrPtr;
WACC_PairPtrPtr* new_WACC_PairPtrPtr(void *a, void *b);
