#include <stdint.h>
#include <stdbool.h>

typedef int32_t WACC_Int;
typedef int8_t WACC_Bool;
typedef int8_t WACC_Char;

typedef struct { int length; char element_size; void *elements; } WACC_Array;
WACC_Array* new_WACC_Array(int length, char element_size, void *elements);
void* WACC_Array_index_check(WACC_Array *array, WACC_Int index);
#define WACC_Array_Index(array, index) (*(void **)(WACC_Array_index_check(array, index), ((char *)array->elements + array->element_size * index)))
void del_WACC_Array(WACC_Array *array);

typedef struct { void* fst; void* snd; } WACC_Pair;
#define WACC_Pair_First(p) (*(void **)p)
#define WACC_Pair_Second(p) (*(void **)((char *)p + sizeof(void *)))
WACC_Pair* new_WACC_Pair(void* fst, void* snd);
void del_WACC_Pair(void *p);

void WACC_print_Int(WACC_Int i);
void WACC_print_Bool(WACC_Bool b);
void WACC_print_Char(WACC_Char c);
void WACC_print_String(WACC_Array *s);
void WACC_print_Address(void *p);

void WACC_println_Int(WACC_Int i);
void WACC_println_Bool(WACC_Bool b);
void WACC_println_Char(WACC_Char c);
void WACC_println_String(WACC_Array *s);
void WACC_println_Address(void *p);