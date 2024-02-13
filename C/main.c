#include <stdlib.h>
#include <memory.h>
#include "WACC.h"

int main(void) {
    WACC_Pair *p = new_WACC_Pair(1, 2);
    WACC_Pair_First(p) = 114514;
    WACC_print_Int(WACC_Pair_First(p));
}
