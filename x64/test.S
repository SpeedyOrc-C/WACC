#ifdef __APPLE__
    #define fflush   _fflush
    #define write   _write
    #define printf  _printf
    #define exit    _exit
    
    .globl _main
    #define main _main
#endif

#ifdef __linux__
    #define flush   fflush@PLT
    #define write   write@PLT
    #define printf  printf@PLT
    #define exit    exit@PLT

    .globl main
#endif

main:
    push %rbp

    mov $0, %rdi
    call println_pointer

    mov $114514, %rdi
    call println_pointer

    mov $233333, %edi
    call println_int

    mov $'C', %dil
    call println_char

    mov $0, %dil
    call println_bool

    mov $1, %dil
    call println_bool

    pop %rbp
    mov $0, %edi
    call exit

println_char:
    push %rbp

    call print_char
    call print_line_break

    pop %rbp
    ret

print_char:
    push %rbp

    mov %dil, %sil
    lea str_format_char(%rip), %rdi
    call printf

    mov $0, %rdi
    call fflush

    pop %rbp
    ret

println_int:
    push %rbp

    call print_int
    call print_line_break

    pop %rbp
    ret

print_int:
    push %rbp

    mov %edi, %esi
    lea str_format_int(%rip), %rdi
    call printf
    
    mov $0, %rdi
    call fflush

    pop %rbp
    ret

println_pointer:
    push %rbp

    call print_pointer
    call print_line_break

    pop %rbp
    ret

print_pointer:
    push %rbp

    cmp $0, %rdi
    jz print_null_pointer

    print_normal_pointer:
        mov %rdi, %rsi
        lea str_format_pointer(%rip), %rdi
        call printf
        
        mov $0, %rdi
        call fflush

        pop %rbp
        ret
    
    print_null_pointer:
        mov $1, %edi
        lea str_nil(%rip), %rsi
        mov $6, %edx
        call write

        pop %rbp
        ret

println_bool:
    push %rbp

    call print_bool
    call print_line_break
    
    pop %rbp
    ret

print_bool:
    push %rbp
    cmp $0, %dil
    jz print_false

    print_true:
        mov $1, %edi
        lea str_true(%rip), %rsi
        mov $5, %edx
        call write

        pop %rbp
        ret

    print_false:
        mov $1, %edi
        lea str_false(%rip), %rsi
        mov $6, %edx
        call write
        
        pop %rbp
        ret

print_line_break:
    push %rbp

    mov $1, %edi
    lea str_line_break(%rip), %rsi
    mov $2, %edx
    call write

    pop %rbp
    ret

str_true:           .asciz "true"
str_false:          .asciz "false"
str_nil:            .asciz "(nil)"
str_line_break:     .asciz "\n"
str_format_char:    .asciz "%c"
str_format_int:     .asciz "%d"
str_format_pointer: .asciz "%p"
