module WACC.Backend.X64.Unix.Internal where

import qualified Data.Sequence as Sq
import WACC.Backend.X64.Structure
import WACC.IR.Structure (Size(..))

{-
print_string:
    push %rbp
    mov %rsp, %rbp

    mov %rdi, %rax

    mov $1, %edi
    lea 4(%rax), %rsi
    mov (%rax), %edx
    call write

    leave
    ret
-}
printString :: Sq.Seq Instruction
printString = Sq.fromList
    [
    Label "print_string",
    Push (Register (RBP, B8)),
    Move (Register (RSP, B8)) (Register (RBP, B8)),
    Move (Register (RDI, B8)) (Register (RAX, B8)),

    Move (Immediate $ ImmediateInt 1) (Register (RDI, B4)),
    LoadAddress (MemoryIndirect (Just (ImmediateInt 4)) (RAX, B8) Nothing) 
                (Register (RSI, B8)),
    Move (MemoryIndirect Nothing (RAX, B8) Nothing) (Register (RDX, B4)),
    Call "write",

    Leave,
    Return
    ]

{-
format_int: .asciz "%d"
print_int:
    push %rbp
    mov %rsp, %rbp

    mov %edi, %esi
    lea format_int(%rip), %rdi
    and $-16, %rsp
    call printf

    leave
    ret
-}
printInt :: Sq.Seq Instruction
printInt = Sq.fromList
    [
    Label "format_int", AsciiZero "%d",

    Label "print_int",
    Push (Register (RBP, B8)),
    Move (Register (RSP, B8)) (Register (RBP, B8)),

    Move (Register (RDI, B4)) (Register (RSI, B4)),
    LoadAddress (MemoryIndirect (Just "format_int") (RIP, B8) Nothing) (Register (RDI, B8)),
    And (Immediate $ ImmediateInt (-16)) (Register (RSP, B8)),
    Call "printf",

    Move (Immediate $ ImmediateInt 0) (Register (RDI, B8)),
    Call "fflush",

    Leave,
    Return
    ]

{-
bool_true: .asciz "true"
bool_false: .asciz "false"
print_bool:
    push %rbp
    mov %rsp, %rbp

    cmp $0, %dil
    je print_false
print_true:
    mov $1, %edi
    lea bool_true(%rip), %rsi
    mov $4, %edx
    call write

    leave
    ret
print_false:
    mov $1, %edi
    lea bool_false(%rip), %rsi
    mov $5, %edx
    call write

    leave
    ret
-}

printBool :: Sq.Seq Instruction
printBool = Sq.fromList
    [
    Label "bool_true", AsciiZero "true",
    Label "bool_false", AsciiZero "false",

    Label "print_bool",
    Push (Register (RBP, B8)),
    Move (Register (RSP, B8)) (Register (RBP, B8)),

    Compare (Immediate (ImmediateInt 0)) (Register (RDI, B1)),
    JumpWhen Equal "print_false",

    Label "print_true",
    Move (Immediate $ ImmediateInt 1) (Register (RDI, B4)),
    LoadAddress (MemoryIndirect (Just "bool_true") (RIP, B8) Nothing) (Register (RSI, B8)),
    Move (Immediate $ ImmediateInt 4) (Register (RDX, B4)),
    Call "write",
    Leave,
    Return,

    Label "print_false",
    Move (Immediate $ ImmediateInt 1) (Register (RDI, B4)),
    LoadAddress (MemoryIndirect (Just "bool_false") (RIP, B8) Nothing) (Register (RSI, B8)),
    Move (Immediate $ ImmediateInt 5) (Register (RDX, B4)),
    Call "write",
    Leave,
    Return
    ]

{-
line_break: .asciz "\n"
print_line_break:
    push %rbp
    mov %rsp, %rbp

    mov $1, %edi
    lea line_break(%rip), %rsi
    mov $2, %edx
    call write

    leave
    ret
-}

{-
.section .rodata
271	# length of .L._printp_str0
272		.int 2
273	.L._printp_str0:
274		.asciz "%p"
275	.text
276	_printp:
277		pushq %rbp
278		movq %rsp, %rbp
279		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
280		andq $-16, %rsp
281		movq %rdi, %rsi
282		leaq .L._printp_str0(%rip), %rdi
283		# on x86, al represents the number of SIMD registers used as variadic arguments
284		movb $0, %al
285		call printf@plt
286		movq $0, %rdi
287		call fflush@plt
288		movq %rbp, %rsp
289		popq %rbp
290		ret
-}

printPointer :: Sq.Seq Instruction
printPointer =
    Sq.fromList [
        Label ".L._printp_str0",
        AsciiZero "%p",
        Label ".nil.string",
        Int 6,
        AsciiZero "(nil)",
        Label "print_pointer",
        Push (Register (RBP, B8)),
        Move (Register (RSP, B8)) (Register (RBP, B8)),
        Compare (Immediate (ImmediateInt 0)) (Register (RDI, B8)),
        JumpWhen Equal "print_null",
        And (Immediate $ ImmediateInt (-16)) (Register (RSP, B8)),
        Move (Register (RDI, B8)) (Register (RSI, B8)),
        LoadAddress
            (MemoryIndirect (Just ".L._printp_str0") (RIP, B8) Nothing)
            (Register (RDI, B8)),
        Move (Immediate $ ImmediateInt 0) (Register (RAX, B1)),
        Call "printf",
        Move (Immediate $ ImmediateInt 0) (Register (RDI, B8)),
        Call "fflush",
        Move (Register (RBP, B8)) (Register (RSP, B8)),
        Pop (Register (RBP, B8)),
        Return,
        Label "print_null",
        LoadAddress (MemoryIndirect (Just ".nil.string") (RIP, B8) Nothing)
                (Register (RDI, B8)),
        Call "print_string",
        Return
    ]
{-
.section .rodata
346	# length of .L._errOutOfMemory_str0
347		.int 27
348	.L._errOutOfMemory_str0:
349		.asciz "fatal error: out of memory\n"
350	.text
351	_errOutOfMemory:
352		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
353		andq $-16, %rsp
354		leaq .L._errOutOfMemory_str0(%rip), %rdi
355		call _prints
356		movb $-1, %dil
357		call exit@plt
-}

errorOutOfMemory :: Sq.Seq Instruction
errorOutOfMemory = Sq.fromList
    [
        Int 27,
        Label ".L._errOutOfMemory_str0",
        AsciiZero "fatal error: out of memory\n",
        Label "_errOutOfMemory",
        And (Immediate $ ImmediateInt (-16)) (Register (RSP, B8)),
        LoadAddress
            (MemoryIndirect (Just ".L._errOutOfMemory_str0") (RIP, B8) Nothing)
            (Register (RDI, B8)),
        Call "print_string",
        Move (Immediate $ ImmediateInt (-1)) (Register (RDI, B1)),
        Call "exit"
    ]

{-
_malloc:
293		pushq %rbp
294		movq %rsp, %rbp
295		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
296		andq $-16, %rsp
297		call malloc@plt
298		cmpq $0, %rax
299		je _errOutOfMemory
300		movq %rbp, %rsp
301		popq %rbp
302		ret
-}

mallocFunction :: Sq.Seq Instruction
mallocFunction = Sq.fromList
    [
        Label "w.malloc",
        Push (Register (RBP, B8)),
        Move (Register (RSP, B8)) (Register (RBP, B8)),

        And (Immediate $ ImmediateInt (-16)) (Register (RSP, B8)),
        Call "malloc",
        Compare (Immediate $ ImmediateInt 0) (Register (RAX, B8)),
        JumpWhen Equal "_errOutOfMemory",
        Move (Register (RBP, B8))(Register (RSP, B8)) ,
        Pop (Register (RBP, B8)),
        Return
    ]

{-.section .rodata
86	# length of .L._errNull_str0
87		.int 45
88	.L._errNull_str0:
89		.asciz "fatal error: null pair dereferenced or freed\n"
90	.text
91	_errNull:
92		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
93		andq $-16, %rsp
94		leaq .L._errNull_str0(%rip), %rdi
95		call _prints
96		movb $-1, %dil
97		call exit@plt-}

errorNull :: Sq.Seq Instruction
errorNull = Sq.fromList
    [
        Int 45,
        Label ".L._errNull_str0",
        AsciiZero "fatal error: null pair dereferenced or freed\n",
        Label "_errNull",
        And (Immediate $ ImmediateInt (-16)) (Register (RSP, B8)),
        LoadAddress
            (MemoryIndirect (Just ".L._errNull_str0") (RIP, B8) Nothing)
            (Register (RDI, B8)),
        Call "print_string",
        Move (Immediate $ ImmediateInt (-1)) (Register (RDI, B1)),
        Call "exit"
    ]
{-
.section .rodata
154	# length of .L._errOutOfBounds_str0
155		.int 42
156	.L._errOutOfBounds_str0:
157		.asciz "fatal error: array index %d out of bounds\n"
158	.text
159	_errOutOfBounds:
160		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
161		andq $-16, %rsp
162		leaq .L._errOutOfBounds_str0(%rip), %rdi
163		# on x86, al represents the number of SIMD registers used as variadic arguments
164		movb $0, %al
165		call printf@plt
166		movq $0, %rdi
167		call fflush@plt
168		movb $-1, %dil
169		call exit@plt
-}

errorOutOfBounds :: Sq.Seq Instruction
errorOutOfBounds = Sq.fromList
    [
        Int 42,
        Label ".L._errOutOfBounds_str0",
        AsciiZero "fatal error: array index %d out of bounds\n",
        Label "_errOutOfBounds",
        And (Immediate $ ImmediateInt (-16)) (Register (RSP, B8)),
        LoadAddress
            (MemoryIndirect (Just ".L._errOutOfBounds_str0") (RIP, B8) Nothing)
            (Register (RDI, B8)),
        Move (Immediate $ ImmediateInt 0) (Register (RAX, B1)),
        Call "printf",
        Move (Immediate $ ImmediateInt 0) (Register (RDI, B8)),
        Call "fflush",
        Move (Immediate $ ImmediateInt (-1)) (Register (RDI, B1)),
        Call "exit"
    ]

{-
_arrLoad8:
215		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
216		pushq %rbx
217		cmpl $0, %r10d
218		cmovl %r10, %rsi
219		jl _errOutOfBounds
220		movl -4(%r9), %ebx
221		cmpl %ebx, %r10d
222		cmovge %r10, %rsi
223		jge _errOutOfBounds
224		movq (%r9,%r10,8), %r9
225		popq %rbx
226		ret
-}
arrLoad8 :: Sq.Seq Instruction
arrLoad8 = Sq.fromList
    [
        Label "_arrLoad8",
        Push (Register (RBX, B8)),
        Compare (Immediate $ ImmediateInt 0) (Register (R10, B4)),
        CompareMove Less (Register (R10, B8)) (Register (RSI, B8)),
        JumpWhen Less "_errOutOfBounds",
        Move (MemoryIndirect (Just (ImmediateInt (-4))) (R9, B8) Nothing) (Register (RBX, B4)),
        Compare (Register (RBX, B4)) (Register (R10, B4)),
        CompareMove GreaterEqual (Register (R10, B8)) (Register (RSI, B8)),
        JumpWhen GreaterEqual "_errOutOfBounds",
        Move (MemoryIndirect (Just (ImmediateInt (-4))) (R9, B8) (Just ((R10, B8), 8))) (Register (R9, B8)),
        Pop (Register (RBX, B8)),
        Call "exit"
    ]

arrLoad4 :: Sq.Seq Instruction
arrLoad4 = Sq.fromList
    [
        Label "_arrLoad4",
        Push (Register (RBX, B8)),
        Compare (Immediate $ ImmediateInt 0) (Register (R10, B4)),
        CompareMove Less (Register (R10, B8)) (Register (RSI, B8)),
        JumpWhen Less "_errOutOfBounds",
        Move (MemoryIndirect (Just (ImmediateInt (-4))) (R9, B8) Nothing) (Register (RBX, B4)),
        Compare (Register (RBX, B4)) (Register (R10, B4)),
        CompareMove GreaterEqual (Register (R10, B8)) (Register (RSI, B8)),
        JumpWhen GreaterEqual "_errOutOfBounds",
        MoveSize B4 (MemoryIndirect (Just (ImmediateInt (-4))) (R9, B8) (Just ((R10, B8), 4))) (Register (R9, B4)),
        Pop (Register (RBX, B8)),
        Call "exit"
    ]

arrLoad1 :: Sq.Seq Instruction
arrLoad1 = Sq.fromList
    [
        Label "_arrLoad1",
        Push (Register (RBX, B8)),
        Compare (Immediate $ ImmediateInt 0) (Register (R10, B4)),
        CompareMove Less (Register (R10, B8)) (Register (RSI, B8)),
        JumpWhen Less "_errOutOfBounds",
        Move (MemoryIndirect (Just (ImmediateInt (-4))) (R9, B8) Nothing) (Register (RBX, B4)),
        Compare (Register (RBX, B4)) (Register (R10, B4)),
        CompareMove GreaterEqual (Register (R10, B8)) (Register (RSI, B8)),
        JumpWhen GreaterEqual "_errOutOfBounds",
        MoveSize B1 (MemoryIndirect (Just (ImmediateInt (-4))) (R9, B8) (Just ((R10, B8), 1))) (Register (R9, B1)),
        Pop (Register (RBX, B8)),
        Call "exit"
    ]


{-
.section .rodata
61	# length of .L._println_str0
62		.int 0
63	.L._println_str0:
64		.asciz ""
65	.text
66	_println:
67		pushq %rbp
68		movq %rsp, %rbp
69		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
70		andq $-16, %rsp
71		leaq .L._println_str0(%rip), %rdi
72		call puts@plt
73		movq $0, %rdi
74		call fflush@plt
75		movq %rbp, %rsp
76		popq %rbp
77		ret
-}
printLineBreak :: Sq.Seq Instruction
printLineBreak = Sq.fromList
    [
    Label ".L._println_str0", AsciiZero "",

    Label "print_line_break",
    Push (Register (RBP, B8)),
    Move (Register (RSP, B8)) (Register (RBP, B8)),
    And (Immediate $ ImmediateInt (-16)) (Register (RSP, B8)),
    LoadAddress (MemoryIndirect (Just ".L._println_str0") (RIP, B8) Nothing) 
                (Register (RDI, B8)),
    Call "puts",
    Move (Immediate $ ImmediateInt 0) (Register (RDI, B8)),
    Call "fflush",
    Leave,
    Return
    ]
