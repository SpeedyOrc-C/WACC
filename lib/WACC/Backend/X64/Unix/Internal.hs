module WACC.Backend.X64.Unix.Internal where

import qualified Data.Sequence as Sq
import WACC.Backend.X64.Structure
import WACC.IR.Structure (Size(..))

printString :: Sq.Seq Instruction
printString = Sq.fromList
    [
    Label "print_string",
    Push (Register (RBP, B8)),
    Move (Register (RSP, B8)) (Register (RBP, B8)),
    Move (Register (RDI, B8)) (Register (RAX, B8)),

    Move (Immediate $ ImmediateInt 1) (Register (RDI, B4)),
    LoadAddress (MemoryIndirect Nothing (RAX, B8) Nothing)
                (Register (RSI, B8)),
    Move (MemoryIndirect (Just (ImmediateInt (-4))) (RAX, B8) Nothing) (Register (RDX, B4)),
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
    Int 2,
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

printChar :: Sq.Seq Instruction
printChar = Sq.fromList [
    Label "print_char",
    Push (Register (RBP, B8)),
    Move (Register (RSP, B8)) (Register (RBP, B8)),

    MoveZeroSizeExtend B1 B4 (Register (RDI, B1)) (Register (RDI, B4)),
    Move (Immediate $ ImmediateInt 1) (Register (RSI, B4)),
    Call "putchar",

    Move (Immediate $ ImmediateInt 0) (Register (RDI, B8)),
    Call "fflush",

    Leave,
    Return
    ]

{-
.section .rodata
46	# length of .L._prints_str0
47		.int 4
48	.L._prints_str0:
49		.asciz "%.*s"
50	.text
51	_prints:
52		pushq %rbp
53		movq %rsp, %rbp
54		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
55		andq $-16, %rsp
56		movq %rdi, %rdx
57		movl -4(%rdi), %esi
58		leaq .L._prints_str0(%rip), %rdi
59		# on x86, al represents the number of SIMD registers used as variadic arguments
60		movb $0, %al
61		call printf@plt
62		movq $0, %rdi
63		call fflush@plt
64		movq %rbp, %rsp
65		popq %rbp
66		ret
67
-}
printString' :: Sq.Seq Instruction
printString' = Sq.fromList [
    Int 4,
    Label ".L._prints_str0",
    AsciiZero "%.*s",
    Label "_prints",
    Push (Register (RBP, B8)),
    Move (Register (RSP, B8)) (Register (RBP, B8)),

    And (Immediate $ ImmediateInt (-16)) (Register (RSP, B8)),
    Move (Register (RDI, B8)) (Register (RDX, B8)),
    Move (MemoryIndirect (Just $ ImmediateInt (-4)) (RDI, B8) Nothing)(Register (RSI, B4)),
    LoadAddress (MemoryIndirect (Just ".L._prints_str0") (RIP, B8) Nothing) (Register (RDI, B8)),

    Move (Immediate $ ImmediateInt 0) (Register (RAX, B1)),
    Call "printf",
    Move (Immediate $ ImmediateInt 0) (Register (RDI, B8)),
    Call "fflush",
    Leave,
    Return]


{-
.section .rodata
77	# length of .L._printc_str0
78		.int 2
79	.L._printc_str0:
80		.asciz "%c"
81	.text
82	_printc:
83		pushq %rbp
84		movq %rsp, %rbp
85		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
86		andq $-16, %rsp
87		movb %dil, %sil
88		leaq .L._printc_str0(%rip), %rdi
89		# on x86, al represents the number of SIMD registers used as variadic arguments
90		movb $0, %al
91		call printf@plt
92		movq $0, %rdi
93		call fflush@plt
94		movq %rbp, %rsp
95		popq %rbp
96		ret
-}

printChar' :: Sq.Seq Instruction
printChar' = Sq.fromList [
    Int 2,
    Label ".L._printc_str0",
    AsciiZero "%c",
    Label "_printc",
    Push (Register (RBP, B8)),
    Move (Register (RSP, B8)) (Register (RBP, B8)),

    And (Immediate $ ImmediateInt (-16)) (Register (RSP, B8)),
    Move (Register (RDI, B1)) (Register (RSI, B1)),

    LoadAddress (MemoryIndirect (Just ".L._printc_str0") (RIP, B8) Nothing) (Register (RDI, B8)),

    Move (Immediate $ ImmediateInt 0) (Register (RAX, B1)),
    Call "printf",
    Move (Immediate $ ImmediateInt 0) (Register (RDI, B8)),
    Call "fflush",
    Leave,
    Return
    ]

{-
seek_array_element4:
    push %rbp
    mov %rsp, %rbp

    movslq %esi, %rsi
    lea (%rdi, %rsi, 4), %rax

    leave
    ret
-}

seekArrayElement4 :: Sq.Seq Instruction
seekArrayElement4 = Sq.fromList
    [
    Label "seek_array_element4",
    Push (Register (RBP, B8)),
    Move (Register (RSP, B8)) (Register (RBP, B8)),

    MoveSignSizeExtend B4 B8 (Register (RSI, B4)) (Register (RSI, B8)),
    LoadAddress (MemoryIndirect Nothing (RDI, B8) (Just ((RSI, B8), 4))) (Register (RAX, B8)),

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
        Int 2,
        Label ".L._printp_str0",
        AsciiZero "%p",
        Int 5,
        Label ".nil.string",
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
        Call "_prints",
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
        Call "_prints",
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
        Call "_prints",
        Move (Immediate $ ImmediateInt (-1)) (Register (RDI, B1)),
        Call "exit"
    ]

errorOverFlow :: Sq.Seq Instruction
errorOverFlow = Sq.fromList
    [
        Int 52,
        Label ".L._errOverflow_str0",
        AsciiZero "fatal error: integer overflow or underflow occurred\n",
        Label "_errOverFlow",
        And (Immediate $ ImmediateInt (-16)) (Register (RSP, B8)),
        LoadAddress
            (MemoryIndirect (Just ".L._errOverflow_str0") (RIP, B8) Nothing)
            (Register (RDI, B8)),
        Call "_prints",
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
        Move (MemoryIndirect Nothing (R9, B8) (Just ((R10, B8), 8))) (Register (R9, B8)),
        Pop (Register (RBX, B8)),
        Return
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
        MoveSize B4 (MemoryIndirect Nothing (R9, B8) (Just ((R10, B8), 4))) (Register (R9, B4)),
        Pop (Register (RBX, B8)),
        Return
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
        MoveSize B1 (MemoryIndirect Nothing (R9, B8) (Just ((R10, B8), 1))) (Register (R9, B1)),
        Pop (Register (RBX, B8)),
        Return
    ]


printLineBreak :: Sq.Seq Instruction
printLineBreak = Sq.fromList
    [
    Int 0,
    Label ".L._println_str0",
    AsciiZero "s",
    Label "line_break", AsciiZero "\n",

    Label "print_line_break",
    Push (Register (RBP, B8)),
    Move (Register (RSP, B8)) (Register (RBP, B8)),

    Move (Immediate $ ImmediateInt 1) (Register (RDI, B4)),
    LoadAddress (MemoryIndirect (Just "line_break") (RIP, B8) Nothing) (Register (RSI, B8)),
    Move (Immediate $ ImmediateInt 1) (Register (RDX, B4)),
    Call "write",

    Leave,
    Return
    ]

readInt :: Sq.Seq Instruction
readInt
    = Sq.fromList
        [Int 2,
        Label ".L._readi_str0",
        AsciiZero "%d",
        Label "readInt",
        Push (Register (RBP, B8)),
        Move (Register (RSP, B8)) (Register (RBP, B8)),
        And (Immediate $ ImmediateInt (-16)) (Register (RSP, B8)),
        Subtract (Immediate $ ImmediateInt 16) (Register (RSP, B8)),
        Move (Register (RDI, B1)) (MemoryIndirect Nothing (RSP, B8) Nothing),
        LoadAddress (MemoryIndirect Nothing (RSP, B8) Nothing) (Register (RSI, B8)),

        LoadAddress
            (MemoryIndirect (Just ".L._readi_str0") (RIP, B8) Nothing)
            (Register (RDI, B8)),

        Move (Immediate $ ImmediateInt 0) (Register (RAX, B1)),
        Call "scanf",
        MoveSignSizeExtend B4 B8
            (MemoryIndirect Nothing (RSP, B8) Nothing)
            (Register (RAX, B8)),

        Add (Immediate $ ImmediateInt 16) (Register (RSP, B8)),
        Leave,
        Return
        ]

{-
.section .rodata
43	# length of .L._readc_str0
44		.int 3
45	.L._readc_str0:
46		.asciz " %c"
47	.text
48	_readc:
49		pushq %rbp
50		movq %rsp, %rbp
51		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
52		andq $-16, %rsp
53		# RDI contains the "original" value of the destination of the read
54		# allocate space on the stack to store the read: preserve alignment!
55		# the passed default argument should be stored in case of EOF
56		subq $16, %rsp
57		movb %dil, (%rsp)
58		leaq (%rsp), %rsi
59		leaq .L._readc_str0(%rip), %rdi
60		# on x86, al represents the number of SIMD registers used as variadic arguments
61		movb $0, %al
62		call scanf@plt
63		movsbq (%rsp), %rax
64		addq $16, %rsp
65		movq %rbp, %rsp
66		popq %rbp
67		ret
68
69	.section .rodata
70	# length of .L._readi_str0
71		.int 2
72	.L._readi_str0:
73		.asciz "%d"
74	.text
75	_readi:
76		pushq %rbp
77		movq %rsp, %rbp
78		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
79		andq $-16, %rsp
80		# RDI contains the "original" value of the destination of the read
81		# allocate space on the stack to store the read: preserve alignment!
82		# the passed default argument should be stored in case of EOF
83		subq $16, %rsp
84		movl %edi, (%rsp)
85		leaq (%rsp), %rsi
86		leaq .L._readi_str0(%rip), %rdi
87		# on x86, al represents the number of SIMD registers used as variadic arguments
88		movb $0, %al
89		call scanf@plt
90		movslq (%rsp), %rax
91		addq $16, %rsp
92		movq %rbp, %rsp
93		popq %rbp
94		ret
-}