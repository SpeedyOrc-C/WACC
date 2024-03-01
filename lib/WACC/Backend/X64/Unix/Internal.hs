module WACC.Backend.X64.Unix.Internal where

import qualified Data.Sequence as Sq
import WACC.Backend.X64.Structure
import WACC.IR.Structure (Size(..), SingleStatement (AssignIndirect))

newFunction :: String 
    -> [(String, String)]
    -> [Instruction] 
    -> Sq.Seq Instruction
newFunction name strings doblock
    = (stringsToInstructions strings Sq.|> Text)
        Sq.>< (Label name Sq.<| 
        Sq.fromList 
        [Push (Register (RBP, B8)),
        Move (Register (RSP, B8)) (Register (RBP, B8)),
        And (Immediate $ ImmediateInt (-16)) (Register (RSP, B8))]) Sq.>< 
        Sq.fromList doblock

newPrintFunction :: String 
    -> [(String, String)]
    -> [Instruction] 
    -> Sq.Seq Instruction
newPrintFunction name strings doblock = 
    newFunction name strings doblock'
    where
        doblock' 
            = doblock ++
                [Move (Immediate $ ImmediateInt 0) (Register (RAX, B1)),
                Call "printf",
                Move (Immediate $ ImmediateInt 0) (Register (RDI, B8)),
                Call "fflush",
                Leave,
                Return]

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

errorFunction :: String -> String -> Sq.Seq Instruction
errorFunction name err
    = stringsToInstructions [(label, err')] Sq.><
        Sq.fromList
        [Text,
        Label name,
        And (Immediate $ ImmediateInt (-16)) (Register (RSP, B8)),
        LoadAddress
            (MemoryIndirect (Just (ImmediateLabel label)) (RIP, B8) Nothing)
            (Register (RDI, B8)),
        Call "_prints",
        Move (Immediate $ ImmediateInt (-1)) (Register (RDI, B1)),
        Call "exit"]
    where
        label = ".L._" ++ name ++ "str0" 
        err' = "fatal error: " ++ err ++ "\n"

errorIntParam :: String -> String -> Sq.Seq Instruction
errorIntParam name err
    = stringsToInstructions [(label, err')] Sq.>< 
    Sq.fromList [
        Text,
        Label name,
        And (Immediate $ ImmediateInt (-16)) (Register (RSP, B8)),
        LoadAddress
            (stringAddress label)
            (Register (RDI, B8)),
        Move (Immediate $ ImmediateInt 0) (Register (RAX, B1)),
        Call "printf",
        Move (Immediate $ ImmediateInt 0) (Register (RDI, B8)),
        Call "fflush",
        Move (Immediate $ ImmediateInt (-1)) (Register (RDI, B1)),
        Call "exit"
    ]
    where
        label = ".L._" ++ name ++ "str0" 
        err' = "fatal error: " ++ err ++ "\n"
{-
.section .rodata
28	# length of .L._printb_str0
29		.int 5
30	.L._printb_str0:
31		.asciz "false"
32	# length of .L._printb_str1
33		.int 4
34	.L._printb_str1:
35		.asciz "true"
36	# length of .L._printb_str2
37		.int 4
38	.L._printb_str2:
39		.asciz "%.*s"
40	.text
41	_printb:
42		pushq %rbp
43		movq %rsp, %rbp
44		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
45		andq $-16, %rsp
46		cmpb $0, %dil
47		jne .L_printb0
48		leaq .L._printb_str0(%rip), %rdx
49		jmp .L_printb1
50	.L_printb0:
51		leaq .L._printb_str1(%rip), %rdx
52	.L_printb1:
53		movl -4(%rdx), %esi
54		leaq .L._printb_str2(%rip), %rdi
55		# on x86, al represents the number of SIMD registers used as variadic arguments
56		movb $0, %al
57		call printf@plt
58		movq $0, %rdi
59		call fflush@plt
60		movq %rbp, %rsp
61		popq %rbp
62		ret
-}
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

printString :: Sq.Seq Instruction
printString =
    newPrintFunction "_prints" [(".L._prints_str0", "%.*s")] 
    [Move (Register (RDI, B8)) (Register (RDX, B8)),
    Move (MemoryIndirect (Just $ ImmediateInt (-4)) (RDI, B8) Nothing)
         (Register (RSI, B4)),
    LoadAddress (MemoryIndirect (Just ".L._prints_str0") 
                (RIP, B8) Nothing) 
                (Register (RDI, B8))]

printChar :: Sq.Seq Instruction
printChar = 
    newPrintFunction "_printc" [(".L._printc_str0", "%c")]
    [Move (Register (RDI, B1)) (Register (RSI, B1)),
    LoadAddress (MemoryIndirect (Just ".L._printc_str0") (RIP, B8) Nothing) (Register (RDI, B8))]

printInt :: Sq.Seq Instruction
printInt = 
    newPrintFunction "_printi" [(".L.printi_str0", "%d")] 
    [Move (Register (RDI, B4)) (Register (RSI, B4)),
    LoadAddress 
        (MemoryIndirect (Just ".L.printi_str0") (RIP, B8) Nothing) 
        (Register (RDI, B8))]

{-
seek_array_element4:
    push %rbp
    mov %rsp, %rbp

    movslq %esi, %rsi
    lea (%rdi, %rsi, 4), %rax

    leave
    ret
-}

seekArrayElement :: Int -> Sq.Seq Instruction
seekArrayElement size = Sq.fromList
    [
    Label $ "seek_array_element" ++ show size,
    Push (Register (RBP, B8)),
    Move (Register (RSP, B8)) (Register (RBP, B8)),
    Compare (Immediate $ ImmediateInt 0) (Register (RSI, B4)),
    JumpWhen Less "_errOutOfBounds",
    Compare (MemoryIndirect (Just $ ImmediateInt (-4)) (RDI,B8) Nothing) (Register (RSI, B4)),
    JumpWhen GreaterEqual "_errOutOfBounds",
    MoveSignSizeExtend B4 B8 (Register (RSI, B4)) (Register (RSI, B8)),
    LoadAddress (MemoryIndirect Nothing (RDI, B8) (Just ((RSI, B8), size))) (Register (RAX, B8)),

    Leave,
    Return
    ]

seekArrayElement1 :: Sq.Seq Instruction
seekArrayElement1 = seekArrayElement 1
seekArrayElement4 :: Sq.Seq Instruction
seekArrayElement4 = seekArrayElement 4
seekArrayElement8 :: Sq.Seq Instruction
seekArrayElement8 = seekArrayElement 8

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
printBool = newPrintFunction 
    "_printb" 
    [(".L._printb_str0", "false"), 
        (".L._printb_str1", "true"), 
        (".L._printb_str2", "%.*s")] 
    [Compare (Immediate $ ImmediateInt 0) (Register (RDI, B1)),
     JumpWhen NotEqual ".L_printb0",
     LoadAddress (stringAddress ".L._printb_str0") (Register (RDX, B8)),
     Jump ".L_printb1",
     Label ".L_printb0",
     LoadAddress (stringAddress ".L._printb_str1") (Register (RDX, B8)),
     Label ".L_printb1",
     Move (MemoryIndirect (Just $ ImmediateInt (-4)) 
        (RDX, B8) Nothing) 
        (Register (RSI, B4)),
     LoadAddress (stringAddress ".L._printb_str2") (Register (RDI, B8))
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
printLineBreak :: Sq.Seq Instruction
printLineBreak = newFunction "print_line_break" [("line_break", "\n")]
    [
    Move (Immediate $ ImmediateInt 1) (Register (RDI, B4)),
    LoadAddress (stringAddress "line_break") (Register (RSI, B8)),
    Move (Immediate $ ImmediateInt 1) (Register (RDX, B4)),
    Call "write",
    Leave,
    Return
    ]

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
    newPrintFunction
        "_printp" 
        [(".L._printp_str0", "%p"), (".L._printp_str1", "(nil)")]
        [Compare (Immediate (ImmediateInt 0)) (Register (RDI, B8)),
        JumpWhen Equal "print_null",
        Move (Register (RDI, B8)) (Register (RSI, B8)),
        LoadAddress
            (MemoryIndirect (Just ".L._printp_str0") (RIP, B8) Nothing)
            (Register (RDI, B8))] Sq.>< 
    Sq.fromList
    [Label "print_null",
    LoadAddress (MemoryIndirect (Just ".L._printp_str1") (RIP, B8) Nothing)
                (Register (RDI, B8)),
    Call "_prints",
    Leave,
    Return]

errorOutOfMemory :: Sq.Seq Instruction
errorOutOfMemory = errorFunction "_errOutOfMemory" "out of memory"

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
errorNull = errorFunction 
    "_errNull" 
    "null pair dereferenced or freed"

errorDivideZero :: Sq.Seq Instruction
errorDivideZero = errorFunction
    "_errDivZero"
    "division or modulo by zero"

errorOverFlow :: Sq.Seq Instruction
errorOverFlow = errorFunction 
    "_errOverFlow"
    "integer overflow or underflow occurred"

errorBadChar :: Sq.Seq Instruction
errorBadChar = errorIntParam
    "_errBadChar"
    "int %d is not ascii character 0-127"

errorOutOfBounds :: Sq.Seq Instruction
errorOutOfBounds = errorIntParam "_errOutOfBounds" "array index %d out of bounds"

readHelperFunction :: String -> (String, String) -> Size -> Sq.Seq Instruction
readHelperFunction name str@(label, _) size
    = newFunction name [str] [
        Subtract (Immediate $ ImmediateInt 16) (Register (RSP, B8)),
        Move (Register (RDI, B1)) (MemoryIndirect Nothing (RSP, B8) Nothing),
        LoadAddress (MemoryIndirect Nothing (RSP, B8) Nothing) (Register (RSI, B8)),

        LoadAddress
            (stringAddress label)
            (Register (RDI, B8)),

        Move (Immediate $ ImmediateInt 0) (Register (RAX, B1)),
        Call "scanf",
        MoveSignSizeExtend size B8
            (MemoryIndirect Nothing (RSP, B8) Nothing)
            (Register (RAX, B8)),

        Add (Immediate $ ImmediateInt 16) (Register (RSP, B8))
    ]

readInt :: Sq.Seq Instruction
readInt = 
    readHelperFunction "readInt" (".L._readi_str0", "%d") B4

readChar :: Sq.Seq Instruction
readChar = 
    readHelperFunction "readChar" (".L._readc_str0", " %c") B1
   
