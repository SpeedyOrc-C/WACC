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
printLineBreak = Sq.fromList
    [
    Label "line_break", AsciiZero "\n",

    Label "print_line_break",
    Push (Register (RBP, B8)),
    Move (Register (RSP, B8)) (Register (RBP, B8)),

    Move (Immediate $ ImmediateInt 1) (Register (RDI, B4)),
    LoadAddress (MemoryIndirect (Just "line_break") (RIP, B8) Nothing) 
                (Register (RSI, B8)),
    Move (Immediate $ ImmediateInt 2) (Register (RDX, B4)),
    Call "write",

    Leave,
    Return
    ]
