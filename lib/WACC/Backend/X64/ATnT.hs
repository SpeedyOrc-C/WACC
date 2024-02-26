module WACC.Backend.X64.ATnT where

import WACC.Backend.X64.Structure
import WACC.IR.Structure (Size(..))
import qualified Data.Sequence as Sq
import Data.Foldable (Foldable(toList))

{- Define a typeclass for types that 
   can be converted to AT&T assembly syntax. -}
class ATnT a where
    atnt :: a -> String

{- Convert a register and its size to its AT&T syntax string representation. -}
atntRegister :: Register -> String
atntRegister (physicalRegister, size) = case physicalRegister of
    RAX -> case size of B8 -> "rax"; B4 -> "eax"; B2 -> "ax"; B1 -> "al"
    RCX -> case size of B8 -> "rcx"; B4 -> "ecx"; B2 -> "cx"; B1 -> "cl"
    RDX -> case size of B8 -> "rdx"; B4 -> "edx"; B2 -> "dx"; B1 -> "dl"
    RBX -> case size of B8 -> "rbx"; B4 -> "ebx"; B2 -> "bx"; B1 -> "bl"
    RSI -> case size of B8 -> "rsi"; B4 -> "esi"; B2 -> "si"; B1 -> "sil"
    RDI -> case size of B8 -> "rdi"; B4 -> "edi"; B2 -> "di"; B1 -> "dil"
    RSP -> case size of B8 -> "rsp"; B4 -> "esp"; B2 -> "sp"; B1 -> "spl"
    RBP -> case size of B8 -> "rbp"; B4 -> "ebp"; B2 -> "bp"; B1 -> "bpl"
    R8 -> case size of B8 -> "r8"; B4 -> "r8d"; B2 -> "r8w"; B1 -> "r8b"
    R9 -> case size of B8 -> "r9"; B4 -> "r9d"; B2 -> "r9w"; B1 -> "r9b"
    R10 -> case size of B8 -> "r10"; B4 -> "r10d"; B2 -> "r10w"; B1 -> "r10b"
    R11 -> case size of B8 -> "r11"; B4 -> "r11d"; B2 -> "r11w"; B1 -> "r11b"
    R12 -> case size of B8 -> "r12"; B4 -> "r12d"; B2 -> "r12w"; B1 -> "r12b"
    R13 -> case size of B8 -> "r13"; B4 -> "r13d"; B2 -> "r13w"; B1 -> "r13b"
    R14 -> case size of B8 -> "r14"; B4 -> "r14d"; B2 -> "r14w"; B1 -> "r14b"
    R15 -> case size of B8 -> "r15"; B4 -> "r15d"; B2 -> "r15w"; B1 -> "r15b"

    RIP -> case size of
        B8 -> "rip"
        _ -> error "Cannot use RIP with size other than 8 bits"

{- Make Register, Immediate, and Operand types instances of ATnT,
   enabling their conversion to AT&T syntax.-}
instance ATnT Register where
    atnt reg = "%" ++ atntRegister reg

instance ATnT Immediate where
    atnt (ImmediateInt n) = show n
    atnt (ImmediateChar c) = show c
    atnt (ImmediateLabel l) = l

instance ATnT Operand where
    atnt (Immediate imm) = "$" ++ atnt imm
    atnt (Register reg) = atnt reg
    atnt (MemoryDirect imm) = "(" ++ atnt imm ++ ")"
    atnt (MemoryIndirect offset base index) =
        offset' ++ "(" ++ base' ++ index' ++ ")"
        where
        offset' = maybe "" atnt offset
        base' = atnt base
        index' = case index of
            Just (reg, scale) -> ", " ++ atnt reg ++ ", " ++ show scale
            Nothing -> ""

{- Helper function to prepend an appropriate indent to most instructions. -}
ident :: Instruction -> String
ident = \case
    Label {} -> ""
    IfDefined {} -> ""
    Define {} -> ""
    EndIf -> ""
    Global {} -> ""
    _ -> "    "

{- Convert a Size value to its corresponding AT&T operand size suffix. -}
sizeSuffix :: Size -> String
sizeSuffix = \case
    B1 -> "b"
    B2 -> "w"
    B4 -> "l"
    B8 -> "q"

{- Implement ATnT instance for Instruction, 
   converting instructions to AT&T syntax. -}
instance ATnT Instruction where
    atnt s = ident s ++ case s of
        Label l -> l ++ ":"
        IfDefined l -> "#ifdef " ++ l
        EndIf -> "#endif"
        Define l s -> "#define " ++ l ++ " " ++ s

        Move from to -> "mov " ++ atnt from ++ ", " ++ atnt to
        MoveSize size from to -> "mov" ++ sizeSuffix size ++ " " ++ atnt from
                                 ++ ", " ++ atnt to

        LoadAddress from to -> "lea " ++ atnt from ++ ", " ++ atnt to
        Push op -> "push " ++ atnt op
        Pop op -> "pop " ++ atnt op
        Call op -> "call " ++ atnt op
        Return -> "ret"
        Leave -> "leave"
        EmptyLine -> ""

        Not op -> "not " ++ atnt op
        Add from to -> "add " ++ atnt from ++ ", " ++ atnt to
        Subtract from to -> "sub " ++ atnt from ++ ", " ++ atnt to

        Jump l -> "jmp " ++ atnt l
        JumpZero l -> "jz " ++ atnt l

        Test a b -> "test " ++ atnt a ++ ", " ++ atnt b

        Int n -> ".int " ++ show n
        Global l -> ".globl " ++ l
        AsciiZero s -> ".asciz " ++ show s

        e -> error $ "ATnT: " ++ show e

{- Convert a sequence of instructions to a single AT&T syntax string. -}
instance ATnT (Sq.Seq Instruction) where
    atnt = unlines . map atnt . toList

{- Generate the AT&T syntax assembly file from a list of elements
   that can be converted to AT&T syntax. -}
generateFile :: ATnT a => [a] -> String
generateFile = unlines . map atnt
