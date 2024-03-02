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
    SectionReadOnly -> ""
    SectionText -> ""
    SectionLiteral4 -> ""
    SectionCString -> ""
    _ -> "    "

{- Convert a Size value to its corresponding AT&T operand size suffix. -}
sizeSuffix :: Size -> String
sizeSuffix = \case
    B1 -> "b"
    B2 -> "w"
    B4 -> "l"
    B8 -> "q"

conditionSuffix :: Condition -> String
conditionSuffix = \case
    Equal -> "e"
    NotEqual -> "ne"
    Zero -> "z"
    NotZero -> "nz"
    Negative -> "s"
    NotNegative -> "ns"
    Greater -> "g"
    GreaterEqual -> "ge"
    Less -> "l"
    LessEqual -> "le"
    Above -> "a"
    AboveEqual -> "ae"
    Below -> "b"
    BelowEqual -> "be"
    Overflow -> "o"

{- Implement ATnT instance for Instruction,
   converting instructions to AT&T syntax. -}
instance ATnT Instruction where
    atnt s = ident s ++ case s of
        Label l -> l ++ ":"
        IfDefined l -> "#ifdef " ++ l
        EndIf -> "#endif"
        Define l str -> "#define " ++ l ++ " " ++ str

        Move from to -> "mov " ++ combineTwoOp from to
        MoveSize size from to ->
            "mov" ++ sizeSuffix size ++ " " ++
            combineTwoOp from to
        MoveZeroSizeExtend size size' from to ->
            "movz" ++ sizeSuffix size ++ sizeSuffix size' ++ " " ++
            combineTwoOp from to
        MoveSignSizeExtend size size' from to ->
            "movs" ++ sizeSuffix size ++ sizeSuffix size' ++ " " ++
            combineTwoOp from to
        CompareMove c from to ->
            "cmov" ++ conditionSuffix c ++ " " ++
            combineTwoOp from to

        LoadAddress from to -> "lea " ++ combineTwoOp from to
        Push op -> "push " ++ atnt op
        Pop op -> "pop " ++ atnt op
        Call op -> "call " ++ atnt op
        Return -> "ret"
        Leave -> "leave"
        EmptyLine -> ""
        CLTD -> "cltd"
        Set cond operand -> "set" ++ conditionSuffix cond ++ " " ++ atnt operand

        Not op -> "not " ++ atnt op
        Xor from to-> "xor " ++ combineTwoOp from to
        Add from to -> "add "   ++ combineTwoOp from to
        Subtract from to -> "sub " ++ combineTwoOp from to
        DivideI a-> "idivl " ++ atnt a
        Multiply from to -> "imull " ++ combineTwoOp from to

        Increase op -> "inc " ++ atnt op
        Decrease op -> "dec " ++ atnt op

        And from to -> "and " ++ combineTwoOp from to
        Or from to -> "or " ++ combineTwoOp from to

        Jump l -> "jmp " ++ atnt l
        JumpWhen c l -> "j" ++ conditionSuffix c ++ " " ++ atnt l

        Test a b -> "test " ++ combineTwoOp a b
        Compare a b -> "cmp " ++ combineTwoOp a b

        Int n -> ".int " ++ show n
        Global l -> ".globl " ++ l
        AsciiZero str -> ".asciz " ++ show str

        SectionText -> "section_text"
        SectionReadOnly -> "section_read_only"
        SectionLiteral4 -> "section_literal4"
        SectionCString -> "section_cstring"

        Comment c -> "// " ++ c
        e -> error $ "ATnT: " ++ show e

{- Convert a sequence of instructions to a single AT&T syntax string. -}
instance ATnT (Sq.Seq Instruction) where
    atnt = unlines . map atnt . toList

{- Generate the AT&T syntax assembly file from a list of elements
   that can be converted to AT&T syntax. -}
generateFile :: ATnT a => [a] -> String
generateFile = unlines . map atnt

{- This is a helper function which combines two oprands to
   an instruction string. -}
combineTwoOp :: Operand -> Operand -> String
combineTwoOp op1 op2 = atnt op1 ++ ", " ++ atnt op2
