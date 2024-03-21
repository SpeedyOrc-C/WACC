module WACC.Backend.X64.ATnT where

import WACC.Backend.X64.Structure
import WACC.IR.Structure (Size(..))
import Data.Sequence ((<|), (|>), (><), Seq(..), singleton, empty)

class ATnT a where
    atnt :: a -> Seq String

instance ATnT a => ATnT (a, a) where
    atnt :: ATnT a => (a, a) -> Seq String
    atnt (a, b) = atnt a >< ", " <| atnt b

instance ATnT Register where
    atnt :: Register -> Seq String
    atnt (reg, size) = ("%" <|) $ singleton $ case (reg, size) of
        (RAX, B8) -> "rax"; (RAX, B4) -> "eax"; (RAX, B2) -> "ax"; (RAX, B1) -> "al";
        (RCX, B8) -> "rcx"; (RCX, B4) -> "ecx"; (RCX, B2) -> "cx"; (RCX, B1) -> "cl";
        (RDX, B8) -> "rdx"; (RDX, B4) -> "edx"; (RDX, B2) -> "dx"; (RDX, B1) -> "dl";
        (RBX, B8) -> "rbx"; (RBX, B4) -> "ebx"; (RBX, B2) -> "bx"; (RBX, B1) -> "bl";
        (RSI, B8) -> "rsi"; (RSI, B4) -> "esi"; (RSI, B2) -> "si"; (RSI, B1) -> "sil";
        (RDI, B8) -> "rdi"; (RDI, B4) -> "edi"; (RDI, B2) -> "di"; (RDI, B1) -> "dil";
        (RSP, B8) -> "rsp"; (RSP, B4) -> "esp"; (RSP, B2) -> "sp"; (RSP, B1) -> "spl";
        (RBP, B8) -> "rbp"; (RBP, B4) -> "ebp"; (RBP, B2) -> "bp"; (RBP, B1) -> "bpl";
        (R8, B8) -> "r8"; (R8, B4) -> "r8d"; (R8, B2) -> "r8w"; (R8, B1) -> "r8b";
        (R9, B8) -> "r9"; (R9, B4) -> "r9d"; (R9, B2) -> "r9w"; (R9, B1) -> "r9b";
        (R10, B8) -> "r10"; (R10, B4) -> "r10d"; (R10, B2) -> "r10w"; (R10, B1) -> "r10b";
        (R11, B8) -> "r11"; (R11, B4) -> "r11d"; (R11, B2) -> "r11w"; (R11, B1) -> "r11b";
        (R12, B8) -> "r12"; (R12, B4) -> "r12d"; (R12, B2) -> "r12w"; (R12, B1) -> "r12b";
        (R13, B8) -> "r13"; (R13, B4) -> "r13d"; (R13, B2) -> "r13w"; (R13, B1) -> "r13b";
        (R14, B8) -> "r14"; (R14, B4) -> "r14d"; (R14, B2) -> "r14w"; (R14, B1) -> "r14b";
        (R15, B8) -> "r15"; (R15, B4) -> "r15d"; (R15, B2) -> "r15w"; (R15, B1) -> "r15b";
        (RIP, B8) -> "rip";
        _ -> error "try to get the register of non normal size"

instance ATnT Immediate where
    atnt :: Immediate -> Seq String
    atnt = \case
        ImmediateInt n -> singleton $ show n
        ImmediateChar c -> singleton $ show c
        ImmediateLabel l -> singleton l

instance ATnT Operand where
    atnt :: Operand -> Seq String
    atnt = \case
        Immediate imm -> "$" <| atnt imm
        Register reg -> atnt reg
        MemoryDirect imm -> "(" <| (atnt imm |> ")")
        MemoryIndirect offset base index ->
            offset' >< ("(" <| base') >< (index' |> ")")
            where
            offset' = maybe empty atnt offset
            base' = atnt (base, B8)
            index' = case index of
                Just (reg, scale) -> ", " <| atnt reg >< ", " <| singleton (show scale)
                Nothing -> empty

instance ATnT Size where
    atnt :: Size -> Seq String
    atnt = singleton . \case
        B1 -> "b"; B2 -> "w"; B4 -> "l"; B8 -> "q";
        _ -> error "try to get the suffix of non normal size"

instance ATnT Condition where
    atnt :: Condition -> Seq String
    atnt = singleton . \case
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

indent :: Instruction -> Seq String
indent = \case
    Label {} -> empty
    IfDefined {} -> empty
    Define {} -> empty
    EndIf -> empty
    Global {} -> empty
    SectionReadOnly -> empty
    SectionText -> empty
    SectionLiteral4 -> empty
    SectionCString -> empty
    EmptyLine -> empty
    _ -> singleton "    "

instance ATnT Instruction where
    atnt :: Instruction -> Seq String
    atnt s = indent s >< case s of
        Label l -> singleton l |> ":"
        IfDefined l -> "#ifdef " <| singleton l
        EndIf -> singleton "#endif"
        Define l str -> "#define " <| singleton l >< " " <| singleton str

        Move from to -> "mov " <| atnt (from, to)
        MoveSize size from to ->
            "mov" <| atnt size >< " " <| atnt (from, to)
        MoveZeroSizeExtend size size' from to ->
            "movz" <| atnt size >< atnt size' >< " " <| atnt (from, to)
        MoveSignSizeExtend size size' from to ->
            "movs" <| atnt size >< atnt size' >< " " <| atnt (from, to)
        CompareMove c from to ->
            "cmov" <| atnt c >< " " <| atnt (from, to)

        LoadAddress from to -> "lea " <| atnt (from, to)
        Push op -> "push " <| atnt op
        Pop op -> "pop " <| atnt op
        Call op -> "call " <| atnt op
        Return -> singleton "ret"
        Leave -> singleton "leave"
        EmptyLine -> empty
        CLTD -> singleton "cltd"
        Set cond operand -> "set" <| atnt cond >< " " <| atnt operand

        Not op -> "not " <| atnt op
        Xor from to-> "xor " <| atnt (from, to)
        Add from to -> "add "   <| atnt (from, to)
        Subtract from to -> "sub " <| atnt (from, to)
        DivideI a-> "idivl " <| atnt a
        Multiply from to -> "imull " <| atnt (from, to)

        Increase op -> "inc " <| atnt op
        Decrease op -> "dec " <| atnt op

        And from to -> "and " <| atnt (from, to)
        Or from to -> "or " <| atnt (from, to)

        Jump l -> "jmp " <| atnt l
        JumpWhen c l -> "j" <| atnt c >< " " <| atnt l

        Test a b -> "test " <| atnt (a, b)
        Compare a b -> "cmp " <| atnt (a, b)

        Int n -> ".int " <| singleton (show n)
        Global l -> ".globl " <| singleton l
        AsciiZero str -> ".asciz " <| singleton (show str)

        SectionText -> singleton "section_text"
        SectionReadOnly -> singleton "section_read_only"
        SectionLiteral4 -> singleton "section_literal4"
        SectionCString -> singleton "section_cstring"

        Comment c -> "// " <| singleton c

        e -> error $ "ATnT: " ++ show e
