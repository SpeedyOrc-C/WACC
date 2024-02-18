module WACC.Backend.X64.Structure where

data Program = Program [Instruction]

data Instruction
    = Label String

    | Move Operand Operand
    | MoveSign Operand Operand
    | MoveZero Operand Operand

    | Push Operand
    | Pop Operand

    | ExtendB2ToB4 Operand
    | ExtendB4ToB8 Operand
    | ExtendB8ToB16 Operand

    | Increase Operand
    | Decrease Operand
    | Negate Operand
    | Not Operand

    | LoadAddress Operand Operand
    | Add Operand Operand
    | Subtract Operand Operand
    | Multiply Operand Operand
    | Xor Operand Operand
    | Or Operand Operand
    | And Operand Operand

    | ShiftLeft Int Operand
    | ShiftRight Int Operand
    | ShiftRightArithmetic Int Operand

    -- RDX:RAX = RAX * Operand
    | MultiplyFullI Operand
    | MultiplyFullU Operand

    -- (RAX, RDX) = (RDX:RAX / Operand, RDX:RAX % Operand)
    | DivideI Operand
    | DivideU Operand

    -- Operand 2 - Operand 1
    | Compare Operand Operand
    -- Bitwise AND
    | Test Operand Operand

    | Jump Operand
    | JumpZero Operand
    | JumpNonZero Operand
    | JumpNegative Operand
    | JumpNonNegative Operand
    | JumpGreater Operand
    | JumpGreaterEqual Operand
    | JumpLess Operand
    | JumpLessEqual Operand
    | JumpAbove Operand
    | JumpAboveEqual Operand
    | JumpBelow Operand
    | JumpBelowEqual Operand

    -- Push next instruction's address and jump to it.
    | Call Operand

    -- Pop the return address and jump back to it.
    | Return

    -- RSP = RBP; RBP = pop()
    | Leave

data Operand
    = Immediate Immediate
    | Register Register
    | MemoryDirect Immediate
    | MemoryIndirect {
        offset :: Maybe Int,
        base :: Register,
        index :: Maybe (Register, Int)
        }
    | LabelAddress String

data Immediate
    = ImmediateInt Int
    | ImmediateChar Char

data Register
    {- Caller-save registers
    Caller save them before the function call if these registers are used.
    They may be changed after returning from the subroutine. -}
    = RAX | EAX  | AX   | AL
    | RCX | ECX  | CX   | CL
    | RDX | EDX  | DX   | DL
    | RDI | EDI  | DI   | DIL
    | RSI | ESI  | SI   | SIL
    | RSP | ESP  | SP   | SPL
    | R8  | R8D  | R8W  | R8B
    | R9  | R9D  | R9W  | R9B
    | R10 | R10D | R10W | R10B
    | R11 | R11D | R11W | R11B

    {- Callee-save
    Subroutines save them if these registers are used in the subroutine,
    and pop them before the subroutine return so that the caller can
    still use them. -}
    | RBX | EBX  | BX   | BL
    | RBP | EBP  | BP   | BPL
    | R12 | R12D | R12W | R12B
    | R13 | R13D | R13W | R13B
    | R14 | R14D | R14W | R14B
    | R15 | R15D | R15W | R15B

data Size = B8 | B4 | B2 | B1

parameter1 :: Size -> Register
parameter1 = \case B8 -> RDI; B4 -> EDI; B2 -> DI; B1 -> DIL

parameter2 :: Size -> Register
parameter2 = \case B8 -> RSI; B4 -> ESI; B2 -> SI; B1 -> SIL

parameter3 :: Size -> Register
parameter3 = \case B8 -> RDX; B4 -> EDX; B2 -> DX; B1 -> DL

parameter4 :: Size -> Register
parameter4 = \case B8 -> RCX; B4 -> ECX; B2 -> CX; B1 -> CL

parameter5 :: Size -> Register
parameter5 = \case B8 -> R8; B4 -> R8D; B2 -> R8W; B1 -> R8B

parameter6 :: Size -> Register
parameter6 = \case B8 -> R9; B4 -> R9D; B2 -> R9W; B1 -> R9B

returnValue :: Size -> Register
returnValue = \case B8 -> RAX; B4 -> EAX; B2 -> AX; B1 -> AL

stackPointer :: Size -> Register
stackPointer = \case B8 -> RSP; B4 -> ESP; B2 -> SP; B1 -> SPL

instance Num Immediate where
    fromInteger :: Integer -> Immediate
    fromInteger = ImmediateInt . fromInteger
