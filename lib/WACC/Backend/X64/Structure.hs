module WACC.Backend.X64.Structure where

import Data.String

import qualified Data.Set as S
import qualified Data.Sequence as Sq
import Data.Sequence (Seq)
import WACC.IR.Structure (Size(..), HasSize (..))

{- A program is made up of a list of instructions. -}
newtype Program = Program {
    dataSegmentsDefinition :: [Instruction]
} deriving Show

{- This is a function to generate move instructions
   between two operands, handling special cases. -}
move :: Size -> Operand -> Operand -> Seq Instruction
move _ from@(Register _) to = return $ Move from to
move _ from@(MemoryIndirect (Just (ImmediateLabel _)) (RIP, B8) Nothing) to =
    return $ LoadAddress from to
move _ from to@(Register _) = return $ Move from to
move size from@(MemoryIndirect {}) to@(MemoryIndirect {}) =
    Sq.fromList [ Move from (Register (RAX, size))
    , Move (Register (RAX, size)) to]
move size from to = return $ MoveSize size from to

data Condition
    = Equal | NotEqual
    | Zero | NotZero
    | Negative | NotNegative
    | Greater | GreaterEqual
    | Less | LessEqual
    | Above | AboveEqual
    | Below | BelowEqual
    deriving (Show)

data Instruction
    = Label String

    | Move Operand Operand
    | MoveSign Operand Operand
    | MoveZero Operand Operand
    | MoveSize Size Operand Operand
    | MoveSignSize Size Operand Operand
    | MoveZeroSize Size Operand Operand

    | MoveZeroSizeExtend Size Size Operand Operand
    | MoveSignSizeExtend Size Size Operand Operand

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

    {- RDX:RAX = RAX * Operand -}
    | MultiplyFullI Operand
    | MultiplyFullU Operand

    {- (RAX, RDX) = (RDX:RAX / Operand, RDX:RAX % Operand) -}
    | DivideI Operand
    | DivideU Operand

    {- Operand 2 - Operand 1 -}
    | Compare Operand Operand
    | CompareMove Condition Operand Operand

    {- Bitwise AND -}
    | Test Operand Operand

    {- Different kinds of Jump instructions. -}
    | Jump Immediate
    | JumpWhen Condition Immediate

    {- Push next instruction's address and jump to it. -}
    | Call Immediate

    {- Pop the return address and jump back to it. -}
    | Return

    {- RSP = RBP; RBP = pop() -}
    | Leave

    {- Null-terminated ASCII string. -}
    | AsciiZero String

    | Int Int
    | Global String

    {- Macro directives -}
    | IfDefined String
    | EndIf
    | Define String String

    -- .section .rodata
    | RodataSection
    | Text

    {- For layout use only, not compiled. -}
    | Comment String
    | EmptyLine
    deriving (Show)

{- Representation of registers and their sizes. -}
type Register = (PhysicalRegister, Size)

{- Operand types for instructions, it can be an immediate value,
   a register, or a memory address. -}
data Operand
    = Immediate Immediate
    | Register Register
    | MemoryDirect Immediate
    | MemoryIndirect {
        offset :: Maybe Immediate,
        base :: Register,
        index :: Maybe (Register, Int)
        }
    deriving (Show)

{- Immediate values can be integers, characters, or labels. -}
data Immediate
    = ImmediateInt Int
    | ImmediateChar Char
    | ImmediateLabel String
    deriving (Show)

data PhysicalRegister
    {- Caller-save registers
    Caller save them before the function call if these registers are used.
    They may be changed after returning from the subroutine. -}
    = RAX | RCX | RDX | RDI | RSI | RSP | R8  | R9  | R10 | R11

    {- Callee-save
    Subroutines save them if these registers are used in the subroutine,
    and pop them before the subroutine return so that the caller can
    still use them. -}
    | RBX | RBP | R12 | R13 | R14 | R15

    | RIP
    deriving (Enum, Eq, Ord, Show)

{- Check if a register is caller-save. -}
isCallerSave :: PhysicalRegister -> Bool
isCallerSave = (`elem` callerSaveRegisters)
callerSaveRegisters :: S.Set PhysicalRegister
callerSaveRegisters = S.fromList
    [RAX, RCX, RDX, RDI, RSI, RSP, R8, R9, R10, R11]

{- Check if a register is callee-save. -}
isCalleeSave :: PhysicalRegister -> Bool
isCalleeSave = (`elem` calleeSaveRegisters)
calleeSaveRegisters :: S.Set PhysicalRegister
calleeSaveRegisters = S.fromList
    [RBX, RBP, R12, R13, R14, R15]

{- Enables using string literals as immediate labels in the assembly code. -}
instance IsString Immediate where
    fromString :: String -> Immediate
    fromString = ImmediateLabel

instance HasSize Operand where
    getSize :: Operand -> Size
    getSize = \case
        Register (_, size) -> size
        _ -> B8
