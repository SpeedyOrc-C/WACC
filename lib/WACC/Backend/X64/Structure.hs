module WACC.Backend.X64.Structure where

import Data.String

import qualified Data.Set as S
import qualified Data.Sequence as Sq
import Data.Sequence (Seq)
import WACC.IR.Structure (Size(..))

newtype Program = Program {
    dataSegmentsDefinition :: [Instruction]
} deriving Show

move :: Size -> Operand -> Operand -> Seq Instruction
move _ from@(Register _) to = return $ Move from to
move _ from to@(Register _) = return $ Move from to
move _ from@(MemoryIndirect (Just (ImmediateLabel {})) (RIP, B8) Nothing) to =
    return $ LoadAddress from to
move size from@(MemoryIndirect {}) to@(MemoryIndirect {}) =
    Sq.fromList [ Move from (Register (RAX, size))
    , Move (Register (RAX, size)) to]
move size from to = return $ MoveSize size from to

data Instruction
    = Label String

    | Move Operand Operand
    | MoveSign Operand Operand
    | MoveZero Operand Operand
    | MoveSize Size Operand Operand
    | MoveSignSize Size Operand Operand
    | MoveZeroSize Size Operand Operand

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

    | Jump Immediate
    | JumpZero Immediate
    | JumpNonZero Immediate
    | JumpNegative Immediate
    | JumpNonNegative Immediate
    | JumpGreater Immediate
    | JumpGreaterEqual Immediate
    | JumpLess Immediate
    | JumpLessEqual Immediate
    | JumpAbove Immediate
    | JumpAboveEqual Immediate
    | JumpBelow Immediate
    | JumpBelowEqual Immediate

    -- Push next instruction's address and jump to it.
    | Call Immediate

    -- Pop the return address and jump back to it.
    | Return

    -- RSP = RBP; RBP = pop()
    | Leave

    | AsciiZero String
    | Int Int
    | Global String

    -- Macro directives
    | IfDefined String
    | EndIf
    | Define String String

    -- For layout use only, not compiled.
    | Comment String
    | EmptyLine
    deriving (Show)

type Register = (PhysicalRegister, Size)

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

isCallerSave :: PhysicalRegister -> Bool
isCallerSave = (`elem` callerSaveRegisters)
callerSaveRegisters :: S.Set PhysicalRegister
callerSaveRegisters = S.fromList
    [RAX, RCX, RDX, RDI, RSI, RSP, R8, R9, R10, R11]

isCalleeSave :: PhysicalRegister -> Bool
isCalleeSave = (`elem` calleeSaveRegisters)
calleeSaveRegisters :: S.Set PhysicalRegister
calleeSaveRegisters = S.fromList
    [RBX, RBP, R12, R13, R14, R15]

instance IsString Immediate where
    fromString :: String -> Immediate
    fromString = ImmediateLabel
