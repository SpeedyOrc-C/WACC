module WACC.Backend.X64.Unix.Config where

import qualified Data.Set as S
import qualified Data.Map as M

import WACC.Backend.X64.Config
import WACC.Backend.X64.Internal
import WACC.Backend.X64.Unix.Internal
import WACC.Backend.X64.Structure (PhysicalRegister(..))

config :: Config
config = Config {
    callerSaveRegisters = S.fromList
        [RAX, RCX, RDX, RDI, RSI, RSP, R8, R9, R10, R11],

    calleeSaveRegisters = S.fromList
        [RBX, RBP, R12, R13, R14, R15],

    parameterRegisters =
        [RDI, RSI, RDX, RCX, R8, R9],

    availableRegisters = S.fromList
        [RBX, R12, R13, R14, R15, R10, R11, R9, R8, RCX, RSI, RDI],

    rankRegister = \case
        RBX -> 1; R12 -> 2;  R13 -> 3;  R14 -> 4
        R15 -> 5; R10 -> 6;  R11 -> 7;  R9  -> 8
        R8  -> 9; RCX -> 10; RSI -> 11; RDI -> 12
        r -> error $ "Cannot use register " ++ show r,

    internalFunctions = M.fromList
        [ (PrintString, printString)
        , (PrintChar, printChar)
        , (PrintInt, printInt)
        , (PrintBool, printBool)
        , (PrintLineBreak, printLineBreak)
        , (PrintPointer, printPointer)
        , (ErrorOutOfMemory, errorOutOfMemory)
        , (ErrorNull, errorNull)
        , (ErrorDivideZero, errorDivideZero)
        , (ErrorOverflow, errorOverFlow)
        , (ErrorBadChar, errorBadChar)
        , (ErrorOutOfBounds, errorOutOfBounds)
        , (SeekArrayElement1, seekArrayElement1)
        , (SeekArrayElement4, seekArrayElement4)
        , (SeekArrayElement8, seekArrayElement8)
        , (ReadInt, readInt)
        , (ReadChar, readChar)
        ]
}
