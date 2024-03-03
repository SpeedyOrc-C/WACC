module WACC.Backend.X64.Windows.Config where

import qualified Data.Set as S
import qualified Data.Map as M

import WACC.Backend.X64.Config
import WACC.Backend.X64.Internal
import WACC.Backend.X64.Windows.Internal
import WACC.Backend.X64.Structure (PhysicalRegister(..))

config :: Config
config = Config {
    callerSaveRegisters = S.fromList
        [RAX, RCX, RDX, RSP, R8, R9, R10, R11],
    
    calleeSaveRegisters = S.fromList
        [RBX, RSI, RDI, RBP, R12, R13, R14, R15],

    parameterRegisters =
        [RCX, RDX, R8, R9],
    
    availableRegisters = S.fromList
        [RBX, R12, R13, R14, R15, R10, R11, RSI, RDI, R9, R8, RCX],

    rankRegister = \case
        RBX -> 1; R12 -> 2; R13 -> 3; R14 -> 4
        R15 -> 5; R10 -> 6; R11 -> 7; RSI -> 8
        RDI -> 9; R9 -> 10; R8 -> 11; RCX -> 12
        r -> error $ "Cannot use register " ++ show r ++ " in Windows.",

    minSizeOfReservedStackForCallee = 32,
    
    internalFunctions = M.fromList
        [ (PrintString, printString)
        , (PrintLineBreak, printLineBreak)
        , (PrintInt, printInt)
        , (PrintChar, printChar)
        , (PrintBool, printBool)
        , (ErrorOverflow, errorOverflow)
        ]
}