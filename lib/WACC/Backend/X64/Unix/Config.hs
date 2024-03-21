module WACC.Backend.X64.Unix.Config where

import qualified Data.Set as S
import qualified Data.Map as M

import WACC.Backend.X64.Config
import WACC.Backend.X64.Internal
import WACC.Backend.X64.Unix.Internal
import WACC.Backend.X64.Structure (PhysicalRegister(..), Instruction (..))

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
        r -> error $ "Cannot use register " ++ show r ++ " in Unix.",

    minSizeOfReservedStackForCallee = 0,

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
        ],

    macro = [
        IfDefined "__APPLE__",
            Define "section_read_only" "",
            Define "section_literal4" "",
            Define "section_cstring" "",
            Define "section_text" ".text",

            Define "fflush"  "_fflush",
            Define "write"   "_write",
            Define "printf"  "_printf",
            Define "scanf"   "_scanf",
            Define "exit"    "_exit",
            Define "malloc"  "_malloc",
            Define "putchar" "_putchar",
            Define "getchar" "_getchar",
            Define "free" "_free",

            Global "_main",
            Define "main" "_main",
        EndIf,

        EmptyLine,

        IfDefined "__linux__",
            Define "section_read_only" ".section .rodata",
            Define "section_literal4" "",
            Define "section_cstring" "",
            Define "section_text" ".text",

            Define "fflush" "fflush@PLT",
            Define "write" "write@PLT",
            Define "printf" "printf@PLT",
            Define "scanf" "scanf@PLT",
            Define "exit" "exit@PLT",
            Define "free" "free@PLT",
            Define "malloc" "malloc@PLT",
            Define "putchar" "putchar@PLT",
            Define "getchar" "getchar@PLT",

            Global "main",
        EndIf
    ]
}
