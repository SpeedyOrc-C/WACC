module WACC.Backend.X64.Windows.Internal where

import qualified Data.Sequence as Sq
import Data.Sequence ((><))
import WACC.Backend.X64.Structure
import WACC.IR.Structure (Size(..))

string :: String -> String -> Sq.Seq Instruction
string name s = Sq.fromList [Label name, AsciiZero s]

f :: String -> [Instruction] -> Sq.Seq Instruction
f name statements =
    Sq.fromList
        [ Label name
        , Push (Register (RBP, B8))
        , Move (Register (RSP, B8)) (Register (RBP, B8))
        ] ><
    Sq.fromList statements ><
    Sq.fromList [Leave, Return]

reserveStack :: Instruction
reserveStack = Subtract (Immediate $ ImmediateInt 32) (Register (RSP, B8))

releaseStack :: Instruction
releaseStack = Add (Immediate $ ImmediateInt 32) (Register (RSP, B8))

printChar :: Sq.Seq Instruction
printChar =
    string "format_char" "%c" ><
    f "print_char" [
        Move (Register (RCX, B4)) (Register (RDX, B4)),
        leaLabel "format_char" (Register (RCX, B8)),
        reserveStack,
        Call "printf",
        Move (Immediate $ ImmediateInt 0) (Register (RCX, B4)),
        Call "fflush",
        releaseStack
    ]

printBool :: Sq.Seq Instruction
printBool =
    string "bool_true" "true" ><
    string "bool_false" "false" ><
    f "print_bool" [
        Compare (Immediate $ ImmediateInt 0) (Register (RCX, B1)),
        JumpWhen Zero "print_bool_else",
        leaLabel "bool_true" (Register (RDX, B8)),
        Move (Immediate $ ImmediateInt 4) (Register (R8, B4)),
        Jump "print_bool_fi",
        Label "print_bool_else",
        leaLabel "bool_false" (Register (RDX, B8)),
        Move (Immediate $ ImmediateInt 5) (Register (R8, B4)),
        Label "print_bool_fi",

        Move (Immediate $ ImmediateInt 1) (Register (RCX, B8)),
        reserveStack,
        Call "write",
        releaseStack
    ]

printInt :: Sq.Seq Instruction
printInt = string "format_int" "%d" >< f "print_int" [
    Move (Register (RCX, B4)) (Register (RDX, B4)),
    leaLabel "format_int" (Register (RCX, B8)),
    reserveStack,
    Call "printf",
    Move (Immediate $ ImmediateInt 0) (Register (RCX, B4)),
    Call "fflush",
    releaseStack
    ]

printString :: Sq.Seq Instruction
printString = f "print_string" [
    Move (Register (RCX, B8)) (Register (RAX, B8)),

    Move (Immediate $ ImmediateInt 1) (Register (RCX, B4)),
    Move (Register (RAX, B8)) (Register (RDX, B8)),
    Move (MemoryIndirect (Just $ ImmediateInt (-4)) RAX Nothing) (Register (R8, B4)),
    reserveStack,
    Call "write",
    releaseStack
    ]

printLineBreak :: Sq.Seq Instruction
printLineBreak = string "line_break" "\r\n" >< f "print_line_break" [
    Move (Immediate $ ImmediateInt 1) (Register (RCX, B4)),
    leaLabel "line_break" (Register (RDX, B8)),
    Move (Immediate $ ImmediateInt 2) (Register (R8, B4)),
    reserveStack,
    Call "write",
    releaseStack
    ]

runtimeError :: String -> String -> Sq.Seq Instruction
runtimeError name message = Sq.fromList
    [ Label $ "message_" ++ name
    , AsciiZero message

    , Label $ "error_" ++ name
    , Push (Register (RBP, B8))
    , Move (Register (RSP, B8)) (Register (RBP, B8))

    , Move (Immediate $ ImmediateInt 1) (Register (RCX, B4))
    , leaLabel ("message_" ++ name) (Register (RDX, B8))
    , Move (Immediate $ ImmediateInt (length message)) (Register (R8, B4))
    , reserveStack
    , Call "write"

    , Move (Immediate $ ImmediateInt (-1)) (Register (RCX, B1))
    , Call "exit"
    ]

errorOverflow :: Sq.Seq Instruction
errorOverflow = runtimeError
    "overflow"
    "An integer overflowed or underflowed."
