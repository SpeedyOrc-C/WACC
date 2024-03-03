module WACC.Backend.X64.Windows.Internal where

import qualified Data.Sequence as Sq
import Data.Sequence ((<|), (|>), (><))
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
        , Subtract (Immediate $ ImmediateInt 32) (Register (RSP, B8))
        ] ><
    Sq.fromList statements ><
    Sq.fromList [Leave, Return]

printString :: Sq.Seq Instruction
printString = f "print_string" [
    Move (Register (RCX, B8)) (Register (RAX, B8)),

    Move (Immediate $ ImmediateInt 1) (Register (RCX, B4)),
    Move (Register (RAX, B8)) (Register (RDX, B8)),
    Move (MemoryIndirect (Just $ ImmediateInt (-4)) (RAX, B8) Nothing) (Register (R8, B4)),
    Call "write"
    ]

printLineBreak :: Sq.Seq Instruction
printLineBreak = string "line_break" "\r\n" >< f "print_line_break" [
    Move (Immediate $ ImmediateInt 1) (Register (RCX, B4)),
    LoadAddress (MemoryIndirect (Just "line_break") (RIP, B8) Nothing) (Register (RDX, B8)),
    Move (Immediate $ ImmediateInt 2) (Register (R8, B4)),
    Call "write"
    ]
