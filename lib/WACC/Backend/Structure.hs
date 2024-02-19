module WACC.Backend.Structure where

import Data.Map (Map)
data Length = EightBytes 
             | FourBytes 
             | TwoBytes 
             | OneBytes
            deriving Eq
data HalfRegister = RAX 
              | RBX 
              | RCX 
              | RDX 
              | RSI 
              | RDI 
              | RDP 
              | RSP
              | RBP
              | R8 
              | R9
              | R10 
              | R11
              | R12
              | R13
              | R14
              | R15
            deriving Eq
data Register 
    = Reg HalfRegister Length
        deriving Eq
type Pointer = (Int, (Register, Register, Int))
data Operand = Register Register 
             | ImmNum Int 
             | Address Pointer Length
            deriving Eq

data Commands =
      ADD Operand
    | SUB Operand
    | IMUL Operand Operand
    | IDIV Operand
    | MOVSX Operand Operand
    | CDQ
    | CMP Operand Operand
    | LEA Operand Operand
    | POP Operand
    | CALL String
    | RET
    | JMP String
    | JC String
    | JO String
    | JLE String
    | JE String
    | JGE String
    | JG String
    | JL String
    | JB String
    | JBE String
    | JA String
    | JAE String

data Status = Status{
    unusedCallerSavedRegisters :: [HalfRegister],
    unusedCalleeSavedRegisters :: [HalfRegister],
    simbolTables :: [String `Map` Operand]
}


data AppendList a = Aempty | Acons (AppendList a) a

toList :: AppendList a -> [a]
toList = flip toListHelper []
    where 
        toListHelper :: AppendList a -> [a] -> [a]
        toListHelper Aempty xs = xs
        toListHelper (Acons xs x) ys = toListHelper xs (x:ys)

fromList :: [a] -> AppendList a
fromList [] = Aempty
fromList xs = Acons (fromList (init xs)) (last xs)

empty :: AppendList a
empty = Aempty

append :: AppendList a -> a -> AppendList a
append = Acons