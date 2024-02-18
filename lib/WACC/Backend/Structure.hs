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
    unusedRegersters :: [HalfRegister],
    simbolTables :: [String `Map` Operand]
}