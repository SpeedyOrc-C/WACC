module WACC.Backend.X64.Structure where

import Data.String

import qualified Data.Maybe as M
import qualified Data.Sequence as Sq
import qualified Data.List as L
import Data.Sequence (Seq)
import WACC.IR.Structure (Size(..), HasSize (..))
import qualified WACC.IR.Structure as IR

containRAX :: Operand -> Bool
containRAX (Register (RAX, _)) = True
containRAX (MemoryIndirect _ RAX _) = True
containRAX _ = False

containRegister :: Operand -> Maybe PhysicalRegister
containRegister (Register (r, _)) = Just r
containRegister (MemoryIndirect _ r _) = Just r
containRegister _ = Nothing

pushedRegister :: PhysicalRegister -> Operand -> Operand
pushedRegister _ x@(MemoryIndirect _ RSP _) = addToIndirect x 8
pushedRegister r (Register (RAX, s)) = Register (r, s)
pushedRegister r (MemoryIndirect x RAX y) = MemoryIndirect x r y
pushedRegister _ x = x

{- A program is made up of a list of instructions. -}
newtype Program = Program {
    dataSegmentsDefinition :: [Instruction]
} deriving Show

stringsToInstructions :: [(String, String)] -> Seq Instruction
stringsToInstructions strs = SectionReadOnly Sq.<|
        foldl (Sq.><) Sq.Empty [Sq.fromList
            [Int (length str),
            Label label,
            AsciiZero str] |(label, str) <- strs]

stringAddress :: String -> Operand
stringAddress str
    = MemoryIndirect (Just (ImmediateLabel str)) RIP Nothing

leaLabel :: String -> Operand -> Instruction
leaLabel label = LoadAddress (MemoryIndirect (Just (ImmediateLabel label)) RIP Nothing)

{- This is a function to generate a function definition. -}

{- This is a function to generate move instructions
   between two operands, handling special cases. -}

addToIndirect :: Operand -> Int -> Operand
addToIndirect (MemoryIndirect Nothing x y) z 
    = MemoryIndirect (Just $ ImmediateInt z) x y
addToIndirect (MemoryIndirect (Just (ImmediateInt h)) x y) z 
    = MemoryIndirect (Just $ ImmediateInt (h+z)) x y
addToIndirect (Register (reg, _)) z
    = MemoryIndirect (Just $ ImmediateInt z) reg Nothing
addToIndirect _ _
    = error "the address of struct must be in \
        \either register or indirect address"

move :: Size -> Operand -> Operand -> Seq Instruction
move (B size) from to
    | containRAX from || containRAX to =
        (Push (Register (regist, B8))
        Sq.<|
        (move B8 (Register (RAX,B8)) (Register (regist, B8)) Sq.><
        Sq.fromList (concat 
            [[ Move (addToIndirect from' off) (Register (RAX, x))
                , Move (Register (RAX, x)) (addToIndirect to' off)]
            | (x, off) <- sizes `zip` scanl (+) 0 (map IR.sizeToInt sizes)]) Sq.><
        move B8 (Register (regist, B8))(Register (RAX,B8))))
        Sq.|>
        Pop (Register (regist, B8))
    | otherwise = 
        Sq.fromList (concat 
            [[ Move (addToIndirect from off) (Register (RAX, x))
                , Move (Register (RAX, x)) (addToIndirect to off)]
            | (x, off) <- sizes `zip` scanl (+) 0 (map IR.sizeToInt sizes)])
    where 
        (n, m) = size `divMod` 8
        n' = n + if m == 0 then 0 else 1
        sizes = replicate (n' - 1) B8 ++ getSizeHelper (size - (n' - 1) * 8) 8
        intToSize     :: Int -> Size
        intToSize 1 = B1
        intToSize 2 = B2
        intToSize 4 = B4
        intToSize 8 = B8
        intToSize x = B x
        getSizeHelper :: Int -> Int -> [Size]
        getSizeHelper _ 0 
            = error "should pass in size of smaller than 8 bytes in"
        getSizeHelper 0 _
            = []
        getSizeHelper x s
            |x >= s = intToSize s: getSizeHelper (x - s) (s `div` 2)
            |otherwise = getSizeHelper x (s `div` 2)
        regist = head ([RDX, R10, R11] 
            L.\\ M.catMaybes (containRegister from:[containRegister to]))
        from' = pushedRegister regist from
        to' = pushedRegister regist to
            


move _ from@(Register _) to = return $ Move from to
move _ from@(MemoryIndirect (Just (ImmediateLabel _)) RIP Nothing) to =
    loadAddress from to
move _ from to@(Register _) = return $ Move from to

move size from@(MemoryIndirect {}) to@(MemoryIndirect {}) =
    Sq.fromList [ Move from (Register (RAX, size))
    , Move (Register (RAX, size)) to]
move _ _ (Immediate {}) = error "cannot have Immediate at right parameter"
move size from to = return $ MoveSize size from to

moveSign :: Operand -> Operand -> Instruction
moveSign _ (Immediate {}) = error "cannot have Immediate at right parameter"
moveSign op op2 = MoveSign op op2
moveZero :: Operand -> Operand -> Instruction
moveZero _ (Immediate {}) = error "cannot have Immediate at right parameter"
moveZero op op2 = MoveZero op op2
moveSize :: Size -> Operand -> Operand -> Instruction
moveSize _ _ (Immediate {}) = error "cannot have Immediate at right parameter"
moveSize size op op2 = MoveSize size op op2

moveSizeSign :: t1 -> t2 -> Operand -> a
moveSizeSign _ _ (Immediate {}) = error "cannot have Immediate at right parameter"
moveSizeSign size op op2 = moveSizeSign size op op2

moveZeroSign :: t1 -> t2 -> Operand -> a
moveZeroSign _ _ (Immediate {}) = error "cannot have Immediate at right parameter"
moveZeroSign size op op2 = moveZeroSign size op op2

moveZeroSizeExtend :: p1 -> p2 -> p3 -> Operand -> a
moveZeroSizeExtend _ _ _ (Immediate {}) = error "cannot have Immediate at right parameter"
moveZeroSizeExtend s1 s2 o1 o2 = moveZeroSizeExtend s1 s2 o1 o2

moveSignSizeExtend :: Size -> Size -> Operand -> Operand -> Instruction
moveSignSizeExtend _ _ _ (Immediate {}) = error "cannot have Immediate at right parameter"
moveSignSizeExtend s1 s2 o1 o2 = MoveSignSizeExtend s1 s2 o1 o2
set :: Condition -> Operand -> Instruction
set _  (Immediate {}) = error "cannot have Immediate at right parameter"
set cond operat = Set cond operat

add' :: Operand -> Operand -> Instruction
add' _ (Immediate {}) = error "cannot have Immediate at right parameter"
add' op op2 = Add op op2

subtract' :: Operand -> Operand -> Instruction
subtract' _ (Immediate {}) = error "cannot have Immediate at right parameter"
subtract' op op2 = Subtract op op2

multiply' :: Operand -> Operand -> Instruction
multiply' _ (Immediate {}) = error "cannot have Immediate at right parameter"
multiply' op op2 = Multiply op op2

xor' :: Operand -> Operand -> Instruction
xor' _ (Immediate {}) = error "cannot have Immediate at right parameter"
xor' op op2 = Xor op op2

or' :: Operand -> Operand -> Instruction
or' _ (Immediate {}) = error "cannot have Immediate at right parameter"
or' op op2 = Or op op2

and' :: Operand -> Operand -> Instruction
and' _ (Immediate {}) = error "cannot have Immediate at right parameter"
and' op op2 = And op op2

test' :: Operand -> Operand -> Instruction
test' _ (Immediate {}) = error "cannot have Immediate at right parameter"
test' op op2 = Test op op2

compareMove :: Condition -> Operand -> Operand -> Instruction
compareMove _ _ (Immediate {}) = error "cannot have Immediate at right parameter"
compareMove cond op1 op2 = CompareMove cond op1 op2

loadAddress :: Operand -> Operand -> Seq Instruction
loadAddress from to@(MemoryIndirect {})
    = Sq.fromList [ LoadAddress from (Register (RAX, B8))
    , Move (Register (RAX, B8)) to]
loadAddress _ (Immediate {}) = error "cannot have Immediate at right parameter"
loadAddress from to = Sq.singleton $ LoadAddress from to

data Condition
    = Equal | NotEqual
    | Zero | NotZero
    | Negative | NotNegative
    | Greater | GreaterEqual
    | Less | LessEqual
    | Above | AboveEqual
    | Below | BelowEqual
    | Overflow
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

    | Increase Operand
    | Decrease Operand
    | Not Operand
    | Set Condition Operand

    | CLTD
    | LoadAddress Operand Operand
    | Add Operand Operand
    | Subtract Operand Operand
    | Multiply Operand Operand
    | Xor Operand Operand
    | Or Operand Operand
    | And Operand Operand

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

    | SectionReadOnly
    | SectionText
    | SectionLiteral4
    | SectionCString

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
        base :: PhysicalRegister,
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

instance IsString Operand where
    fromString :: String -> Operand
    fromString = Immediate . fromString

instance IsString Immediate where
    fromString :: String -> Immediate
    fromString = ImmediateLabel

instance HasSize Operand where
    getSize :: Operand -> Size
    getSize = \case
        Register (_, size) -> size
        _ -> B8
