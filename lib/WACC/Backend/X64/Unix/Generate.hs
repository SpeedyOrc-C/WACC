module WACC.Backend.X64.Unix.Generate where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.List
import           Data.Function
import           Data.Traversable
import           Control.Monad.Trans.State.Lazy

import qualified WACC.IR.Structure as IR
import           WACC.IR.Structure (Size(..), Identifier)
import           WACC.Backend.X64.Structure
import           WACC.Backend.StackPool

data MemoryLocation
    = AtRegister Register
    | AtStack Int Size
    | AtParameterStack Int Size
    deriving (Show)

rankRegister :: PhysicalRegister -> Int
rankRegister = \case
    RBX -> 1; R12 -> 2;  R13 -> 3;  R14 -> 4
    R15 -> 5; R10 -> 6;  R11 -> 7;  R9  -> 8
    R8  -> 9; RCX -> 10; RSI -> 11; RDI -> 12

    r -> error $ "Cannot use register " ++ show r

registers :: S.Set PhysicalRegister
registers = S.fromList
    [RBX, R12, R13, R14, R15, R10, R11, R9, R8, RCX, RSI, RDI]

parameter :: Int -> Size -> Operand
parameter n size = case n of
    1 -> Register (RDI, size)
    2 -> Register (RSI, size)
    3 -> Register (RDX, size)
    4 -> Register (RCX, size)
    6 -> Register (R9, size)
    5 -> Register (R8, size)
    _ -> error $ "Cannot use parameter " ++ show n

data GeneratorState = GeneratorState {
    memoryTable :: M.Map Identifier MemoryLocation,
    registerPool :: S.Set PhysicalRegister,
    stackPool :: [StackSegment Identifier]
} deriving (Show)

allocate :: Identifier -> Size -> State GeneratorState MemoryLocation
allocate var size = do
    registerPool <- gets registerPool
    memoryTable <- gets memoryTable
    stackPool <- gets stackPool

    if S.size registerPool < S.size registers then do
        -- There are free registers.
        let freeRegisters = registers S.\\ registerPool
        let freeRegister = minimumBy (compare `on` rankRegister) $ S.toList freeRegisters

        let location = AtRegister (freeRegister, size)

        modify $ \s -> s {
            registerPool = S.insert freeRegister registerPool,
            memoryTable = M.insert var location memoryTable
        }

        return location

    else do
        -- All registers are in use, allocate on the stack.
        let (offset, stackPool') = allocateStack (IR.sizeToInt size) var stackPool
        let location = AtStack offset size

        modify $ \s -> s {
            stackPool = stackPool',
            memoryTable = M.insert var location memoryTable
        }

        return location

free :: Identifier -> State GeneratorState ()
free name = do
    memoryTable <- gets memoryTable
    stackPool <- gets stackPool
    registerPool <- gets registerPool

    case memoryTable M.! name of
        AtRegister reg@(physicalRegister, _) -> do
            modify $ \s -> s {
                registerPool = S.delete physicalRegister registerPool,
                memoryTable = M.delete name memoryTable
            }

        AtStack offset _ -> do
            modify $ \s -> s {
                stackPool = freeStack offset stackPool,
                memoryTable = M.delete name memoryTable
            }

        _ -> return ()

operandFromMemoryLocation :: MemoryLocation -> Operand
operandFromMemoryLocation = \case
    AtRegister register ->
        Register register
    AtStack offset _ ->
        MemoryIndirect (Just (ImmediateInt offset)) (RSP, B8) Nothing
    AtParameterStack offset _ ->
        MemoryIndirect (Just (ImmediateInt offset)) (RBP, B8) Nothing

scalar :: IR.Scalar -> State GeneratorState Operand
scalar = \case
    IR.Immediate n -> return $
        Immediate (ImmediateInt n)

    IR.Variable name -> do
        memoryTable <- gets memoryTable
        return $ operandFromMemoryLocation $ memoryTable M.! name

    IR.String n ->
        return $ MemoryIndirect (Just $ ImmediateLabel ("str." ++ show n)) (RIP, B8) Nothing

expression :: IR.Expression -> State GeneratorState (Operand, [Instruction])
expression = \case
    IR.Scalar s -> do
        op <- scalar s
        return (op, [])


singleStatement :: IR.SingleStatement -> State GeneratorState [Instruction]
singleStatement = \case
    IR.Exit s -> do
        op <- scalar s
        return [Move op (parameter 1 B4), Call "exit"]

    IR.Assign size to from -> do
        memoryTable <- gets memoryTable
        (op, evaluate) <- expression from

        case memoryTable M.!? to of
            Just location -> do
                return $ evaluate ++
                    [Move op (operandFromMemoryLocation location)]

    IR.PrintString s -> do
        op <- scalar s
        registerPool <- gets registerPool
        return $
            if RDI `elem` S.elems registerPool then
                [ Push (Register (RDI, B8))
                , LoadAddress op (parameter 1 B8)
                , Call "print_string"
                , Pop (Register (RDI, B8))]
            else
                [ LoadAddress op (parameter 1 B8)
                , Call "print_string"]

    e -> error $ "Not implemented: " ++ show e

instruction :: IR.NoControlFlowStatement -> State GeneratorState [Instruction]
instruction = \case
    IR.NCF statement -> singleStatement statement

    IR.Label name ->
        return [Label name]

    IR.Goto name ->
        return [Jump (Immediate (ImmediateLabel name))]

instructions :: [IR.NoControlFlowStatement] -> State GeneratorState [Instruction]
instructions xs = concat <$> traverse instruction xs

function :: IR.Function IR.NoControlFlowStatement -> State GeneratorState [Instruction]
function (IR.Function name parameters statements) = do
    oldState <- get

    ss <- instructions statements

    put oldState

    return $
        [ Label name
        , Push (Register (RBP, B8))
        , Move (Register (RSP, B8)) (Register (RBP, B8))] ++

        ss ++

        [ Leave
        , Return]

macro :: [Instruction]
macro = [
    IfDefined "__APPLE__",
        Define "fflush" "_fflush",
        Define "write"  "_write",
        Define "printf" "_printf",
        Define "exit"   "_exit",
        Global "_main",
        Define "main" "_main",
    EndIf,
    EmptyLine,
    IfDefined "__linux__",
        Define "flush" "fflush@PLT",
        Define "write" "write@PLT",
        Define "printf" "printf@PLT",
        Define "exit" "exit@PLT",
        Global "main",
    EndIf
    ]
{-
print_string:
    push %rbp
    mov %rsp, %rbp

    mov %rdi, %rax

    mov $1, %edi
    lea 4(%rax), %rsi
    mov (%rax), %edx
    call write

    leave
    ret
-}
innerPrintString :: [Instruction]
innerPrintString = [
    Label "print_string",
    Push (Register (RBP, B8)),
    Move (Register (RSP, B8)) (Register (RBP, B8)),
    Move (Register (RDI, B8)) (Register (RAX, B8)),

    Move (Immediate $ ImmediateInt 1) (Register (RDI, B4)),
    LoadAddress (MemoryIndirect (Just (ImmediateInt 4)) (RAX, B8) Nothing) (Register (RSI, B8)),
    Move (MemoryIndirect Nothing (RAX, B8) Nothing) (Register (RDX, B4)),
    Call "write",

    Leave,
    Return
    ]

program :: IR.Program IR.NoControlFlowStatement -> State GeneratorState [Instruction]
program (IR.Program dataSegment functions) = do
    functions' <- traverse function functions

    dataSegment' <-
        for (M.toList dataSegment) $ \(name, number) -> do
            return
                [ Label ("str." ++ show number)
                , Int (length name + 1)
                , AsciiZero name]

    return $
        macro ++ [EmptyLine] ++
        intercalate [EmptyLine] (functions' ++ [innerPrintString]) ++ [EmptyLine] ++
        concat dataSegment'

generateX64 :: IR.Program IR.NoControlFlowStatement -> [Instruction]
generateX64 p = evalState (program p) (GeneratorState M.empty S.empty [])
