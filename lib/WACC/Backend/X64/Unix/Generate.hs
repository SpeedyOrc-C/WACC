module WACC.Backend.X64.Unix.Generate where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Sq
import Data.Sequence((|>), (<|), (><), Seq((:|>)))
import           Data.List
import           Data.Foldable
import           Data.Function
import           Data.Traversable
import           Control.Monad.Trans.State.Lazy

import qualified WACC.IR.Structure as IR
import           WACC.IR.Structure (Size(..), Identifier)
import           WACC.Backend.X64.Structure
import           WACC.Backend.StackPool
import Data.Functor

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

parameterList = [RDI, RSI, RDX, RCX, R9, R8]

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
        AtRegister (physicalRegister, _) -> do
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

    IR.Variable identifier@(IR.Temporary {}) -> do
        memoryTable <- gets memoryTable
        free identifier
        return $ operandFromMemoryLocation $ memoryTable M.! identifier

    IR.Variable identifier -> do
        memoryTable <- gets memoryTable
        return $ operandFromMemoryLocation $ memoryTable M.! identifier

    IR.String n ->
        return $ MemoryIndirect (Just $ ImmediateLabel ("str." ++ show n)) (RIP, B8) Nothing

usedCallerSaveRegisters :: State GeneratorState ([PhysicalRegister], [PhysicalRegister])
usedCallerSaveRegisters = do
    registerPool <- gets registerPool
    let used = registerPool `S.intersection` callerSaveRegisters
    let usedParam = used `S.intersection` S.fromList parameterList

    let usedParamRegs = S.toList usedParam
    let usedNonParamRegs = S.toList (used S.\\ usedParam)

    return (usedParamRegs, usedNonParamRegs)

pushRegisters :: [PhysicalRegister] -> (Seq Instruction, Seq Instruction)
pushRegisters pushed =
    ( Sq.fromList [Push (Register (x, B8)) | x <- pushed]
    , Sq.fromList [Pop (Register (x, B8)) | x <- reverse pushed])

moveESP :: Int -> (Seq Instruction, Seq Instruction)
moveESP x
    | x <= 0 = (Sq.empty, Sq.empty)
    | otherwise = (
        Sq.singleton (Subtract
            (Immediate $ ImmediateInt x)
            (Register (RSP, B8))),
        Sq.singleton (Add
            (Immediate $ ImmediateInt x)
            (Register (RSP, B8))))
expression :: IR.Expression -> State GeneratorState (Operand, Seq Instruction)
expression = \case
    IR.Scalar s -> do
        op <- scalar s
        return (op, Sq.empty)

    IR.Add a b -> do
        a' <- scalar a
        b' <- scalar b
        return
            ( Register (RAX, B4)
            , Sq.fromList
              [ Move a' (Register (RAX, B4))
              , Add b' (Register (RAX, B4))])

    IR.Call size name scalarsWithSize@(unzip -> (sizes, scalars)) -> do
        memoryTable <- gets memoryTable
        (usedParamRegs, usedNonParamRegs) <- usedCallerSaveRegisters

        let (_, unzip -> (stackScalarsSizes, stackScalars)) =
                splitAt 6 scalarsWithSize

        let pushedParamsOver6Size = sum (IR.sizeToInt <$> stackScalarsSizes)

        let pushedOffset =
                8 * (length usedParamRegs + length usedNonParamRegs)
                + pushedParamsOver6Size

        let usedParamRegsWithIndices = zip usedParamRegs
                [pushedParamsOver6Size, pushedParamsOver6Size + 8..]

        scalars' <- for scalars $ \case
            IR.Variable identifier -> return $
                case memoryTable M.! identifier of
                    AtStack offset _ ->
                        MemoryIndirect
                            (Just $ ImmediateInt (offset + pushedOffset))
                            (RSP, B8)
                            Nothing

                    AtRegister (reg, size) ->
                        case lookup reg usedParamRegsWithIndices of
                            Nothing -> Register (reg, size)
                            Just offset ->
                                MemoryIndirect
                                    (Just $ ImmediateInt offset)
                                    (RSP, B8)
                                    Nothing

                    location -> operandFromMemoryLocation location

            s -> scalar s

        let (operandsToBeMoved, operandsToBePushed) = splitAt 6 scalars'

        let allToBePushed =
                usedNonParamRegs ++
                reverse usedParamRegs

        let (pushes, pops) = pushRegisters allToBePushed
            (minuses, adds) = moveESP pushedParamsOver6Size
            assignParameter' :: Int -> [(Size, Operand)] -> Seq Instruction
            assignParameter' _ [] = Sq.empty
            assignParameter' offset ((size, op):ops) =
                move size op
                    (MemoryIndirect
                        (Just (ImmediateInt offset'))
                        (RSP, B8)
                        Nothing) ><
                assignParameter' offset' ops
                where
                    offset' = offset - IR.sizeToInt size

        let assignInstructions :: Seq Instruction
            assignInstructions =
                asum (paramList <&> \(i, (size, op)) ->
                    move size op (parameter i size)) ><
                assignParameter' pushedParamsOver6Size paramStack
                where
                    (paramReg, paramStack) = splitAt 6 (zip sizes scalars')
                    paramList = zip [1..6] paramReg

        return
            (Register (RAX, size),
            pushes ><
            minuses ><
            (assignInstructions |>
            Call (ImmediateLabel name)) ><
            adds ><
            pops
            )

    _ -> undefined

singleStatement :: IR.SingleStatement -> State GeneratorState (Seq Instruction)
singleStatement = \case
    IR.Exit s -> do
        op <- scalar s
        return $ Sq.fromList [Move op (parameter 1 B4), Call "exit"]

    IR.Assign size to from@(IR.Scalar (IR.String {})) -> do
        memoryTable <- gets memoryTable
        (op, evaluate) <- expression from

        case memoryTable M.!? to of
            Just location -> do
                return $ evaluate ><
                    move B8 op (operandFromMemoryLocation location)
            Nothing -> do
                location <- allocate to size
                return $ evaluate ><
                    move B8 op (operandFromMemoryLocation location)

    IR.Assign size to from -> do
        memoryTable <- gets memoryTable
        (op, evaluate) <- expression from

        case memoryTable M.!? to of
            Just location -> do
                return $ evaluate ><
                    move size op (operandFromMemoryLocation location)
            Nothing -> do
                location <- allocate to size
                return $ evaluate ><
                    move size op (operandFromMemoryLocation location)

    -- IR.AssignIndirect size to from -> do
    --     memoryTable <- gets memoryTable
    --     (op, evaluate) <- expression from

    --     case memoryTable M.!? to of


    IR.PrintString s -> do
        op <- scalar s
        registerPool <- gets registerPool
        return $
            if RDI `elem` S.elems registerPool then
                Sq.fromList
                [ Push (Register (RDI, B8))
                , LoadAddress op (parameter 1 B8)
                , Call "print_string"
                , Pop (Register (RDI, B8))]
            else
                Sq.fromList
                [ LoadAddress op (parameter 1 B8)
                , Call "print_string"]

    IR.Return s -> do
        op <- scalar s
        return $ Sq.fromList [Move op (Register (RAX, B8)), Leave, Return]

    e -> error $ "Not implemented: " ++ show e

instruction :: IR.NoControlFlowStatement -> State GeneratorState (Seq Instruction)
instruction = \case
    IR.NCF statement -> singleStatement statement

    IR.Label name ->
        return $ Sq.singleton $ Label name

    IR.Goto name ->
        return $ Sq.singleton $ Jump (ImmediateLabel name)

    IR.FreeVariable identifier -> do
        free identifier
        return Sq.empty

    IR.WhileReference identifiers -> do
        traverse_ free (S.toList identifiers)
        return Sq.empty

    e -> error $ "Not implemented: " ++ show e

instructions :: [IR.NoControlFlowStatement] -> State GeneratorState (Seq Instruction)
instructions = fmap asum . traverse instruction

function :: IR.Function IR.NoControlFlowStatement -> State GeneratorState (Seq Instruction)
function (IR.Function name parameters statements) = do
    oldState <- get

    ss <- instructions statements

    put oldState

    return $
        (Sq.fromList
        [ Label name
        , Push (Register (RBP, B8))
        , Move (Register (RSP, B8)) (Register (RBP, B8))] ><
        ss) ><
        Sq.fromList (if name == "main" then [Leave, Return] else [])

functions :: [IR.Function IR.NoControlFlowStatement] -> State GeneratorState (Seq Instruction)
functions = fmap asum . traverse function

macro :: Seq Instruction
macro =
    Sq.fromList
    [
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
innerPrintString :: Seq Instruction
innerPrintString = Sq.fromList
    [
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

program :: IR.Program IR.NoControlFlowStatement -> State GeneratorState (Seq Instruction)
program (IR.Program dataSegment fs) = do
    functions' <- functions fs

    dataSegment' <-
        for (Sq.fromList $ M.toList dataSegment) $ \(name, number) -> do
            return $ Sq.fromList
                [ Label ("str." ++ show number)
                , Int (length name + 1)
                , AsciiZero name]

    return
        $  (macro |> EmptyLine)
        >< (functions' |> EmptyLine)
        >< (innerPrintString |> EmptyLine)
        >< asum dataSegment'

generateX64 :: IR.Program IR.NoControlFlowStatement -> Seq Instruction
generateX64 p = evalState (program p) (GeneratorState M.empty S.empty [])
