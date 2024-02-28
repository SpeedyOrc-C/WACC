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

import qualified WACC.Backend.X64.Unix.Internal as Internal
import qualified WACC.IR.Structure as IR
import           WACC.IR.Structure (Size(..), Identifier, sizeToInt)
import           WACC.Backend.X64.Structure
import           WACC.Backend.StackPool
import Data.Functor

{- This indicates the location of the data. Stored in registers,
   stored in the stack or stored in the parameter stack. -}
data MemoryLocation
    = AtRegister Register
    | AtStack Int Size
    | AtParameterStack Int Size
    deriving (Show)

{- Function to rank physical registers based on priority. -}
rankRegister :: PhysicalRegister -> Int
rankRegister = \case
    RBX -> 1; R12 -> 2;  R13 -> 3;  R14 -> 4
    R15 -> 5; R10 -> 6;  R11 -> 7;  R9  -> 8
    R8  -> 9; RCX -> 10; RSI -> 11; RDI -> 12

    r -> error $ "Cannot use register " ++ show r

{- A set of all available physical registers. -}
registers :: S.Set PhysicalRegister
registers = S.fromList
    [RBX, R12, R13, R14, R15, R10, R11, R9, R8, RCX, RSI, RDI]

parameter :: Int -> Size -> Register
parameter n size = case n of
    1 -> (RDI, size)
    2 -> (RSI, size)
    3 -> (RDX, size)
    4 -> (RCX, size)
    6 -> (R9, size)
    5 -> (R8, size)
    _ -> error $ "Cannot use parameter " ++ show n

parameterList :: [PhysicalRegister]
parameterList = [RDI, RSI, RDX, RCX, R8, R9]

data GeneratorState = GeneratorState {
    memoryTable :: M.Map Identifier MemoryLocation,
    registerPool :: S.Set PhysicalRegister,
    stackPool :: [StackSegment Identifier],
    stainedCalleeSaveRegs :: S.Set PhysicalRegister,
    maxStackSize :: Int,
    tmpStackOffset :: Int
} deriving (Show)

{- This function is responsible for allocating memory for a variable.
   It first checks if there are any free registers available. If so, 
   it allocates the variable to one of the available registers.
   If all registers are in use, it allocates memory on the stack. -}
allocate :: Identifier -> Size -> State GeneratorState MemoryLocation
allocate var size = do
    registerPool <- gets registerPool
    memoryTable <- gets memoryTable
    stackPool <- gets stackPool
    stainedCalleeSaveRegs <- gets stainedCalleeSaveRegs
    maxStackSize <- gets maxStackSize

    if S.size registerPool < S.size registers then do
        -- There are free registers.
        let freeRegisters = registers S.\\ registerPool
        let freeRegister = minimumBy (compare `on` rankRegister) $ S.toList 
                            freeRegisters

        let location = AtRegister (freeRegister, size)

        modify $ \s -> s {
            registerPool = S.insert freeRegister registerPool,
            memoryTable = M.insert var location memoryTable,
            stainedCalleeSaveRegs = S.insert freeRegister stainedCalleeSaveRegs
        }

        return location

    else do
        -- All registers are in use, allocate on the stack.
        let (offset, stackPool') = allocateStack (IR.sizeToInt size) var 
                                    stackPool
        let location = AtStack offset size

        modify $ \s -> s {
            stackPool = stackPool',
            memoryTable = M.insert var location memoryTable,
            maxStackSize = max maxStackSize (offset + IR.sizeToInt size)
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

operandFromMemoryLocation :: MemoryLocation -> State GeneratorState Operand
operandFromMemoryLocation = \case
    AtRegister register ->
        return $ Register register
    AtStack offset _ -> do
        tmpStackOffset <- gets tmpStackOffset
        return $ MemoryIndirect
            (Just (ImmediateInt (offset - tmpStackOffset))) (RSP, B8) Nothing
    AtParameterStack offset _ ->
        -- +8 go beyond the pushed RBP
        -- +8 go beyond the return address
        return $ MemoryIndirect
            (Just (ImmediateInt (offset + 16))) (RBP, B8) Nothing

scalar :: IR.Scalar -> State GeneratorState Operand
scalar = \case
    IR.Immediate n -> return $
        Immediate (ImmediateInt n)

    IR.Variable identifier@(IR.Temporary {}) -> do
        memoryTable <- gets memoryTable
        free identifier
        operandFromMemoryLocation $ memoryTable M.! identifier

    IR.Variable identifier -> do
        memoryTable <- gets memoryTable
        operandFromMemoryLocation $ memoryTable M.! identifier

    IR.String n ->
        return $ MemoryIndirect (Just $ ImmediateLabel ("str." ++ show n)) 
                                (RIP, B8) Nothing

usedCallerSaveRegisters :: State GeneratorState ([PhysicalRegister], 
                            [PhysicalRegister])
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

push :: PhysicalRegister -> State GeneratorState (Seq Instruction)
push op = do
    modify $ \s -> s { tmpStackOffset = tmpStackOffset s - 8 }
    return $ Sq.singleton $ Push (Register (op, B8))

pop :: PhysicalRegister -> State GeneratorState (Seq Instruction)
pop op = do
    modify $ \s -> s { tmpStackOffset = tmpStackOffset s + 8 }
    return $ Sq.singleton $ Pop (Register (op, B8))

expression :: IR.Expression -> State GeneratorState (Operand, Seq Instruction)
expression = \case
    IR.Scalar s -> do
        op <- scalar s
        return (op, Sq.empty)

    IR.Not a -> do
        a' <- scalar a
        return (Register (RAX, B4), Sq.fromList
            [ Move a' (Register (RAX, B4))
            , Not (Register (RAX, B4))])

    IR.Negate a -> do
        a' <- scalar a
        return (Register (RAX, B4), Sq.fromList
            [ Move a' (Register (RAX, B4))
            , Negate (Register (RAX, B4))])

    IR.Add a b -> do
        a' <- scalar a
        b' <- scalar b
        return
            ( Register (RAX, B4)
            , Sq.fromList
              [ Move a' (Register (RAX, B4))
              , Add b' (Register (RAX, B4))])
    IR.NewPair (fstSize, sndSize) (a, b) -> do
        registerPool <- gets registerPool
        let usedRDX = S.member RDX registerPool
        (_, address) <- expression (IR.Call B8 "_malloc" 
                                    [(B4, IR.Immediate 16)])

        maybePushRDX <- if usedRDX then push RDX else return Sq.empty
        a' <- scalar a
        b' <- scalar b
        maybePopRDX <- if usedRDX then pop RDX else return Sq.empty
        return(
            Register (RAX, B8)
            ,
            address >< move B8 (Register (RAX, B8)) (Register (RDX, B8)) ><
            maybePushRDX ><
            move fstSize a' (MemoryIndirect Nothing (RDX, B8) Nothing) ><
            move sndSize b' (MemoryIndirect (Just (ImmediateInt 8)) (RDX, B8) 
                                            Nothing) ><
            move B8 (Register (RDX, B8)) (Register (RAX, B8)) ><
            maybePopRDX
            )

    IR.NewArray size@(IR.sizeToInt -> size') scalars -> do
        registerPool <- gets registerPool
        let usedRDX = S.member RDX registerPool
        let len = length scalars
        (_, address) <- expression (IR.Call B8 "_malloc" 
                                    [(B4, IR.Immediate (4 + size'*len))])

        maybePushRDX <- if usedRDX then push RDX else return Sq.empty
        scalars' <- traverse scalar scalars
        maybePopRDX <- if usedRDX then pop RDX else return Sq.empty
        return(
            Register (RAX, B8)
            --Comment (show len ++ " element array") <|
            , (address|> Add (Immediate (ImmediateInt 4)) 
                                        (Register (RAX, B8))) ><
            maybePushRDX ><
            move B8 (Register (RAX, B8)) (Register (RDX, B8)) ><
            move size (Immediate (ImmediateInt len)) (MemoryIndirect
                        (Just (ImmediateInt (-4)))
                        (RDX, B8)
                        Nothing) ><
            asum
                [move size scalar' (MemoryIndirect
                        (Just (ImmediateInt (size' * i)))
                        (RDX, B8)
                        Nothing)
                    |(scalar', i) <- zip scalars' [0..]]
            >< move B8 (Register (RDX, B8)) (Register (RAX, B8))
            >< maybePopRDX)


    IR.Subtract a b -> do
        a' <- scalar a
        b' <- scalar b
        return
            ( Register (RAX, B4)
            , Sq.fromList
              [ Move a' (Register (RAX, B4))
              , Subtract b' (Register (RAX, B4))])

    IR.Multiply a b -> do
        registerPool <- gets registerPool
        let usedRDX = S.member RDX registerPool

        maybePushRDX <- if usedRDX then push RDX else return Sq.empty
        a' <- scalar a
        b' <- scalar b
        maybePopRDX <- if usedRDX then pop RDX else return Sq.empty

        return ( Register (RAX, B4)
            ,  maybePushRDX
            >< Sq.fromList
               [ Move a' (Register (RAX, B4))
               , Multiply b' (Register (RAX, B4))]
            >< maybePopRDX
            )

    IR.Divide a b -> do
        registerPool <- gets registerPool
        let usedRDX = S.member RDX registerPool

        maybePushRDX <- if usedRDX then push RDX else return Sq.empty
        a' <- scalar a
        b' <- scalar b
        maybePopRDX <- if usedRDX then pop RDX else return Sq.empty

        return ( Register (RAX, B4)
            ,  maybePushRDX
            >< Sq.fromList
               [ Move a' (Register (RAX, B4))
               , Move (Immediate $ ImmediateInt 0) (Register (RDX, B4))
               , DivideI b'
               , Move (Register (RDX, B4)) (Register (RAX, B4))]
            >< maybePopRDX
            )

    IR.Remainder a b -> do
        registerPool <- gets registerPool
        let usedRDX = S.member RDX registerPool

        maybePushRDX <- if usedRDX then push RDX else return Sq.empty
        a' <- scalar a
        b' <- scalar b
        maybePopRDX <- if usedRDX then pop RDX else return Sq.empty

        return ( Register (RAX, B4)
            ,  maybePushRDX
            >< Sq.fromList
               [ Move a' (Register (RAX, B4))
               , Move (Immediate $ ImmediateInt 0) (Register (RDX, B4))
               , DivideI b'
               , Move (Register (RDX, B4)) (Register (RAX, B4))]
            >< maybePopRDX
            )

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

                    AtParameterStack offset _ ->
                        MemoryIndirect
                            (Just (ImmediateInt (offset + 16))) (RBP, B8) 
                                                                Nothing

            s -> scalar s

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
                    move size op (Register (parameter i size))) ><
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
        return $ Sq.fromList [Move op (Register (parameter 1 B4)), Call "exit"]

    IR.Assign size to from@(IR.Scalar (IR.String {})) -> do
        memoryTable <- gets memoryTable
        (op, evaluate) <- expression from

        case memoryTable M.!? to of
            Just location -> do
                to' <- operandFromMemoryLocation location
                return $ evaluate ><
                    move B8 op to'
            Nothing -> do
                location <- allocate to size
                to' <- operandFromMemoryLocation location
                return $ evaluate ><
                    move B8 op to'

    IR.Assign size to from -> do
        memoryTable <- gets memoryTable
        (op, evaluate) <- expression from

        case memoryTable M.!? to of
            Just location -> do
                to' <- operandFromMemoryLocation location
                return $ evaluate ><
                    move size op to'
            Nothing -> do
                location <- allocate to size
                to' <- operandFromMemoryLocation location
                return $ evaluate ><
                    move size op to'

    IR.AssignIndirect size to from -> do
        memoryTable <- gets memoryTable
        (op, evaluate) <- expression from

        case memoryTable M.! to of
            AtRegister reg ->
                return $ evaluate ><
                    move size op (MemoryIndirect Nothing reg Nothing)

            AtStack offset _ -> do
                tmpStackOffset <- gets tmpStackOffset
                return $ evaluate ><
                    move size (MemoryIndirect
                            (Just (ImmediateInt (offset - tmpStackOffset)))
                            (RSP, B8)
                            Nothing)
                        (Register (RAX, B8)) ><
                    move size op (MemoryIndirect Nothing (RAX, B8) Nothing)

            AtParameterStack offset _ ->
                    -- +8 go beyond the pushed RBP
                    -- +8 go beyond the return address
                return $ evaluate ><
                    move size (MemoryIndirect
                            (Just (ImmediateInt (offset + 16))) (RBP, B8) 
                                                                Nothing)
                        (Register (RAX, B8)) ><
                    move size op (MemoryIndirect Nothing (RAX, B8) Nothing)


    IR.PrintString s -> do
        op <- scalar s
        registerPool <- gets registerPool
        return $
            if RDI `elem` S.elems registerPool then
                Sq.fromList
                [ Push (Register (RDI, B8))
                , LoadAddress op (Register (parameter 1 B8))
                , Call "print_string"
                , Pop (Register (RDI, B8))]
            else
                Sq.fromList
                [ LoadAddress op (Register (parameter 1 B8))
                , Call "print_string"]

    IR.PrintLineBreak -> snd <$> expression (IR.Call B8 "print_line_break" [])

    IR.Return s -> do
        op <- scalar s
        return $ Sq.fromList [Move op (Register (RAX, B8)), Leave, Return]

    e -> error $ "Not implemented: " ++ show e

instruction :: IR.NoControlFlowStatement -> 
                State GeneratorState (Seq Instruction)
instruction = \case
    IR.NCF statement -> singleStatement statement

    IR.Label name ->
        return $ Sq.singleton $ Label name

    IR.Goto name ->
        return $ Sq.singleton $ Jump (ImmediateLabel name)

    IR.GotoIfNot s name -> do
        op <- scalar s
        return $ Sq.fromList
            [ Move op (Register (RAX, B1))
            , Test (Immediate $ ImmediateLabel name) 
                   (Immediate $ ImmediateInt 1)]

    IR.FreeVariable identifier -> do
        free identifier
        return Sq.empty

    IR.WhileReference identifiers -> do
        traverse_ free (S.toList identifiers)
        return Sq.empty


instructions :: [IR.NoControlFlowStatement] -> 
                State GeneratorState (Seq Instruction)
instructions = fmap asum . traverse instruction

accumulate :: Num a => [a] -> [a]
accumulate = f 0 where
    f _ [] = []
    f n (x:xs) = (n + x) : f (n + x) xs

function :: IR.Function IR.NoControlFlowStatement -> 
            State GeneratorState (Seq Instruction)
function (IR.Function name parameters statements) = do
    oldState <- get

    let (registerParams, stackParams) = splitAt 6 parameters

    for_ (zip [1..] registerParams) $ \(i, (ident, size)) -> do
        memoryTable <- gets memoryTable
        modify $ \s -> s {
            memoryTable = M.insert ident (AtRegister (parameter i size)) 
                                                            memoryTable
        }

    let stackParamOffsets = 0 : accumulate (sizeToInt . snd <$> stackParams)

    for_ (zip stackParamOffsets stackParams) $ \(offset, (ident, size)) -> do
        memoryTable <- gets memoryTable
        modify $ \s -> s {
            memoryTable = M.insert ident (AtParameterStack offset size) 
                                                            memoryTable
        }

    ss <- instructions statements
    (S.toList -> stainedCalleeSaveRegs) <- gets stainedCalleeSaveRegs
    maxStackSize <- gets maxStackSize
    let (pushes, pops) = pushRegisters stainedCalleeSaveRegs
        (pushStack, popStack) = moveESP maxStackSize
    put oldState

    return $
        (Sq.fromList
        [ Label name
        , Push (Register (RBP, B8))
        , Move (Register (RSP, B8)) (Register (RBP, B8))] ><
        pushes ><
        pushStack ><
        ss >< popStack >< pops) ><
        Sq.fromList (if name == "main" then [Leave, Return] else [])

functions :: [IR.Function IR.NoControlFlowStatement] -> State GeneratorState 
                                                            (Seq Instruction)
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

program :: IR.Program IR.NoControlFlowStatement -> 
            State GeneratorState (Seq Instruction)
program (IR.Program dataSegment fs) = do
    functions' <- functions fs

    dataSegment' <-
        for (Sq.fromList $ M.toList dataSegment) $ \(name, number) ->
            return $ Sq.fromList
                [ Label ("str." ++ show number)
                , Int (length name + 1)
                , AsciiZero name]

    return
        $  (macro |> EmptyLine)
        >< (functions' |> EmptyLine)
        >< (asum [Internal.printString, Internal.printLineBreak] |> EmptyLine)
        >< asum dataSegment'

initialState :: GeneratorState
initialState = GeneratorState {
    memoryTable = M.empty,
    registerPool = S.empty,
    stackPool = [],
    stainedCalleeSaveRegs = S.empty,
    maxStackSize = 0,
    tmpStackOffset = 0
}

generateX64 :: IR.Program IR.NoControlFlowStatement -> Seq Instruction
generateX64 p = evalState (program p) initialState
