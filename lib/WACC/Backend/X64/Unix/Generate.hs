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
import Debug.Trace (traceShowId, traceShow, trace)
import Data.Char

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
    tmpStackOffset :: Int,
    tmpPushedRegs :: [PhysicalRegister],
    functionName :: String
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
        let freeRegister = minimumBy (compare `on` rankRegister) $ S.toList freeRegisters

        let location = AtRegister (freeRegister, size)

        modify $ \s -> s {
            registerPool = S.insert freeRegister registerPool,
            memoryTable = M.insert var location memoryTable,
            stainedCalleeSaveRegs = S.insert freeRegister stainedCalleeSaveRegs
        }

        return location

    else do
        -- All registers are in use, allocate on the stack.
        let (offset, stackPool') = allocateStack (IR.sizeToInt size) var stackPool
        let location = AtStack offset size

        modify $ \s -> s {
            stackPool = stackPool',
            memoryTable = M.insert var location memoryTable,
            maxStackSize = max maxStackSize (offset + IR.sizeToInt size)
        }

        return location

{- This function frees the resources associated with the given identifier.
   It removes the identifier's entry from the memory table and releases
   the corresponding memory (either register or stack). -}
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

{- Given a 'MemoryLocation', this function generates an 'Operand'. -}
operandFromMemoryLocation :: MemoryLocation -> State GeneratorState Operand
operandFromMemoryLocation = \case
    AtRegister register@(reg, _) -> do
        tmpPushedRegs <- gets tmpPushedRegs
        return $ case elemIndex reg tmpPushedRegs of
            Nothing -> Register register
            Just offset -> MemoryIndirect
                (Just (ImmediateInt (offset * 8))) (RSP, B8) Nothing

    AtStack offset _ -> do
        tmpStackOffset <- gets tmpStackOffset
        return $ MemoryIndirect
            (Just (ImmediateInt (offset - tmpStackOffset))) (RSP, B8) Nothing

    AtParameterStack offset _ ->
        -- +8 go beyond the pushed RBP
        -- +8 go beyond the return address
        return $ MemoryIndirect
            (Just (ImmediateInt (offset + 16))) (RBP, B8) Nothing

{- Given an IR.Scalar, this function generates the corresponding 'Operand'. -}
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
        return $ MemoryIndirect
            (Just $ ImmediateLabel ("str." ++ show n))
            (RIP, B8) Nothing

{- This function determines which caller-save registers
   are currently being used. -}
usedCallerSaveRegisters ::
            State GeneratorState ([PhysicalRegister], [PhysicalRegister])
usedCallerSaveRegisters = do
    registerPool <- gets registerPool
    let used = registerPool `S.intersection` callerSaveRegisters
    let usedParam = used `S.intersection` S.fromList parameterList

    let usedParamRegs = S.toList usedParam
    let usedNonParamRegs = S.toList (used S.\\ usedParam)

    return (usedParamRegs, usedNonParamRegs)

{- Given a list of physical registers to be pushed onto the stack,
   this function generates a pair of sequences of instructions to push
   and pop the registers onto and from the stack, respectively. -}
pushRegisters :: [PhysicalRegister] -> (Seq Instruction, Seq Instruction)
pushRegisters pushed =
    ( Sq.fromList [Push (Register (x, B8)) | x <- pushed]
    , Sq.fromList [Pop (Register (x, B8)) | x <- reverse pushed])

{- Given an offset, this function generates a pair of sequences of instructions
   to adjust the stack pointer (RSP) by that offset. -}
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

{- Pushes the given physical register onto the stack. -}
push :: PhysicalRegister -> State GeneratorState (Seq Instruction)
push op = do
    modify $ \s -> s {
        tmpStackOffset = tmpStackOffset s - 8,
        tmpPushedRegs = op : tmpPushedRegs s
    }
    return $ Sq.singleton $ Push (Register (op, B8))

{- Pops the given physical register from the stack. -}
pop :: PhysicalRegister -> State GeneratorState (Seq Instruction)
pop op = do
    modify $ \s -> s {
        tmpStackOffset = tmpStackOffset s + 8,
        tmpPushedRegs = drop 1 (tmpPushedRegs s)
    }
    return $ Sq.singleton $ Pop (Register (op, B8))

useManyTemporary :: [PhysicalRegister] -> State GeneratorState (Seq Instruction)
    -> State GeneratorState (Seq Instruction)
useManyTemporary regs = foldl (.) id (map useTemporary regs)

useTemporary :: PhysicalRegister -> State GeneratorState (Seq Instruction)
    -> State GeneratorState (Seq Instruction)
useTemporary reg runner = do
    registerPool <- gets registerPool
    let used = reg `S.member` registerPool

    if not used then
        runner
    else do
        pushInstr <- push reg
        result <- runner
        popInstr <- pop reg

        return $ pushInstr >< result >< popInstr

malloc :: Int -> State GeneratorState (Seq Instruction)
malloc size = expression (IR.Call B8 "malloc" [(B4, IR.Immediate size)])

expression :: IR.Expression -> State GeneratorState (Seq Instruction)
expression = \case
    IR.Scalar s -> do
        op <- scalar s
        let size = IR.getSize op
        return $ move size op (Register (RAX, IR.getSize op))

    IR.Not s -> do
        op <- scalar s
        return $ Sq.fromList
            [ Move op (Register (RAX, B1))
            , Not (Register (RAX, B1))]

    IR.Add s1 s2 -> do
        op1 <- scalar s1
        op2 <- scalar s2
        return $ Sq.fromList
            [ Move op1 (Register (RAX, B4))
            , Add op2 (Register (RAX, B4))
            , JumpWhen Overflow "_errOverFlow"]

    IR.Subtract s1 s2 -> do
        op1 <- scalar s1
        op2 <- scalar s2

        return $ Sq.fromList
            [ Move op1 (Register (RAX, B4))
            , Subtract op2 (Register (RAX, B4))
            , JumpWhen Overflow "_errOverFlow"]

    IR.Multiply s1 s2 -> do
        op1 <- scalar s1
        op2 <- scalar s2

        return $ Sq.fromList
            [ Move op1 (Register (RAX, B4))
            , Multiply op2 (Register (RAX, B4))
            , JumpWhen Overflow "_errOverFlow" ]

    IR.Divide a b -> useManyTemporary [RDX, RSI] $ do
        a' <- scalar a
        b' <- scalar b

        return $ Sq.fromList
            [ Move a' (Register (RAX, B4))
            , Move (Immediate $ ImmediateInt 0) (Register (RDX, B4))
            , Move b' (Register (RSI, B4))
            , DivideI (Register (RSI, B4))]

    IR.Remainder s1 s2 -> do
        result <- expression (IR.Divide s1 s2)
        return $ result |> Move (Register (RDX, B4)) (Register (RAX, B4))

    IR.GreaterEqual size s1 s2 -> useTemporary RDX $ do
        op1 <- scalar s1
        op2 <- scalar s2

        return $ Sq.fromList
            [ Move op1 (Register (RAX, size))
            , Compare op2 (Register (RAX, size))
            , Move (Immediate $ ImmediateInt 0) (Register (RAX, B4))
            , Move (Immediate $ ImmediateInt 1) (Register (RDX, B4))
            , CompareMove GreaterEqual (Register (RDX, B4)) (Register (RAX, B4))
            ]

    IR.Greater size s1 s2 -> useTemporary RDX $ do
        op1 <- scalar s1
        op2 <- scalar s2

        return $ Sq.fromList
            [ Move op1 (Register (RAX, size))
            , Compare op2 (Register (RAX, size))
            , Move (Immediate $ ImmediateInt 0) (Register (RAX, B4))
            , Move (Immediate $ ImmediateInt 1) (Register (RDX, B4))
            , CompareMove Greater (Register (RDX, B4)) (Register (RAX, B4))
            ]

    IR.LessEqual size s1 s2 ->useTemporary RDX $ do
        op1 <- scalar s1
        op2 <- scalar s2

        return $ Sq.fromList
            [ Move op1 (Register (RAX, size))
            , Compare op2 (Register (RAX, size))
            , Move (Immediate $ ImmediateInt 0) (Register (RAX, B4))
            , Move (Immediate $ ImmediateInt 1) (Register (RDX, B4))
            , CompareMove LessEqual (Register (RDX, B4)) (Register (RAX, B4))
            ]

    IR.Less size s1 s2 -> useTemporary RDX $ do
        op1 <- scalar s1
        op2 <- scalar s2

        return $ Sq.fromList
            [ Move op1 (Register (RAX, size))
            , Compare op2 (Register (RAX, size))
            , Move (Immediate $ ImmediateInt 0) (Register (RAX, B4))
            , Move (Immediate $ ImmediateInt 1) (Register (RDX, B4))
            , CompareMove Less (Register (RDX, B4)) (Register (RAX, B4))
            ]

    IR.Equal size s1 s2 -> useTemporary RDX $ do
        op1 <- scalar s1
        op2 <- scalar s2

        return $ Sq.fromList
            [ Move op1 (Register (RAX, size))
            , Compare op2 (Register (RAX, size))
            , Move (Immediate $ ImmediateInt 0) (Register (RAX, B4))
            , Move (Immediate $ ImmediateInt 1) (Register (RDX, B4))
            , CompareMove Equal (Register (RDX, B4)) (Register (RAX, B4))
            ]

    IR.NotEqual size s1 s2 -> useTemporary RDX $ do
        op1 <- scalar s1
        op2 <- scalar s2

        return $ Sq.fromList
            [ Move op1 (Register (RAX, size))
            , Compare op2 (Register (RAX, size))
            , Move (Immediate $ ImmediateInt 0) (Register (RAX, B4))
            , Move (Immediate $ ImmediateInt 1) (Register (RDX, B4))
            , CompareMove NotEqual (Register (RDX, B4)) (Register (RAX, B4))
            ]

    IR.Dereference size s -> do
        op <- scalar s
        return $ Sq.fromList
            [ Move op (Register (RAX, B8))
            , Move (MemoryIndirect Nothing (RAX, B8) Nothing)
                    (Register (RAX, size))
            ]

    IR.SeekPairFirst s -> do
        op <- scalar s
        return $ return $
            Move op (Register (RAX, B8))

    IR.SeekPairSecond s -> do
        op <- scalar s
        return $ Sq.fromList
            [ Move op (Register (RAX, B8))
            , Add (Immediate $ ImmediateInt 8) (Register (RAX, B8))]

    IR.NewPair (fstSize, sndSize) (a, b) -> do
        evaluateAddress <- malloc 16

        initialise <- useTemporary RDX $ do
            opA <- scalar a
            opB <- scalar b

            return $
                return (Move (Register (RAX, B8)) (Register (RDX, B8))) ><
                move fstSize opA (MemoryIndirect Nothing (RDX, B8) Nothing) ><
                move sndSize opB (MemoryIndirect (Just (ImmediateInt 8)) (RDX, B8) Nothing) ><
                return (Move (Register (RDX, B8)) (Register (RAX, B8)))

        return $ evaluateAddress >< initialise

    IR.NewArray size@(IR.sizeToInt -> bytes) xs -> do
        evaluateAddress <- malloc (4 + bytes * length xs)

        initialise <- useTemporary RDX $ do
            elements <- traverse scalar xs

            return $
                move B8 (Register (RAX, B8)) (Register (RDX, B8)) ><

                move B4
                    (Immediate (ImmediateInt (length xs)))
                    (MemoryIndirect (Just (ImmediateInt (-4))) (RDX, B8) Nothing) ><

                asum (zip elements [0..] <&> \(element, index) ->
                    move size element $ MemoryIndirect
                        (Just (ImmediateInt (index * bytes)))
                        (RDX, B8) Nothing
                ) ><

                move B8 (Register (RDX, B8)) (Register (RAX, B8))

        return $
            evaluateAddress ><
            return (Add (Immediate (ImmediateInt 4)) (Register (RAX, B8))) ><
            initialise

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
                            (Just (ImmediateInt (offset + 16))) (RBP, B8) Nothing

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

        return $
            pushes ><
            minuses ><
            (assignInstructions |>
            Call (ImmediateLabel name)) ><
            adds ><
            pops

    IR.SeekArrayElement B1 a i -> expression $
        IR.Call B8 "seek_array_element1" [(B8, a), (B4, i)]
    IR.SeekArrayElement B4 a i -> expression $
        IR.Call B8 "seek_array_element4" [(B8, a), (B4, i)]
    IR.SeekArrayElement B8 a i -> expression $
        IR.Call B8 "seek_array_element8" [(B8, a), (B4, i)]
    IR.SeekArrayElement {} -> error "Other sizes are not supported."

    IR.Length addr -> useTemporary RDX $ do
        addr' <- scalar addr
        return $ Sq.fromList
            [
                Move addr' (Register (RDX, B8)),
                Move (MemoryIndirect (Just $ ImmediateInt (-4))
                        (RDX, B8)
                        Nothing) (Register (RAX, B4))
            ]

    IR.Order scalar' -> do
        scalar'' <- scalar scalar'
        return $ Sq.singleton (MoveSignSizeExtend B1 B4 scalar'' (Register (RAX, B4)))

    IR.Character scalar' -> do
        scalar'' <- scalar scalar'
        return $ Sq.singleton (Move scalar'' (Register (RAX, B4)))
    IR.And a b -> do
        a' <- scalar a
        b' <- scalar b
        return $ Sq.fromList
            [
                Move a' (Register (RAX, B1)),
                And b' (Register (RAX, B1))
            ]
    IR.Or a b -> do
        a' <- scalar a
        b' <- scalar b
        return $ Sq.fromList
            [
                Move a' (Register (RAX, B1)),
                Or b' (Register (RAX, B1))]
    IR.ReadInt
        -> return (Sq.singleton $ Call "readInt")

    IR.ReadChar -> expression (IR.Call B1 "getchar" [])


singleStatement :: IR.SingleStatement -> State GeneratorState (Seq Instruction)
singleStatement = \case
    IR.Exit s -> do
        op <- scalar s
        return $ Sq.fromList [Move op (Register (parameter 1 B4)), Call "exit"]

    IR.Assign size to from -> do
        memoryTable <- gets memoryTable
        evaluate <- expression from

        case memoryTable M.!? to of

            Just location -> do
                to' <- operandFromMemoryLocation location
                return $ evaluate >< move size (Register (RAX, size)) to'

            Nothing -> do
                location <- allocate to size
                to' <- operandFromMemoryLocation location
                return $ evaluate >< move size (Register (RAX, size)) to'

    IR.AssignIndirect size to from -> do
        memoryTable <- gets memoryTable
        evaluate <- expression from

        case memoryTable M.! to of
            AtRegister reg ->
                return $ evaluate ><
                    move size (Register (RAX, size)) (MemoryIndirect Nothing reg Nothing)

            AtStack offset _ -> useTemporary RDX $ do
                tmpStackOffset <- gets tmpStackOffset
                return $ evaluate ><
                    move size (MemoryIndirect
                            (Just (ImmediateInt (offset - tmpStackOffset)))
                            (RSP, B8)
                            Nothing)
                        (Register (RDX, B8)) ><
                    move size (Register (RAX, size)) (MemoryIndirect Nothing (RDX, B8) Nothing)

            AtParameterStack offset _ -> useTemporary RDX $
                -- +8 go beyond the pushed RBP
                -- +8 go beyond the return address
                return $ evaluate ><
                    move size (MemoryIndirect
                            (Just (ImmediateInt (offset + 16))) (RBP, B8) Nothing)
                        (Register (RDX, B8)) ><
                    move size (Register (RAX, size)) (MemoryIndirect Nothing (RDX, B8) Nothing)

    IR.PrintString s -> expression (IR.Call B8 "_prints" [(B8, s)])

    IR.PrintInt s -> expression (IR.Call B8 "print_int" [(B4, s)])

    IR.PrintBool s -> expression (IR.Call B8 "print_bool" [(B1, s)])

    IR.PrintLineBreak -> expression (IR.Call B4 "putchar" [(B4, IR.Immediate (ord '\n'))])

    IR.PrintAddress s -> expression (IR.Call B8 "print_pointer" [(B8, s)])

    IR.Return size s -> do
        op <- scalar s
        return $ Sq.fromList [Move op (Register (RAX, size)), Leave, Return]
        name <- gets functionName

        return $ Sq.fromList
            [ Move op (Register (RAX, size))
            , Jump . ImmediateLabel $ name ++ ".return"]

    IR.Free s -> undefined

    IR.FreeArray s -> undefined

    IR.PrintChar s -> expression (IR.Call B8 "_printc" [(B1, s)])

instruction :: IR.NoControlFlowStatement -> State GeneratorState (Seq Instruction)
instruction = \case
    IR.NCF statement ->
        (Comment (show statement) <|) <$> singleStatement statement

    IR.Label name ->
        return $ Sq.singleton $ Label name

    IR.Goto name ->
        return $ Sq.singleton $ Jump (ImmediateLabel name)

    IR.GotoIfNot s name -> do
        op <- scalar s
        return $ Sq.fromList
            [ Move op (Register (RAX, B1))
            , Compare (Immediate $ ImmediateInt 0) (Register (RAX, B1))
            , JumpWhen Equal (ImmediateLabel name)]

    IR.GotoIf s name -> do
        op <- scalar s
        return $ Sq.fromList
            [ Move op (Register (RAX, B1))
            , Compare (Immediate $ ImmediateInt 0) (Register (RAX, B1))
            , JumpWhen NotEqual (ImmediateLabel name)]

    IR.FreeVariable identifier -> do
        free identifier
        return Sq.empty

    IR.WhileReference identifiers -> do
        traverse_ free (S.toList identifiers)
        return Sq.empty


instructions :: [IR.NoControlFlowStatement] -> State GeneratorState (Seq Instruction)
instructions = fmap asum . traverse instruction

accumulate :: Num a => [a] -> [a]
accumulate = f 0 where
    f _ [] = []
    f n (x:xs) = (n + x) : f (n + x) xs

function :: IR.Function IR.NoControlFlowStatement -> State GeneratorState (Seq Instruction)
function (IR.Function name parameters statements) = do
    oldState <- get

    modify $ \s -> s { functionName = name }

    let (registerParams, stackParams) = splitAt 6 parameters

    for_ (zip [1..] registerParams) $ \(i, (ident, size)) -> do
        memoryTable <- gets memoryTable
        let reg@(physicalReg, _) = parameter i size
        modify $ \s -> s {
            memoryTable = M.insert ident (AtRegister reg) memoryTable,
            registerPool = S.insert physicalReg (registerPool s)
        }

    let stackParamOffsets = 0 : accumulate (sizeToInt . snd <$> stackParams)

    for_ (zip stackParamOffsets stackParams) $ \(offset, (ident, size)) -> do
        memoryTable <- gets memoryTable
        modify $ \s -> s {
            memoryTable = M.insert ident (AtParameterStack offset size) memoryTable
        }

    statements' <- instructions statements
    (S.toList -> stainedCalleeSaveRegs) <- gets stainedCalleeSaveRegs
    maxStackSize <- gets maxStackSize
    let (pushes, pops) = pushRegisters stainedCalleeSaveRegs
        (pushStack, popStack) = moveESP maxStackSize
    put oldState

    -- Main program returns 0 by default, but other functions do not.
    let mainDefaultReturn = if name /= "main" then Sq.empty else Sq.singleton
            (Move (Immediate $ ImmediateInt 0) (Register (RAX, B8)))

    return $
        Sq.fromList
            [ Label name
            , Push (Register (RBP, B8))
            , Move (Register (RSP, B8)) (Register (RBP, B8))] ><
        pushes ><
        pushStack ><
        statements' ><
        Sq.singleton (Label $ name ++ ".return") ><
        popStack ><
        pops ><
        mainDefaultReturn ><
        Sq.fromList [Leave, Return]

functions :: [IR.Function IR.NoControlFlowStatement] -> State GeneratorState (Seq Instruction)
functions = fmap asum . traverse function

macro :: Seq Instruction
macro =
    Sq.fromList
    [
    IfDefined "__APPLE__",
        Define "fflush"  "_fflush",
        Define "write"   "_write",
        Define "printf"  "_printf",
        Define "scanf"   "_scanf",
        Define "exit"    "_exit",
        Define "malloc"  "_malloc",
        Define "putchar" "_putchar",
        Define "getchar" "_getchar",
        Global "_main",
        Define "main" "_main",
    EndIf,
    EmptyLine,
    IfDefined "__linux__",
        Define "fflush" "fflush@PLT",
        Define "write" "write@PLT",
        Define "printf" "printf@PLT",
        Define "scanf" "scanf@PLT",
        Define "exit" "exit@PLT",
        Define "malloc" "malloc@PLT",
        Define "putchar" "putchar@PLT",
        Define "getchar" "getchar@PLT",
        Global "main",
    EndIf
    ]

program :: IR.Program IR.NoControlFlowStatement -> State GeneratorState (Seq Instruction)
program (IR.Program dataSegment fs) = do
    functions' <- functions fs

    dataSegment' <-
        for (Sq.fromList $ M.toList dataSegment) $ \(name, number) ->
            return $ Sq.fromList
                [ Int (length name)
                , Label ("str." ++ show number)
                , AsciiZero name]

    return
        $  (macro |> EmptyLine)
        >< (functions' |> EmptyLine)
        >< (asum
            [ Internal.printString'
            -- , Internal.printString
            , Internal.printInt
            , Internal.printBool
            , Internal.printChar
            , Internal.printChar'
            , Internal.printPointer
            , Internal.seekArrayElement1
            , Internal.seekArrayElement4
            , Internal.seekArrayElement8
            , Internal.errorOutOfBounds
            , Internal.mallocFunction
            , Internal.errorNull
            , Internal.errorOutOfMemory
            , Internal.errorOverFlow
            , Internal.readInt
            ] |> EmptyLine)
        >< asum dataSegment'

initialState :: GeneratorState
initialState = GeneratorState {
    memoryTable = M.empty,
    registerPool = S.empty,
    stackPool = [],
    stainedCalleeSaveRegs = S.empty,
    maxStackSize = 0,
    tmpStackOffset = 0,
    tmpPushedRegs = [],
    functionName = undefined
}

generateX64 :: IR.Program IR.NoControlFlowStatement -> Seq Instruction
generateX64 p = evalState (program p) initialState
