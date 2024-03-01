module WACC.Backend.X64.Unix.Generate where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Sq
import           Data.List
import           Data.Functor
import           Data.Bifunctor
import           Data.Foldable
import           Data.Sequence ((|>), (<|), (><), Seq)
import           Data.Function
import           Data.Traversable
import           Control.Monad
import           Control.Monad.Trans.State.Lazy

import qualified WACC.Backend.X64.Unix.Internal as Internal
import qualified WACC.IR.Structure as IR
import           WACC.IR.Structure (Size(..), Identifier, sizeToInt)
import           WACC.Backend.X64.Structure
import           WACC.Backend.StackPool

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
    tmpPushedRegs :: [Maybe PhysicalRegister],
    functionName :: String,
    calledInternalFunctions :: S.Set Internal.Function
} deriving (Show)

use :: Internal.Function -> State GeneratorState ()
use f = modify $ \s -> s {
    calledInternalFunctions = S.insert f (calledInternalFunctions s)
}

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
            memoryTable = M.insert var location memoryTable
        }

        when (freeRegister `elem` calleeSaveRegisters) $
            modify $ \s -> s {
                stainedCalleeSaveRegs =
                    S.insert freeRegister stainedCalleeSaveRegs
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
        return $ case elemIndex (Just reg) tmpPushedRegs of
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
usedCallerSaveRegisters :: State GeneratorState [PhysicalRegister]
usedCallerSaveRegisters = do
    registerPool <- gets registerPool
    return (S.toList (registerPool `S.intersection` callerSaveRegisters))

{- Given a list of physical registers to be pushed onto the stack,
   this function generates a pair of sequences of instructions to push
   and pop the registers onto and from the stack, respectively. -}
pushRegisters :: [PhysicalRegister] -> (Seq Instruction, Seq Instruction)
pushRegisters pushed = bimap Sq.fromList Sq.fromList
    ( [Push (Register (x, B8)) | x <- pushed]
    , [Pop (Register (x, B8)) | x <- reverse pushed])

ceil16 :: Int -> Int
ceil16 x = if modulo == 0 then x else x + 16 - modulo
    where modulo = x `mod` 16

downRSP :: Int -> Seq Instruction
downRSP 0 = Sq.empty
downRSP x = Sq.singleton $ Subtract (Immediate $ ImmediateInt x) (Register (RSP, B8))

upRSP :: Int -> Seq Instruction
upRSP 0 = Sq.empty
upRSP x = Sq.singleton $ Add (Immediate $ ImmediateInt x) (Register (RSP, B8))

{-
The reason why we take a Maybe PhysicalRegister is because,
sometimes we don't want to push or pop a register but just
move the stack pointer instead. Assuming "something" has been pushed.
-}
push :: Maybe PhysicalRegister -> State GeneratorState (Seq Instruction)
push maybeReg = do
    modify $ \s -> s {
        tmpStackOffset = tmpStackOffset s - 8,
        tmpPushedRegs = maybeReg : tmpPushedRegs s
    }
    return $ case maybeReg of
        Just reg -> Sq.singleton $ Push (Register (reg, B8))
        Nothing -> downRSP 8

pop :: Maybe PhysicalRegister -> State GeneratorState (Seq Instruction)
pop maybeReg = do
    modify $ \s -> s {
        tmpStackOffset = tmpStackOffset s + 8,
        tmpPushedRegs = drop 1 (tmpPushedRegs s)
    }
    return $ case maybeReg of
        Just reg -> Sq.singleton $ Pop (Register (reg, B8))
        Nothing -> upRSP 8

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
        pushInstr <- push (Just reg)
        result <- runner
        popInstr <- pop (Just reg)

        return $ pushInstr >< result >< popInstr

pairHelper :: IR.Scalar -> State GeneratorState (Seq Instruction)
pairHelper s = do
    op <- scalar s

    use Internal.ErrorNull
    use Internal.PrintString

    return $ Sq.fromList
        [ Compare (Immediate $ ImmediateInt 0) op
        , JumpWhen Equal "error_null"
        , Move op (Register (RAX, B8))]

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
            , Xor (Immediate $ ImmediateInt 1) (Register (RAX, B1))]

    IR.Add s1 s2 -> do
        op1 <- scalar s1
        op2 <- scalar s2

        use Internal.ErrorOverflow

        return $ Sq.fromList
            [ Move op1 (Register (RAX, B4))
            , Add op2 (Register (RAX, B4))
            , JumpWhen Overflow "error_overflow"]

    IR.Subtract s1 s2 -> do
        op1 <- scalar s1
        op2 <- scalar s2

        use Internal.ErrorOverflow

        return $ Sq.fromList
            [ Move op1 (Register (RAX, B4))
            , Subtract op2 (Register (RAX, B4))
            , JumpWhen Overflow "error_overflow"]

    IR.Multiply s1 s2 -> do
        op1 <- scalar s1
        op2 <- scalar s2

        use Internal.ErrorOverflow
        use Internal.PrintString

        return $ Sq.fromList
            [ Move op1 (Register (RAX, B4))
            , Multiply op2 (Register (RAX, B4))
            , JumpWhen Overflow "error_overflow" ]

    IR.Divide a b -> useTemporary RSI $ do
        a' <- scalar a
        b' <- scalar b

        use Internal.ErrorDivideZero
        use Internal.PrintString

        return $ Sq.fromList
            [ Move a' (Register (RAX, B4))
            , Move b' (Register (RSI, B4))
            , Compare (Immediate $ ImmediateInt 0) (Register (RSI, B4))
            , JumpWhen Equal "error_divide_zero"
            , CLTD
            , DivideI (Register (RSI, B4))]

    IR.Remainder s1 s2 -> do
        result <- expression (IR.Divide s1 s2)
        return $ result |> Move (Register (RDX, B4)) (Register (RAX, B4))

    IR.GreaterEqual size s1 s2 -> do
        op1 <- scalar s1
        op2 <- scalar s2

        return $ Sq.fromList
            [ Move op1 (Register (RAX, size))
            , Compare op2 (Register (RAX, size))
            , Set GreaterEqual (Register (RAX, B1))
            ]

    IR.Greater size s1 s2 -> do
        op1 <- scalar s1
        op2 <- scalar s2

        return $ Sq.fromList
            [ Move op1 (Register (RAX, size))
            , Compare op2 (Register (RAX, size))
            , Set Greater (Register (RAX, B1))
            ]

    IR.LessEqual size s1 s2 -> do
        op1 <- scalar s1
        op2 <- scalar s2

        return $ Sq.fromList
            [ Move op1 (Register (RAX, size))
            , Compare op2 (Register (RAX, size))
            , Set LessEqual (Register (RAX, B1))
            ]

    IR.Less size s1 s2 -> do
        op1 <- scalar s1
        op2 <- scalar s2

        return $ Sq.fromList
            [ Move op1 (Register (RAX, size))
            , Compare op2 (Register (RAX, size))
            , Set Less (Register (RAX, B1))
            ]

    IR.Equal size s1 s2 -> do
        op1 <- scalar s1
        op2 <- scalar s2

        return $ Sq.fromList
            [ Move op1 (Register (RAX, size))
            , Compare op2 (Register (RAX, size))
            , Set Equal (Register (RAX, B1))
            ]

    IR.NotEqual size s1 s2 -> do
        op1 <- scalar s1
        op2 <- scalar s2

        return $ Sq.fromList
            [ Move op1 (Register (RAX, size))
            , Compare op2 (Register (RAX, size))
            , Set NotEqual (Register (RAX, B1))
            ]

    IR.Dereference size s -> do
        op <- scalar s
        return $ Sq.fromList
            [ Move op (Register (RAX, B8))
            , Move (MemoryIndirect Nothing (RAX, B8) Nothing)
                    (Register (RAX, size))
            ]

    IR.SeekPairFirst s -> pairHelper s

    IR.SeekPairSecond s -> do
        getPair <- pairHelper s
        return $ getPair |> Add (Immediate $ ImmediateInt 8) (Register (RAX, B8))

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

    IR.Call _ name (unzip -> (sizes, scalars)) -> do
        callerSaveRegistersToBePushed <- usedCallerSaveRegisters
        let needAlignStack = odd $ length callerSaveRegistersToBePushed

        (asum -> pushCallerSave) <- traverse (push . Just) callerSaveRegistersToBePushed

        maybeAlignDownRSP <- if needAlignStack then push Nothing else return Sq.empty
        operands <- traverse scalar scalars
        maybeAlignUpRSP <- if needAlignStack then pop Nothing else return Sq.empty

        let (argsReg, argsStack) = splitAt 6 operands
        let (argsRegSizes, argsStackSizes) = splitAt 6 sizes

        let assignArgsReg =
                zip3 [1..6] argsRegSizes argsReg <&> \(n, size, arg) -> do
                    move size arg (Register (parameter n size))

        let allocateArgsStack = downRSP . ceil16 $ sum (IR.sizeToInt <$> argsStackSizes)

        let argsStackOffsets = scanl (+) 0 (IR.sizeToInt <$> argsStackSizes)

        let assignArgsStack =
                zip3 argsStackOffsets argsStackSizes argsStack <&> \(offset, size, arg) -> do
                    move size arg (MemoryIndirect (Just (ImmediateInt offset)) (RSP, B8) Nothing)

        let freeArgsStack = upRSP . ceil16 $ sum (IR.sizeToInt <$> argsStackSizes)

        (asum -> popCallerSave) <- traverse (pop . Just) (reverse callerSaveRegistersToBePushed)

        return $
            pushCallerSave ><
            maybeAlignDownRSP ><
                asum assignArgsReg ><
                allocateArgsStack ><
                    asum assignArgsStack ><
                    Sq.singleton (Call (ImmediateLabel name)) ><
                freeArgsStack ><
            maybeAlignUpRSP ><
            popCallerSave

    IR.SeekArrayElement B1 a i -> do
        use Internal.SeekArrayElement1
        use Internal.ErrorOutOfBounds
        expression $ IR.Call B8 "seek_array_element1" [(B8, a), (B4, i)]

    IR.SeekArrayElement B4 a i -> do
        use Internal.SeekArrayElement4
        use Internal.ErrorOutOfBounds
        expression $ IR.Call B8 "seek_array_element4" [(B8, a), (B4, i)]

    IR.SeekArrayElement B8 a i -> do
        use Internal.SeekArrayElement8
        use Internal.ErrorOutOfBounds
        expression $ IR.Call B8 "seek_array_element8" [(B8, a), (B4, i)]

    IR.SeekArrayElement {} -> error "Other sizes are not supported."

    IR.Length addr -> useTemporary RDX $ do
        addr' <- scalar addr
        return $ Sq.fromList
            [ Move addr' (Register (RDX, B8))
            , Move (MemoryIndirect (Just $ ImmediateInt (-4)) (RDX, B8) Nothing)
                    (Register (RAX, B4))
            ]

    IR.Order scalar' -> useTemporary RDX $ do
        scalar'' <- scalar scalar'
        return $ Sq.fromList
            [Move scalar'' (Register (RDX, B1)),
            MoveSignSizeExtend B1 B4 (Register (RDX, B1)) (Register (RAX, B4))]

    IR.Character scalar' -> do
        scalar'' <- scalar scalar'

        use Internal.ErrorBadChar

        return $ Sq.fromList
            [ Move scalar'' (Register (RAX, B4))
            , MoveSignSizeExtend B4 B8 (Register (RAX, B4)) (Register (RAX, B8))
            , Test (Immediate $ ImmediateInt $ -128) (Register (RAX, B8))
            , CompareMove NotEqual (Register (RAX, B8)) (Register (RSI, B8))
            , JumpWhen NotEqual "error_bad_char"]

    IR.And a b -> do
        a' <- scalar a
        b' <- scalar b

        return $ Sq.fromList
            [ Move a' (Register (RAX, B1))
            , And b' (Register (RAX, B1)) ]

    IR.Or a b -> do
        a' <- scalar a
        b' <- scalar b

        return $ Sq.fromList
            [ Move a' (Register (RAX, B1))
            , Or b' (Register (RAX, B1))]

    IR.ReadInt -> do
        use Internal.ReadInt
        return (Sq.singleton $ Call "read_int")

    IR.ReadChar k -> do
        use Internal.ReadChar
        expression (IR.Call B1 "read_char" [(B1, k)])

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

    IR.PrintString s -> do
        use Internal.PrintString
        expression (IR.Call B8 "print_string" [(B8, s)])

    IR.PrintInt s -> do
        use Internal.PrintInt
        expression (IR.Call B8 "print_int" [(B4, s)])

    IR.PrintBool s -> do
        use Internal.PrintBool
        expression (IR.Call B8 "print_bool" [(B1, s)])

    IR.PrintLineBreak -> do
        use Internal.PrintLineBreak
        expression (IR.Call B8 "print_line_break" [])

    IR.PrintAddress s -> do
        use Internal.PrintPointer
        use Internal.PrintString

        expression (IR.Call B8 "print_pointer" [(B8, s)])

    IR.Return size s -> do
        op <- scalar s
        name <- gets functionName

        return $ Sq.fromList
            [ Move op (Register (RAX, size))
            , Jump . ImmediateLabel $ name ++ ".return"]

    IR.Free s -> do
        op <- scalar s
        callFree <- expression (IR.Call B8 "free" [(B8, s)])

        use Internal.ErrorNull
        use Internal.PrintString

        return $ Sq.fromList
            [Compare (Immediate $ ImmediateInt 0) op, JumpWhen Equal "error_null"]
            >< callFree

    IR.FreeArray s -> useTemporary RDX $ do
        op <- scalar s
        call <- expression (IR.Call B8 "free" [])
        return $
            Sq.fromList
            [Move op (Register (RDX, B8)),
            Subtract (Immediate $ ImmediateInt 4) (Register (RDX, B8)),
            Move (Register (RDX, B8)) (Register (RDI, B8))]
            >< call

    IR.PrintChar s -> do
        use Internal.PrintChar
        expression (IR.Call B8 "print_char" [(B1, s)])

instruction :: IR.NoControlFlowStatement -> State GeneratorState (Seq Instruction)
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

function :: IR.Function IR.NoControlFlowStatement
    -> State GeneratorState (S.Set Internal.Function, Seq Instruction)
function (IR.Function name parameters statements) = do
    modify $ \s -> s { functionName = name }

    let (registerParams, stackParams) = splitAt 6 parameters

    for_ (zip [1..] registerParams) $ \(i, (ident, size)) -> do
        memoryTable <- gets memoryTable
        let reg@(physicalReg, _) = parameter i size
        modify $ \s -> s {
            memoryTable = M.insert ident (AtRegister reg) memoryTable,
            registerPool = S.insert physicalReg (registerPool s)
        }

    let stackParamOffsets = scanl (+) 0 (sizeToInt . snd <$> stackParams)

    for_ (zip stackParamOffsets stackParams) $ \(offset, (ident, size)) -> do
        memoryTable <- gets memoryTable
        modify $ \s -> s {
            memoryTable = M.insert ident (AtParameterStack offset size) memoryTable
        }

    statements' <- instructions statements

    stainedCalleeSaveRegs <- gets stainedCalleeSaveRegs
    maxStackSize <- gets maxStackSize

    let needAlignStack = odd $ length stainedCalleeSaveRegs
    let maybeAlignDownRSP = if needAlignStack then downRSP 8 else Sq.empty
    let maybeAlignUpRSP = if needAlignStack then upRSP 8 else Sq.empty

    let (pushes, pops) = pushRegisters (S.toList stainedCalleeSaveRegs)

    -- Main program returns 0 by default, but other functions do not.
    let mainDefaultReturn = if name /= "main" then Sq.empty else Sq.singleton
            (Move (Immediate $ ImmediateInt 0) (Register (RAX, B8)))

    let allStatements =
            Sq.fromList
                [ Label name
                , Push (Register (RBP, B8))
                , Move (Register (RSP, B8)) (Register (RBP, B8))] ><

            pushes ><
            maybeAlignDownRSP ><

            downRSP maxStackSize ><
                statements' ><
                Sq.singleton (Label $ name ++ ".return") ><
            upRSP maxStackSize ><

            maybeAlignUpRSP ><
            pops ><

            mainDefaultReturn ><
            Sq.fromList [Leave, Return]

    calledInternalFunctions <- gets calledInternalFunctions

    return (calledInternalFunctions, allStatements)

macro :: Seq Instruction
macro =
    Sq.fromList
    [
    IfDefined "__APPLE__",
        Define "section_read_only" "",
        Define "section_literal4" ".literal4",
        Define "section_cstring" ".cstring",
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

program :: IR.Program IR.NoControlFlowStatement -> Seq Instruction
program (IR.Program dataSegment functions) = let

    (unzip -> (calledInternalFunctionsSets, functionsStatements)) =
        (`evalState` initialState) . function <$> functions

    allCalledInternalFunctions = S.toList $ S.unions calledInternalFunctionsSets
    internalFunctionsStatements =
        (Internal.functions M.!) <$> allCalledInternalFunctions

    dataSegmentStatements =
        Sq.fromList (M.toList dataSegment) <&> \(name, number) ->
            Sq.fromList
                [ SectionReadOnly
                , SectionLiteral4
                , Int (length name)
                , SectionCString
                , Label ("str." ++ show number)
                , AsciiZero name
                ]

    addLineBreaks = intersperse (return EmptyLine)

    in

    (                    macro                        |> EmptyLine) ><
    (asum (addLineBreaks functionsStatements        ) |> EmptyLine) ><
    (asum (addLineBreaks internalFunctionsStatements) |> EmptyLine) ><
    (asum                dataSegmentStatements        |> EmptyLine)

initialState :: GeneratorState
initialState = GeneratorState {
    memoryTable = M.empty,
    registerPool = S.empty,
    stackPool = [],
    stainedCalleeSaveRegs = S.empty,
    maxStackSize = 0,
    tmpStackOffset = 0,
    tmpPushedRegs = [],
    functionName = undefined,
    calledInternalFunctions = S.empty
}

generateX64 :: IR.Program IR.NoControlFlowStatement -> Seq Instruction
generateX64 = program
