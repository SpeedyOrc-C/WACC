module WACC.Backend.X64.Generate where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Sq
import           Data.List
import           Data.Functor
import           Data.Bifunctor
import           Data.Traversable
import           Data.Foldable
import           Data.Sequence ((|>), (><), Seq)
import           Data.Function
import           Control.Monad

import qualified Control.Monad.Trans.StateEnv.Lazy as SE
import           Control.Monad.State
import           Control.Monad.StateEnv

import qualified WACC.Backend.X64.Internal as Internal
import qualified WACC.IR.Structure as IR
import           WACC.Backend.X64.Config
import           WACC.IR.Structure (Size(..), Identifier, sizeToInt)
import           WACC.Backend.X64.Structure
import           WACC.Backend.StackPool

-- Generator's state transformer
type GS = SE.State Config GeneratorState

{- This indicates the location of the data. Stored in registers,
   stored in the stack or stored in the parameter stack. -}
data MemoryLocation
    = AtRegister Register
    | AtStack Int Size
    | AtParameterStack Int Size
    deriving (Show)

parameter :: Int -> Size -> GS Register
parameter n size = do
    parameterRegisters <- sees parameterRegisters
    return $ maybe (error ("Cannot use parameter " ++ show n))
        (, size)
        (lookup n (zip [1..] parameterRegisters))

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

use :: Internal.Function -> GS ()
use f = modify $ \s -> s {
    calledInternalFunctions = S.insert f (calledInternalFunctions s)
}

{- This function is responsible for allocating memory for a variable.
   It first checks if there are any free registers available. If so,
   it allocates the variable to one of the available registers.
   If all registers are in use, it allocates memory on the stack. -}
allocate :: Identifier -> Size -> GS MemoryLocation
allocate var size = do
    availableRegisters <- sees availableRegisters
    registerPool <- gets registerPool
    if (case size of B {} -> False; _ -> True)
        && S.size registerPool < S.size availableRegisters
    then allocateOnRegister var size
    else allocateOnStack var size

allocateOnRegister :: Identifier -> Size -> GS MemoryLocation
allocateOnRegister var size = do
    availableRegisters <- sees availableRegisters
    calleeSaveRegisters <- sees calleeSaveRegisters
    rankRegister <- sees rankRegister
    registerPool <- gets registerPool
    memoryTable <- gets memoryTable
    stainedCalleeSaveRegs <- gets stainedCalleeSaveRegs

    let freeRegisters = availableRegisters S.\\ registerPool
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

allocateOnStack :: Identifier -> Size -> GS MemoryLocation
allocateOnStack var size = do
    memoryTable <- gets memoryTable
    stackPool <- gets stackPool
    maxStackSize <- gets maxStackSize

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
free :: Identifier -> GS ()
free name = do
    memoryTable <- gets memoryTable
    stackPool <- gets stackPool
    registerPool <- gets registerPool

    case memoryTable M.!? name of
        Just (AtRegister (physicalRegister, _)) -> do
            modify $ \s -> s {
                registerPool = S.delete physicalRegister registerPool,
                memoryTable = M.delete name memoryTable
            }

        Just (AtStack offset _) -> do
            modify $ \s -> s {
                stackPool = freeStack offset stackPool,
                memoryTable = M.delete name memoryTable
            }

        Just {} -> return ()

        Nothing -> error $ "Cannot free " ++ show name

operandFromMemoryLocation :: MemoryLocation -> GS Operand
operandFromMemoryLocation = \case
    AtRegister register@(reg, _) -> do
        tmpPushedRegs <- gets tmpPushedRegs
        return $ case elemIndex (Just reg) tmpPushedRegs of
            Nothing -> Register register
            Just offset -> MemoryIndirect
                (Just (ImmediateInt (offset * 8))) RSP Nothing

    -- Variable A
    -- Variable B  Minus the offset to get the variables on the stack.
    -- Variable C  ---------------------------------+
    -- Pushing registers before calling a function. |  temporary
    -- Pushing...                                   |   offset
    -- Pushing...  <- RSP points here. -------------+
    AtStack offset _ -> do
        tmpStackOffset <- gets tmpStackOffset
        return $ MemoryIndirect
            (Just (ImmediateInt (offset - tmpStackOffset))) RSP Nothing

    -- 9th parameter
    -- 8th parameter  Move up to get the parameters after 6th.
    -- 7th parameter  ----------------------------------+  2 registers
    -- return address                                   |   16 bytes
    -- pushed RBP     <- RBP points here in the body. --+
    AtParameterStack offset _ ->
        return $ MemoryIndirect
            (Just (ImmediateInt (offset + 16))) RBP Nothing

scalar :: IR.Scalar -> GS Operand
scalar = \case
    IR.Immediate n -> return $
        Immediate (ImmediateInt n)

    IR.Reference identifier ->
        error $ "should not use the scalar to get the operand of the " ++ show identifier

    IR.Variable identifier -> do
        memoryTable <- gets memoryTable
        case memoryTable M.!? identifier of
            Nothing -> error $ "cannot find" ++ show identifier
            Just x -> operandFromMemoryLocation x

    IR.String n ->
        return $ MemoryIndirect
            (Just $ ImmediateLabel ("str." ++ show n))
            RIP Nothing

{- This function determines which caller-save registers
   are currently being used. -}
usedCallerSaveRegisters :: GS [PhysicalRegister]
usedCallerSaveRegisters = do
    callerSaveRegisters <- sees callerSaveRegisters
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
push :: Maybe PhysicalRegister -> GS (Seq Instruction)
push maybeReg = do
    modify $ \s -> s {
        tmpStackOffset = tmpStackOffset s - 8,
        tmpPushedRegs = maybeReg : tmpPushedRegs s
    }
    return $ case maybeReg of
        Just reg -> Sq.singleton $ Push (Register (reg, B8))
        Nothing -> downRSP 8

pop :: Maybe PhysicalRegister -> GS (Seq Instruction)
pop maybeReg = do
    modify $ \s -> s {
        tmpStackOffset = tmpStackOffset s + 8,
        tmpPushedRegs = drop 1 (tmpPushedRegs s)
    }
    return $ case maybeReg of
        Just reg -> Sq.singleton $ Pop (Register (reg, B8))
        Nothing -> upRSP 8
{-let (argsReg, argsStack) = splitAt paramRegCount operands
        let (argsRegSizes, argsStackSizes) = splitAt paramRegCount sizes-}
getCallStackSize :: [(a,Size)] -> Int -> Int -> ([(a,Size)], [(a,Size)])
getCallStackSize [] _ _ = ([], [])
getCallStackSize (x@(_, B _):xs) k j= (argsReg', x:argsStack')
    where
        (argsReg', argsStack') = getCallStackSize xs k j
getCallStackSize (x:xs) k j
    | k < j = (x:argsReg', argsStack')
    | otherwise = (argsReg', x:argsStack')
    where
        (argsReg', argsStack') = getCallStackSize xs (k + 1) j


useManyTemporary :: [PhysicalRegister] -> GS (Seq Instruction)
    -> GS (Seq Instruction)
useManyTemporary regs = foldl (.) id (map useTemporary regs)

useTemporary :: PhysicalRegister -> GS (Seq Instruction)
    -> GS (Seq Instruction)
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

pairHelper :: IR.Scalar -> GS (Seq Instruction)
pairHelper s = do
    op <- scalar s

    use Internal.ErrorNull
    use Internal.PrintString

    return $ Sq.fromList
        [ Compare (Immediate $ ImmediateInt 0) op
        , JumpWhen Equal "error_pair_null"
        , Move op (Register (RAX, B8))]

malloc :: Int -> GS (Seq Instruction)
malloc size = expression (IR.Call B8 "malloc" [(B4, IR.Immediate size)])

getRefRegisters :: [IR.Scalar] -> GS [PhysicalRegister]
getRefRegisters [] = return []
getRefRegisters ((IR.Reference x):xs) = do
    callerSaveRegisters <- sees callerSaveRegisters
    xs' <- getRefRegisters xs
    operand <- scalar (IR.Variable x)
    case operand of
        (Register (reg, _)) ->
            if reg `elem` callerSaveRegisters
                then return xs'
                else return (reg:xs')
        _ ->
            return xs'
getRefRegisters (_:xs) = getRefRegisters xs

expression :: IR.Expression -> GS (Seq Instruction)
expression = \case
    IR.Scalar s -> do
        op <- scalar s
        let size = IR.getSize op
        return $ move size op (Register (RAX, IR.getSize op))
    IR.GetField num s -> do
        op <- scalar s
        return $ loadAddress (addToIndirect op num) (Register (RAX, B8))

    IR.NewStruct -> return Sq.empty
    IR.Not s -> do
        op <- scalar s
        return $ Sq.fromList
            [ Move op (Register (RAX, B1))
            , xor' (Immediate $ ImmediateInt 1) (Register (RAX, B1))]

    IR.Add s1 s2 -> do
        op1 <- scalar s1
        op2 <- scalar s2

        use Internal.ErrorOverflow
        use Internal.PrintString

        return $ Sq.fromList $ case (op1, op2) of
            (reg@Register {}, Immediate (ImmediateInt 1)) ->
                [ Move reg (Register (RAX, B4))
                , Increase (Register (RAX, B4))
                , JumpWhen Overflow "error_overflow"]

            (Immediate (ImmediateInt 1), reg@Register {}) ->
                [ Move reg (Register (RAX, B4))
                , Increase (Register (RAX, B4))
                , JumpWhen Overflow "error_overflow"]

            _ ->
                [ Move op1 (Register (RAX, B4))
                , add' op2 (Register (RAX, B4))
                , JumpWhen Overflow "error_overflow"]

    IR.Subtract s1 s2 -> do
        op1 <- scalar s1
        op2 <- scalar s2

        use Internal.ErrorOverflow
        use Internal.PrintString

        return $ Sq.fromList $ case (op1, op2) of
            (reg@Register {}, Immediate (ImmediateInt 1)) ->
                [ Move reg (Register (RAX, B4))
                , Decrease (Register (RAX, B4))
                , JumpWhen Overflow "error_overflow"]

            (Immediate (ImmediateInt 1), reg@Register {}) ->
                [ Move reg (Register (RAX, B4))
                , Decrease (Register (RAX, B4))
                , JumpWhen Overflow "error_overflow"]

            _ ->
                [ Move op1 (Register (RAX, B4))
                , subtract' op2 (Register (RAX, B4))
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

    IR.Divide a b -> useManyTemporary [RDX, RSI] $ do
        a' <- scalar a
        b' <- scalar b

        use Internal.ErrorDivideZero
        use Internal.PrintString

        return $ Sq.fromList
            [ Move (Immediate $ ImmediateInt 0) (Register (RDX, B8))
            , Move a' (Register (RAX, B4))
            , Move b' (Register (RSI, B4))
            , Compare (Immediate $ ImmediateInt 0) (Register (RSI, B4))
            , JumpWhen Equal "error_divide_zero"
            , CLTD
            , DivideI (Register (RSI, B4))]

    IR.Remainder a b -> useManyTemporary [RDX, RSI] $ do
        a' <- scalar a
        b' <- scalar b

        use Internal.ErrorDivideZero
        use Internal.PrintString

        return $ Sq.fromList
            [ Move (Immediate $ ImmediateInt 0) (Register (RDX, B8))
            , Move a' (Register (RAX, B4))
            , Move b' (Register (RSI, B4))
            , Compare (Immediate $ ImmediateInt 0) (Register (RSI, B4))
            , JumpWhen Equal "error_divide_zero"
            , CLTD
            , DivideI (Register (RSI, B4))
            , Move (Register (RDX, B4)) (Register (RAX, B4))]

    IR.GreaterEqual size s1 s2 -> do
        op1 <- scalar s1
        op2 <- scalar s2

        return $ Sq.fromList
            [ Move op1 (Register (RAX, size))
            , Compare op2 (Register (RAX, size))
            , set GreaterEqual (Register (RAX, B1))
            ]

    IR.Greater size s1 s2 -> do
        op1 <- scalar s1
        op2 <- scalar s2

        return $ Sq.fromList
            [ Move op1 (Register (RAX, size))
            , Compare op2 (Register (RAX, size))
            , set Greater (Register (RAX, B1))
            ]

    IR.LessEqual size s1 s2 -> do
        op1 <- scalar s1
        op2 <- scalar s2

        return $ Sq.fromList
            [ Move op1 (Register (RAX, size))
            , Compare op2 (Register (RAX, size))
            , set LessEqual (Register (RAX, B1))
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
            , Move (MemoryIndirect Nothing RAX Nothing)
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
                move fstSize opA (MemoryIndirect Nothing RDX Nothing) ><
                move sndSize opB (MemoryIndirect (Just (ImmediateInt 8)) RDX Nothing) ><
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
                    (MemoryIndirect (Just (ImmediateInt (-4))) RDX Nothing) ><

                asum (zip elements [0..] <&> \(element, index) ->
                    move size element $ MemoryIndirect
                        (Just (ImmediateInt (index * bytes)))
                        RDX Nothing
                ) ><

                move B8 (Register (RDX, B8)) (Register (RAX, B8))

        return $
            evaluateAddress ><
            return (Add (Immediate (ImmediateInt 4)) (Register (RAX, B8))) ><
            initialise

    IR.Call _ name (unzip -> (sizes, scalars)) -> do
        callerSaveRegistersToBePushed <- usedCallerSaveRegisters

        refRegisters <- getRefRegisters scalars
        let needAlignStack = odd $ length (callerSaveRegistersToBePushed ++ refRegisters)
        paramRegCount <- sees $ length . parameterRegisters

        (asum -> pushRegister) <- traverse (push . Just) (callerSaveRegistersToBePushed ++ refRegisters)

        maybeAlignDownRSP <- if needAlignStack then push Nothing else return Sq.empty

        let (unzip -> (argsReg, argsRegSizes), unzip -> (argsStack, argsStackSizes))
                = getCallStackSize (zip scalars sizes) 0 paramRegCount

        assignArgsReg <-
            for (zip3 [1..paramRegCount] argsRegSizes argsReg) $ \(n, size, arg) ->
                case arg of
                    (IR.Reference x) -> do
                        x' <- scalar (IR.Variable x)
                        p <- parameter n B8
                        return $ loadAddress x' (Register p)
                    _ -> do
                        x' <- scalar arg
                        p <- parameter n size
                        return $ move size x' (Register p)

        minSizeOfReservedStackForCallee <- sees minSizeOfReservedStackForCallee
        let allocateArgsStackSize = max
                (sum (IR.sizeToInt <$> argsStackSizes))
                minSizeOfReservedStackForCallee

        let allocateArgsStack = downRSP . ceil16 $ allocateArgsStackSize

        let argsStackOffsets = scanl (+) 0 (IR.sizeToInt <$> argsStackSizes)
        modify $ \s -> s {
            tmpStackOffset = tmpStackOffset s - allocateArgsStackSize
        }
        assignArgsStack <-
            for (zip3 argsStackOffsets argsStackSizes argsStack) $ \(offset, size, arg) -> do
                case arg of
                    IR.Reference x -> do
                        x' <- scalar (IR.Variable x)
                        return $ loadAddress x' (MemoryIndirect (Just (ImmediateInt offset)) RSP Nothing)
                    _ -> do
                        x' <- scalar arg
                        return $ move size x' (MemoryIndirect (Just (ImmediateInt offset)) RSP Nothing)

        modify $ \s -> s {
            tmpStackOffset = tmpStackOffset s + allocateArgsStackSize
        }
        let freeArgsStack = upRSP . ceil16 $ allocateArgsStackSize

        maybeAlignUpRSP <- if needAlignStack then pop Nothing else return Sq.empty
        (asum -> popCallerSave) <- traverse (pop . Just) (reverse (callerSaveRegistersToBePushed ++ refRegisters))

        return $
            pushRegister ><
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
            , Move (MemoryIndirect (Just $ ImmediateInt (-4)) RDX Nothing)
                    (Register (RAX, B4))
            ]

    IR.Order scalar' -> useTemporary RDX $ do
        scalar'' <- scalar scalar'
        return $ Sq.fromList
            [Move scalar'' (Register (RDX, B1)),
            moveSignSizeExtend B1 B4 (Register (RDX, B1)) (Register (RAX, B4))]

    IR.Character scalar' -> do
        scalar'' <- scalar scalar'

        use Internal.ErrorBadChar

        return $ Sq.fromList
            [ Move scalar'' (Register (RAX, B4))
            , moveSignSizeExtend B4 B8 (Register (RAX, B4)) (Register (RAX, B8))
            , test' (Immediate $ ImmediateInt $ -128) (Register (RAX, B8))
            , compareMove NotEqual (Register (RAX, B8)) (Register (RSI, B8))
            , JumpWhen NotEqual "error_bad_char"]

    IR.And a b -> do
        a' <- scalar a
        b' <- scalar b

        return $ Sq.fromList
            [ Move a' (Register (RAX, B1))
            , and' b' (Register (RAX, B1)) ]

    IR.Or a b -> do
        a' <- scalar a
        b' <- scalar b

        return $ Sq.fromList
            [ Move a' (Register (RAX, B1))
            , or' b' (Register (RAX, B1))]

    IR.ReadInt -> do
        use Internal.ReadInt
        expression $ IR.Call B1 "read_int" []

    IR.ReadChar k -> do
        use Internal.ReadChar
        expression $ IR.Call B1 "read_char" [(B1, k)]

singleStatement :: IR.SingleStatement -> GS (Seq Instruction)
singleStatement = \case
    IR.Exit s -> do
        op <- scalar s
        p1 <- parameter 1 B4
        return $ Sq.fromList [Move op (Register p1), Call "exit"]

    IR.FunctionReturnStruct t' to fn args -> do
        expression $ IR.Call t' fn ((B8, IR.Reference to):args)

    IR.Assign size ident IR.NewStruct -> do
        _ <- allocate ident size
        return Sq.empty

    IR.Assign size to from -> do
        memoryTable <- gets memoryTable
        evaluate <- expression from
        let size' = case size of (B _) -> B8; x -> x
        case memoryTable M.!? to of
            Just location -> do
                to' <- operandFromMemoryLocation location
                return $ evaluate >< move size (Register (RAX, size')) to'

            Nothing -> do
                location <- allocate to size
                to' <- operandFromMemoryLocation location
                return $ evaluate >< move size (Register (RAX, size')) to'

    IR.AssignIndirect size to from -> do
        memoryTable <- gets memoryTable
        evaluate <- expression from

        case memoryTable M.!? to of
            Just (AtRegister (reg, _)) ->
                return $ evaluate ><
                    move size (Register (RAX, size)) (MemoryIndirect Nothing reg Nothing)

            Just (AtStack offset _) -> useTemporary RDX $ do
                tmpStackOffset <- gets tmpStackOffset
                return $ evaluate ><
                    move size (MemoryIndirect
                            (Just (ImmediateInt (offset - tmpStackOffset)))
                            RSP
                            Nothing)
                        (Register (RDX, B8)) ><
                    move size (Register (RAX, size)) (MemoryIndirect Nothing RDX Nothing)

            Just (AtParameterStack offset _) -> useTemporary RDX $
                return $ evaluate ><
                    move size (MemoryIndirect
                            (Just (ImmediateInt (offset + 16))) RBP Nothing)
                        (Register (RDX, B8)) ><
                    move size (Register (RAX, size)) (MemoryIndirect Nothing RDX Nothing)

            Nothing -> error $ "Cannot assign to " ++ show to

    IR.PrintString s -> do
        use Internal.PrintString
        expression $ IR.Call B8 "print_string" [(B8, s)]

    IR.PrintInt s -> do
        use Internal.PrintInt
        expression $ IR.Call B8 "print_int" [(B4, s)]

    IR.PrintBool s -> do
        use Internal.PrintBool
        expression $ IR.Call B8 "print_bool" [(B1, s)]

    IR.PrintLineBreak -> do
        use Internal.PrintLineBreak
        expression $ IR.Call B8 "print_line_break" []

    IR.PrintAddress s -> do
        use Internal.PrintPointer
        use Internal.PrintString

        expression $ IR.Call B8 "print_pointer" [(B8, s)]

    IR.Return size@(B _) s -> do
        op <- scalar s
        name <- gets functionName

        return $ move size op (Register (RDI, B8)) >< Sq.fromList
            [Jump . ImmediateLabel $ name ++ ".return"]

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
            [Compare (Immediate $ ImmediateInt 0) op, JumpWhen Equal "error_free_null"]
            >< callFree

    IR.FreeArray s -> useManyTemporary [RDX, RDI] $ do
        op <- scalar s
        call <- expression (IR.Call B8 "free" [])
        return $
            Sq.fromList
            [Move op (Register (RDX, B8)),
            subtract' (Immediate $ ImmediateInt 4) (Register (RDX, B8)),
            Move (Register (RDX, B8)) (Register (RDI, B8))]
            >< call

    IR.PrintChar s -> do
        use Internal.PrintChar
        expression (IR.Call B8 "print_char" [(B1, s)])

instruction :: IR.NoControlFlowStatement -> GS (Seq Instruction)
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

instructions :: [IR.NoControlFlowStatement] -> GS (Seq Instruction)
instructions = fmap asum . traverse instruction

function :: IR.Function IR.NoControlFlowStatement
    -> GS (S.Set Internal.Function, Seq Instruction)
function (IR.Function x name parameters statements) = do
    modify $ \s -> s { functionName = name }

    startRegisterInt <- case x of
        B {} -> do
            modify $ \s -> s {
                registerPool = S.insert RDI (registerPool s)
            }
            return 2
        _ -> return 1
    paramRegCount <- sees $ length . parameterRegisters
    let (registerParams, stackParams) =
            getCallStackSize parameters (startRegisterInt - 1) paramRegCount

    for_ (zip [startRegisterInt..] registerParams) $ \(i, (ident, size)) -> do
        reg@(physicalReg, _) <- parameter i size
        modify $ \s -> s {
            memoryTable = M.insert ident (AtRegister reg) (memoryTable s),
            registerPool = S.insert physicalReg (registerPool s)
        }

    -- e.g. If there are 4 parameters after the 6th,
    -- and their sizes are:  [8, 1, 4, 1 ].
    -- Then the offsets are: [0, 8, 9, 13].
    let stackParamOffsets = scanl (+) 0 (sizeToInt . snd <$> stackParams)

    for_ (zip stackParamOffsets stackParams) $ \(offset, (ident, size)) -> do
        modify $ \s -> s {
            memoryTable = M.insert ident (AtParameterStack offset size) (memoryTable s)
        }

    statements' <- instructions statements

    stainedCalleeSaveRegs <- gets stainedCalleeSaveRegs
    (ceil16 -> maxStackSize) <- gets maxStackSize

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

program :: Config -> IR.Program IR.NoControlFlowStatement -> Seq Instruction
program config (IR.Program dataSegment functions) = let

    (unzip -> (calledInternalFunctionsSets, functionsStatements)) =
        [SE.evalState (function f) config initialState | f <- functions]

    -- allCalledInternalFunctions = S.toList $ S.unions calledInternalFunctionsSets
    -- internalFunctionsStatements =
    --     (internalFunctions config M.!) <$> allCalledInternalFunctions

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

    (Sq.fromList (              macro config               ) |> EmptyLine) ><
    (       asum (addLineBreaks functionsStatements        ) |> EmptyLine) ><
    -- (       asum (addLineBreaks internalFunctionsStatements) |> EmptyLine) ><
    (       asum                dataSegmentStatements        |> EmptyLine)

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

generateX64 :: Config -> IR.Program IR.NoControlFlowStatement -> Seq Instruction
generateX64 = program
