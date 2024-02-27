module WACC.IR.FlattenExpression where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Char
import           Data.Maybe
import           Data.Functor.Identity
import           Control.Monad
import           Control.Monad.Trans.State.Lazy

import qualified WACC.Semantics.Structure as SM
import           WACC.IR.Structure

lookUp :: Ord k => k -> [M.Map k a] -> a
lookUp _ [] = error "Semantic check has failed!"
lookUp k (m:ms) = lookUp k ms `fromMaybe` (m M.!? k)

data FlattenerState = FlattenerState {
    mappingStack :: [String `M.Map` Identifier],
    variableCounter :: Int,
    dataSegment :: String `M.Map` Int
} deriving Show

initialState :: String `M.Map` Int -> FlattenerState
initialState ds = FlattenerState {
    mappingStack = [],
    variableCounter = M.size ds + 1,
    dataSegment = ds
}

nextVariable :: FlattenerState -> FlattenerState
nextVariable s = s { variableCounter = variableCounter s + 1 }

newTemporary :: State FlattenerState Identifier
newTemporary = do
    number <- gets variableCounter
    modify nextVariable
    return $ Temporary "var" number

newVariable :: String -> State FlattenerState Identifier
newVariable name = do
    number <- gets variableCounter
    modify nextVariable
    return $ Identifier name number

newParameter :: String -> State FlattenerState Identifier
newParameter name = do
    number <- gets variableCounter
    modify nextVariable
    return $ Parameter name number

expressions :: [SM.Expression] -> State FlattenerState ([Scalar], [SingleStatement])
expressions xs = do
    (unzip -> (scalars, concat -> evaluate)) <- traverse expression xs
    return (scalars, evaluate)

expression :: SM.Expression -> State FlattenerState (Scalar, [SingleStatement])
expression = \case
    SM.Identifier _ name -> do
        identifier <- gets $ lookUp name . mappingStack
        return (Variable identifier, [])

    SM.LiteralBool bool ->
        return (Immediate (if bool then 1 else 0), [])

    SM.LiteralInt int ->
        return (Immediate int, [])

    SM.LiteralChar char ->
        return (Immediate (ord char), [])

    SM.LiteralString str -> do
        number <- gets $ (M.! str) . dataSegment
        return (String number, [])

    SM.LiteralArray t xs -> do
        (scalars, evaluate) <- expressions xs
        tmp <- newTemporary
        return (Variable tmp,
            evaluate ++
            [Assign B8 tmp (NewArray (getSize t) scalars)])

    e@(SM.ArrayElement t _ _) -> do
        (result, evaluate) <- indirectExpression e
        tmp <- newTemporary
        return (Variable tmp,
            evaluate ++ [Assign (getSize t) tmp (Dereference result)])

    SM.LiteralPairNull ->
        return (Immediate 0, [])

    SM.LiteralPair (getSize -> sizeA, getSize -> sizeB) (a, b) -> do
        (a', evaluateA) <- expression a
        (b', evaluateB) <- expression b
        tmp <- newTemporary
        return (Variable tmp,
            evaluateA ++ evaluateB ++
            [Assign B8 tmp (NewPair (sizeA, sizeB) (a', b'))])

    e@(SM.PairFirst t _) -> do
        (pair, evaluatePair) <- indirectExpression e
        tmp <- newTemporary
        return (Variable tmp,
            evaluatePair ++ [Assign (getSize t) tmp (Dereference pair)])

    e@(SM.PairSecond t _) -> do
        (pair, evaluatePair) <- indirectExpression e
        tmp <- newTemporary
        return (Variable tmp,
            evaluatePair ++ [Assign (getSize t) tmp (Dereference pair)])

    SM.Not       e -> unary B1 Not       e
    SM.Negate    e -> unary B4 Negate    e
    SM.Length    e -> unary B4 Length    e
    SM.Order     e -> unary B4 Order     e
    SM.Character e -> unary B1 Character e

    SM.Multiply  a b -> binary B4 Multiply  a b
    SM.Divide    a b -> binary B4 Divide    a b
    SM.Remainder a b -> binary B4 Remainder a b
    SM.Add       a b -> binary B4 Add       a b
    SM.Subtract  a b -> binary B4 Subtract  a b

    SM.Greater      SM.CompareInt a b -> binary B1 (Greater      B4) a b
    SM.GreaterEqual SM.CompareInt a b -> binary B1 (GreaterEqual B4) a b
    SM.Less         SM.CompareInt a b -> binary B1 (Less         B4) a b
    SM.LessEqual    SM.CompareInt a b -> binary B1 (LessEqual    B4) a b

    SM.Greater      SM.CompareChar a b -> binary B1 (Greater      B1) a b
    SM.GreaterEqual SM.CompareChar a b -> binary B1 (GreaterEqual B1) a b
    SM.Less         SM.CompareChar a b -> binary B1 (Less         B1) a b
    SM.LessEqual    SM.CompareChar a b -> binary B1 (LessEqual    B1) a b

    SM.Equal    t a b -> binary B1 (Equal    (getSize t)) a b
    SM.NotEqual t a b -> binary B1 (NotEqual (getSize t)) a b

    SM.And a b -> binary B1 And a b
    SM.Or  a b -> binary B1 Or  a b

    SM.FunctionCall t f (unzip -> (ts, args)) -> do
        (scalars, evaluate) <- expressions args
        tmp <- newTemporary
        return (Variable tmp,
            evaluate ++
            [Assign (getSize t) tmp (Call (getSize t) ("wacc_" ++ f) ((getSize <$> ts) `zip` scalars))])
    where
    unary :: Size -> (Scalar -> Expression) -> SM.Expression
                -> State FlattenerState (Scalar, [SingleStatement])
    unary size f a = do
        (a', evaluateA) <- expression a
        tmp <- newTemporary
        return (Variable tmp,
            evaluateA ++ [Assign size tmp (f a')])

    binary :: Size -> (Scalar -> Scalar -> Expression)
                -> SM.Expression -> SM.Expression
                -> State FlattenerState (Scalar, [SingleStatement])
    binary size f a b = do
        (a', evaluateA) <- expression a
        (b', evaluateB) <- expression b
        tmp <- newTemporary
        return (Variable tmp,
            evaluateA ++ evaluateB ++ [Assign size tmp (f a' b')])

indirectExpression ::
    SM.Expression -> State FlattenerState (Scalar, [SingleStatement])
indirectExpression = \case
    i@(SM.Identifier {}) -> expression i

    SM.ArrayElement t array index -> do
        (index', evaluateIndex) <- expression index
        (array', evaluateElementAddress) <- indirectExpression array
        tmp <- newTemporary
        return (Variable tmp,
            evaluateIndex ++ evaluateElementAddress ++
            [Assign B8 tmp (SeekArrayElement (getSize t) array' index')])

    SM.PairFirst _ pair@(SM.Identifier {}) -> do
        (pair', evaluatePair) <- indirectExpression pair
        tmp <- newTemporary
        return (Variable tmp,
            evaluatePair ++
            [Assign B8 tmp (SeekPairFirst pair')])

    SM.PairSecond _ pair@(SM.Identifier {}) -> do
        (pair', evaluatePair) <- indirectExpression pair
        tmp <- newTemporary
        return (Variable tmp,
            evaluatePair ++
            [Assign B8 tmp (SeekPairSecond pair')])

    SM.PairFirst _ pair -> do
        (address, evaluatePair) <- indirectExpression pair
        value <- newTemporary
        tmp <- newTemporary
        return (Variable value,
            evaluatePair ++
            [ Assign B8 value (Dereference address)
            , Assign B8 tmp (SeekPairFirst (Variable value))])

    SM.PairSecond _ pair -> do
        (address, evaluatePair) <- indirectExpression pair
        value <- newTemporary
        tmp <- newTemporary
        return (Variable tmp,
            evaluatePair ++
            [ Assign B8 value (Dereference address)
            , Assign B8 tmp (SeekPairSecond (Variable value))])

    _ -> error "Semantic check has failed."

statement :: SM.Statement -> State FlattenerState [NoExpressionStatement]
statement = \case
    SM.Declare t name rightValue -> do
        (result, evaluation) <- expression rightValue
        identifier <- newVariable name
        modify $ \s -> s {
            mappingStack = M.insert name identifier
                (head (mappingStack s)) : tail (mappingStack s)
        }
        return $ map NE evaluation ++
            [NE $ Assign (getSize t) identifier (Scalar result)]

    SM.Assign _ (SM.Identifier t name) rightValue -> do
        identifier <- gets $ lookUp name . mappingStack
        (result, evaluation) <- expression rightValue
        return $ map NE evaluation ++
            [NE $ Assign (getSize t) identifier (Scalar result)]

    SM.Assign t leftValue rightValue -> do
        (result, evaluateRight) <- expression rightValue
        (scalar, evaluateLeft) <- indirectExpression leftValue
        let Variable identifier = scalar
        return $ map NE evaluateRight ++ map NE evaluateLeft ++
            [NE $ AssignIndirect (getSize t) identifier (Scalar result)]

    SM.If condition thenClause elseClause -> do
        (condition', evaluateCondition) <- expression condition
        thenClause' <- block thenClause
        elseClause' <- block elseClause
        return $ map NE evaluateCondition ++
            [If condition' thenClause' elseClause']

    SM.While condition body -> do
        preWhile@(condition', evaluateCondition) <- expression condition
        body' <- block body
        possibleFreeVariables <- gets $
            S.fromList . map snd . concatMap M.toList . mappingStack
        return [While preWhile body'
                ((reference condition' `S.union`
                  reference evaluateCondition `S.union`
                  reference body')
                    `S.intersection` possibleFreeVariables)]

    SM.Scope b -> do
        b' <- block b
        modify $ \s -> s { mappingStack = tail $ mappingStack s }
        return b'

    SM.Return e -> do
        (result, evaluateExpression) <- expression e
        return $ map NE evaluateExpression ++
            [NE $ Return result]

    SM.Exit e -> do
        (result, evaluateExpression) <- expression e
        return $ map NE evaluateExpression ++
            [NE $ Exit result]

    SM.Print t e -> do
        (result, evaluateExpression) <- expression e
        return $ map NE evaluateExpression ++
            [NE $ printByType t result]

    SM.PrintLine t e -> do
        (result, evaluateExpression) <- expression e
        return $ map NE evaluateExpression ++
            map NE [printByType t result, PrintLineBreak]

    SM.Read _ (SM.Identifier t name) -> do
        var <- gets $ lookUp name . mappingStack
        return [NE $ Assign (getSize t) var (readByType t)]

    SM.Read _ e -> do
        (scalar, evaluateExpression) <- indirectExpression e
        let Variable identifier = scalar
        return $ map NE evaluateExpression ++
            [NE $ AssignIndirect (getSize SM.Int) identifier ReadInt]

    SM.Free e -> do
        (scalar, evaluateExpression) <- indirectExpression e
        return $ map NE evaluateExpression ++
            [NE $ Free scalar]

    where
    printByType = \case
        SM.Bool -> PrintBool
        SM.Int -> PrintInt
        SM.Char -> PrintChar
        SM.String -> PrintString
        SM.Array {} -> PrintAddress
        SM.Pair {} -> PrintAddress
        _ -> error "Semantic check has failed."

    readByType = \case
        SM.Int -> ReadInt
        SM.Char -> ReadChar
        _ -> error "Semantic check has failed."

statements :: [SM.Statement] -> State FlattenerState [NoExpressionStatement]
statements xs = concat <$> traverse statement xs

block :: SM.Block -> State FlattenerState [NoExpressionStatement]
block (SM.Block xs) = do
    modify $ \s -> s { mappingStack = M.empty : mappingStack s }
    statements xs

class HasReference a where
    reference :: a -> S.Set Identifier

instance HasReference a => HasReference [a] where
    reference :: HasReference a => [a] -> S.Set Identifier
    reference = S.unions . map reference

instance HasReference Scalar where
    reference :: Scalar -> S.Set Identifier
    reference = \case
        (Variable var@(Identifier {})) -> S.singleton var
        (Variable var@(Parameter {})) -> S.singleton var
        _ -> S.empty

instance HasReference Expression where
    reference :: Expression -> S.Set Identifier
    reference = \case
        Scalar s -> reference s

        Not a -> reference a
        Negate a -> reference a
        Length a -> reference a
        Order a -> reference a
        Character a -> reference a

        Multiply a b -> reference a `S.union` reference b
        Divide a b -> reference a `S.union` reference b
        Remainder a b -> reference a `S.union` reference b
        Add a b -> reference a `S.union` reference b
        Subtract a b -> reference a `S.union` reference b

        Greater _ a b -> reference a `S.union` reference b
        GreaterEqual _ a b -> reference a `S.union` reference b
        Less _ a b -> reference a `S.union` reference b
        LessEqual _ a b -> reference a `S.union` reference b

        Equal _ a b -> reference a `S.union` reference b
        NotEqual _ a b -> reference a `S.union` reference b

        And a b -> reference a `S.union` reference b
        Or a b -> reference a `S.union` reference b

        NewArray _ xs -> reference xs
        SeekArrayElement _ a i -> reference a `S.union` reference i

        NewPair _ (a, b) -> reference a `S.union` reference b
        SeekPairFirst p -> reference p
        SeekPairSecond p -> reference p

        Dereference s -> reference s

        Call _ _ args -> reference $ snd <$> args
        ReadInt -> S.empty
        ReadChar -> S.empty

instance HasReference SingleStatement where
    reference :: SingleStatement -> S.Set Identifier
    reference = \case
        Assign _ var@(Identifier {}) e -> var `S.insert` reference e
        Assign _ _ e -> reference e
        AssignIndirect _ _ e -> reference e
        Return e -> reference e
        Exit e -> reference e
        Free e -> reference e

        PrintBool e -> reference e
        PrintInt e -> reference e
        PrintChar e -> reference e
        PrintString e -> reference e
        PrintAddress e -> reference e
        PrintLineBreak -> S.empty

instance HasReference NoExpressionStatement where
    reference :: NoExpressionStatement -> S.Set Identifier
    reference = \case
        NE s -> reference s

        While (condition, evaluateCondition) body _ ->
            reference body `S.union`
            reference evaluateCondition `S.union`
            reference condition

        If condition thenClause elseClause ->
            S.unions
                [ reference thenClause
                , reference elseClause
                , reference condition]

function :: SM.Function -> State FlattenerState (Function NoExpressionStatement)
function (SM.Function _ functionName params@(unzip -> (names, types)) b) = do
    oldState <- get

    firstParameterNo <- gets variableCounter

    (map fst -> identifiers) <- forM params $ \(name, t) -> do
        identifier <- newParameter name
        return (identifier, getSize t)

    modify $ \s -> s {
        mappingStack = M.fromList (names `zip` identifiers) : mappingStack s
    }

    b' <- block b

    put oldState

    return $ Function
        functionName
        (identifiers `zip` map getSize types)
        b'

functions :: [SM.Function] -> State FlattenerState [Function NoExpressionStatement]
functions = traverse function

program :: SM.Program -> State FlattenerState (Program NoExpressionStatement)
program (SM.Program fs main) = do
    dataSegment <- gets dataSegment
    let allFunctions 
            = SM.Function SM.Int "main" [] main 
                : map 
                    (\(SM.Function ret name param b) ->
                        SM.Function ret ("wacc_" ++ name) param b) 
                    (S.toList fs)
    functions' <- functions allFunctions
    return $ Program dataSegment functions'

flattenExpression :: (String `M.Map` Int, SM.Program) -> Program NoExpressionStatement
flattenExpression (dataSegment, p) = runIdentity $ evalStateT
    (program p) (initialState dataSegment)
