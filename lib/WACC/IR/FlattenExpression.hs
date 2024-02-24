module WACC.IR.FlattenExpression where

import qualified Data.Map as M
import qualified Data.Set as S
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
nextVariable state = state { variableCounter = variableCounter state + 1 }

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

    SM.LiteralString str -> do
        number <- gets $ (M.! str) . dataSegment
        return (String number, [])

    SM.Add a b -> do
        (a', evaluateA) <- expression a
        (b', evaluateB) <- expression b
        tmp <- newTemporary
        return (Variable tmp,
            evaluateA ++ evaluateB ++ [Assign B4 tmp (Add a' b')])

    SM.LiteralArray t xs -> do
        (scalars, evaluate) <- expressions xs
        tmp <- newTemporary
        return (Variable tmp,
            evaluate ++
            [Assign B8 tmp (NewArray (getSize t) scalars)])

    SM.FunctionCall t f (unzip -> (ts, args)) -> do
        (scalars, evaluate) <- expressions args
        tmp <- newTemporary
        return (Variable tmp,
            evaluate ++
            [Assign (getSize t) tmp (Call f ((getSize <$> ts) `zip` scalars))])

indirectExpression :: SM.Expression -> State FlattenerState (Scalar, [SingleStatement])
indirectExpression = \case
    i@(SM.Identifier {}) -> expression i

    SM.ArrayElement _ array index -> do
        (index', evaluateIndex) <- expression index
        (array', evaluateElementAddress) <- indirectExpression array
        tmp <- newTemporary
        return (Variable tmp,
            evaluateIndex ++ evaluateElementAddress ++
            [Assign B8 tmp (SeekArrayElement array' index')])

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

    SM.Assign t (SM.Identifier _ name) rightValue -> do
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

    SM.While condition body -> do
        (condition', evaluateCondition) <- expression condition
        body' <- block body
        possibleFreeVariables <- gets $
            S.fromList . map snd . concatMap M.toList . mappingStack
        return $ map NE evaluateCondition ++
            [While condition' body' (reference body' `S.intersection` possibleFreeVariables)]

    SM.Scope b -> do
        b' <- block b
        modify $ \s -> s { mappingStack = tail $ mappingStack s }
        return b'

    SM.Return e -> do
        (result, evaluateExpression) <- expression e
        return $ map NE evaluateExpression ++
            [NE $ Return result]

    _ -> undefined

statements :: [SM.Statement] -> State FlattenerState [NoExpressionStatement]
statements xs = concat <$> traverse statement xs

block :: SM.Block -> State FlattenerState [NoExpressionStatement]
block (SM.Block xs) = do
    modify $ \s -> s { mappingStack = M.empty : mappingStack s }
    statements xs

class FlattenExpression a b where
    flatten :: FlattenerState -> a -> (FlattenerState, b)

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
        Add a b -> reference a `S.union` reference b
        NewArray _ xs -> reference xs
        SeekArrayElement a i -> reference a `S.union` reference i
        Call _ args -> reference $ snd <$> args

instance HasReference SingleStatement where
    reference :: SingleStatement -> S.Set Identifier
    reference = \case
        Assign _ var@(Identifier {}) e -> var `S.insert` reference e
        Assign _ _ e -> reference e
        AssignIndirect _ _ e -> reference e
        Return e -> reference e
        Print e -> reference e

instance HasReference NoExpressionStatement where
    reference :: NoExpressionStatement -> S.Set Identifier
    reference = \case
        NE s -> reference s

        While condition body _ -> reference body `S.union` reference condition

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
        ([firstParameterNo..] `zip` map getSize types)
        b'

functions :: [SM.Function] -> State FlattenerState [Function NoExpressionStatement]
functions = traverse function

program :: SM.Program -> State FlattenerState (Program NoExpressionStatement)
program (SM.Program fs main) = do
    dataSegment <- gets dataSegment
    let allFunctions = SM.Function SM.Int "main" [] main : S.toList fs
    functions' <- functions allFunctions
    return $ Program dataSegment functions'

flattenExpression :: (String `M.Map` Int, SM.Program) -> Program NoExpressionStatement
flattenExpression (dataSegment, p) = runIdentity $ evalStateT
    (program p) (initialState dataSegment)
