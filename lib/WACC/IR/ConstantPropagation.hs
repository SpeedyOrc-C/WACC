module WACC.IR.ConstantPropagation where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Foldable
import           Control.Arrow
import           Control.Monad.Trans.State.Lazy

import qualified WACC.Graph as G
import           WACC.IR.Structure
import           WACC.Syntax.Error (intLowerBound, intUpperBound)
import           WACC.IR.FlattenExpression (HasReference(reference))

{- |
This defines the state used during constant propagation. 
It contains a mapping from identifiers to maybe constant integer values. 
-}
newtype PropagatorState = PropagatorState {
    constantMapping :: Identifier `M.Map` Maybe Int
}

{- |
Evaluate a unary operation without any condition on the operand. 
-}
evaluateUnary :: Scalar -> (Int -> Int) -> State PropagatorState (Maybe Int)
evaluateUnary a f = do
    a' <- scalar a
    case a' of
        Just x -> return $ Just $ f x
        _ -> return Nothing

{- |
Evaluate a unary operation with a condition on the operand. 
-}
evaluateUnary' :: Scalar -> (Int -> Int) -> (Int -> Bool)
    -> State PropagatorState (Maybe Int)
evaluateUnary' a f condition = do
    a' <- scalar a
    case a' of
        Just x | condition x -> return $ Just $ f x
        _ -> return Nothing

{- |
Evaluate a binary operation without any condition on the operands. 
-}
evaluateBinary :: Scalar -> Scalar -> (Int -> Int -> Int)
    -> State PropagatorState (Maybe Int)
evaluateBinary a b f = do
    a' <- scalar a
    b' <- scalar b
    case (a', b') of
        (Just x, Just y) -> return $ Just $ f x y
        _ -> return Nothing

{- |
Evaluate a binary operation with a condition on the operand. 
-}
evaluateBinary' :: Scalar -> Scalar
    -> (Int -> Int -> Int) -> (Int -> Int -> Bool)
    -> State PropagatorState (Maybe Int)
evaluateBinary' a b f condition = do
    a' <- scalar a
    b' <- scalar b
    case (a', b') of
        (Just x, Just y) | condition x y -> return $ Just $ f x y
        _ -> return Nothing

{- |
A helper function which extracts the value from a scalar. 
-}
scalar :: Scalar -> State PropagatorState (Maybe Int)
scalar = \case
    Immediate x -> return $ Just x

    Variable var -> do
        mapping <- gets constantMapping
        return $ mapping M.! var

    _ -> return Nothing

-- | Function for evaluating a mod function.
waccMod :: Int -> Int -> Int
waccMod x y = signum x * (abs x `mod` abs y)

{- |
This function evaluates expressions, such as arithmetic operations,
comparisons, logical operations, etc., and returns possibly 
constant integer values. 
-}
expression :: Expression -> State PropagatorState (Maybe Int)
expression = \case
    Scalar s -> scalar s

    Order e -> scalar e
    Character c -> evaluateUnary' c fromEnum (\x -> 0 <= x && x <= 127)

    Add a b -> evaluateBinary' a b (+)
            (\x y -> let n = x + y in intLowerBound <= n && n <= intUpperBound)
    Subtract a b -> evaluateBinary' a b (-)
            (\x y -> let n = x - y in intLowerBound <= n && n <= intUpperBound)
    Multiply a b -> evaluateBinary' a b (*)
            (\x y -> let n = x * y in intLowerBound <= n && n <= intUpperBound)

    Divide a b -> evaluateBinary' a b div (\_ y -> y /= 0)
    Remainder a b -> evaluateBinary' a b waccMod (\_ y -> y /= 0)

    Greater      _ a b -> evaluateBinary a b (\x y -> if x > y  then 1 else 0)
    GreaterEqual _ a b -> evaluateBinary a b (\x y -> if x >= y then 1 else 0)
    Less         _ a b -> evaluateBinary a b (\x y -> if x < y  then 1 else 0)
    LessEqual    _ a b -> evaluateBinary a b (\x y -> if x <= y then 1 else 0)
    Equal        _ a b -> evaluateBinary a b (\x y -> if x == y then 1 else 0)
    NotEqual     _ a b -> evaluateBinary a b (\x y -> if x /= y then 1 else 0)

    Not e   -> evaluateUnary  e   (\x   -> if x == 0           then 1 else 0)
    And a b -> evaluateBinary a b (\x y -> if x /= 0 && y /= 0 then 1 else 0)
    Or  a b -> evaluateBinary a b (\x y -> if x /= 0 || y /= 0 then 1 else 0)

    _ -> return Nothing

{- |
This helper function simplifies a scalar to a single statement. 
-}
simplifyStatement :: (Scalar -> SingleStatement)
    -> Scalar -> State PropagatorState SingleStatement
simplifyStatement constructor s = do
    s' <- scalar s
    case s' of
        Just c -> return $ constructor $ Immediate c
        _ -> return $ constructor s

{- |
This function processes single statements by 
replacing scalar values with constants if possible. 
-}
singleStatement :: SingleStatement
    -> State PropagatorState SingleStatement
singleStatement = \case
    Assign size var e -> do
        e' <- expression e
        case e' of
            Nothing -> do
                modify $ \s -> s {
                    constantMapping = M.insert var Nothing (constantMapping s)
                }
                return $ Assign size var e
            Just c -> do
                modify $ \s -> s {
                    constantMapping = M.insert var (Just c) (constantMapping s)
                }
                return $ Assign size var (Scalar $ Immediate c)

    Exit         s -> simplifyStatement Exit         s
    PrintInt     s -> simplifyStatement PrintInt     s
    PrintBool    s -> simplifyStatement PrintBool    s
    PrintChar    s -> simplifyStatement PrintChar    s
    PrintString  s -> simplifyStatement PrintString  s
    PrintAddress s -> simplifyStatement PrintAddress s

    s -> return s

{- |
This function handles if statements, evaluating conditions 
and propagating constants through both the then and else clauses. 
-}
statementIf :: Scalar -> [NoExpressionStatement] -> [NoExpressionStatement]
    -> State PropagatorState [NoExpressionStatement]
statementIf condition thenClause elseClause = do
    -- Constants may have different values in "then" and "else" branches.
    -- Also, the initial state for both branches must be the same.

    beforeIfState <- get

    thenClause' <- traverse statement thenClause
    thenMapping <- gets constantMapping

    put beforeIfState

    elseClause' <- traverse statement elseClause
    elseMapping <- gets constantMapping

    -- Some constants still have the same values in both branches.
    -- So only keep these kind of constants.

    let mergedMapping =
            (M.intersectionWith $ \a b -> case (a, b) of
                (Just x, Just y) | x == y -> Just x
                _ -> Nothing
            ) thenMapping elseMapping

    modify $ \s -> s { constantMapping = mergedMapping }

    return [If condition (concat thenClause') (concat elseClause')]

{- |
This class defines functions to extract dependencies 
between variables and statements.
-}
class HasDependency a where
    getDependency :: a -> [(Identifier, S.Set Identifier)]

instance HasDependency a => HasDependency [a] where
    getDependency :: HasDependency a => [a] -> [(Identifier, S.Set Identifier)]
    getDependency = concatMap getDependency

instance HasDependency SingleStatement where
    getDependency :: SingleStatement -> [(Identifier, S.Set Identifier)]
    getDependency = \case
        Assign _ var e -> [(var, reference e)]
        _ -> []

instance HasDependency NoExpressionStatement where
    getDependency :: NoExpressionStatement -> [(Identifier, S.Set Identifier)]
    getDependency = \case
        NE s -> getDependency s

        If condition thenClause elseClause -> do
            -- The condition decides which branch to take.
            -- So all definitions in both branches depend on the condition.

            let innerDependencies =
                    getDependency thenClause ++ getDependency elseClause

            case condition of
                Variable var -> map (second (S.insert var)) innerDependencies
                _ -> innerDependencies

        While (condition, evaluateCondition) body _ -> do
            -- The condition decides whether to execute the loop.
            -- So all definitions in the loop depend on the condition.

            let innerDependencies =
                    getDependency evaluateCondition ++ getDependency body

            case condition of
                Variable var -> map (second (S.insert var)) innerDependencies
                _ -> innerDependencies

class HasDependencyOnInput a where
    getDependencyOnInput :: a -> S.Set Identifier

instance HasDependencyOnInput a => HasDependencyOnInput [a] where
    getDependencyOnInput :: HasDependencyOnInput a => [a] -> S.Set Identifier
    getDependencyOnInput = S.unions . map getDependencyOnInput

instance HasDependencyOnInput SingleStatement where
    getDependencyOnInput :: SingleStatement -> S.Set Identifier
    getDependencyOnInput = \case
        Assign _ var ReadInt -> S.singleton var
        Assign _ var ReadChar {} -> S.singleton var
        _ -> S.empty

instance HasDependencyOnInput NoExpressionStatement where
    getDependencyOnInput :: NoExpressionStatement -> S.Set Identifier
    getDependencyOnInput = \case
        NE s -> getDependencyOnInput s

        If _ thenClause elseClause ->
            getDependencyOnInput thenClause `S.union`
            getDependencyOnInput elseClause

        While _ body _ -> getDependencyOnInput body

{- |
This function handles while loops, evaluating loop conditions 
and propagating constants through the loop body. 
-}
statementWhile ::
    [SingleStatement] -> [NoExpressionStatement] -> WhileInfo
    -> State PropagatorState [NoExpressionStatement]
statementWhile evaluateCondition body info = do
    let evalDependencies =
            G.fromListWith S.union $ getDependency evaluateCondition
    let bodyDependencies =
            G.fromListWith S.union $ getDependency body
    let dependencies =
            G.transitiveClosure (evalDependencies `G.union` bodyDependencies)

    let changedVars = G.headEdgesSet dependencies
    let changedFreeVars = changedVars `S.intersection` possibleFreeVars info
    let changedVarsDependOnChangedFreeVars =
            (\var -> not $ S.null
                ((dependencies G.! var) `S.intersection` changedFreeVars))
            `S.filter` changedVars

    let changedVarsDependOnInput = getDependencyOnInput body

    -- Changing the free variables creates a side effect when their values
    -- depends on other changed free variables.
    -- These variables' values are unknown.

    beforeWhileMapping <- gets constantMapping

    let insideWhileMapping =
            foldl (\m var -> M.insert var Nothing m)
                beforeWhileMapping $ S.union
                    changedVarsDependOnChangedFreeVars
                    changedVarsDependOnInput

    modify $ \s -> s { constantMapping = insideWhileMapping }

    (concat -> body') <- traverse statement body

    -- A while loop may or may not execute.
    -- So the free variables may be overwritten in the loop, or stays the same.
    -- So their values after the loop is unknown.

    let afterWhileMapping =
            foldl (\m var -> M.insert var Nothing m)
                beforeWhileMapping $ S.union
                    changedVars
                    changedVarsDependOnInput

    modify $ \s -> s { constantMapping = afterWhileMapping }

    return body'

{- |
This helper function applies constant propagation to a statement.
-}
statement :: NoExpressionStatement
    -> State PropagatorState [NoExpressionStatement]
statement = \case
    NE s -> do
        s' <- singleStatement s
        return [NE s']

    If condition thenClause elseClause -> do
        condition' <- scalar condition
        case condition' of
            Just 1 -> concat <$> traverse statement thenClause
            Just 0 -> concat <$> traverse statement elseClause
            _ -> statementIf condition thenClause elseClause

    While (condition, evaluateCondition) body info -> do
        traverse_ singleStatement evaluateCondition
        condition' <- scalar condition
        case condition' of
            Just 0 -> return []
            _ -> do
                body' <- statementWhile evaluateCondition body info
                return [While (condition, evaluateCondition) body' info]

{- |
This function applies constant propagation to a function's body.
-}
function :: Function NoExpressionStatement -> Function NoExpressionStatement
function (Function name params statements) =
    Function name params . concat $
        evalState (traverse statement statements) $
            PropagatorState (M.fromList [(var, Nothing) | (var, _) <- params])

{- |
This is the main function that applies constant propagation 
to an entire program.
-}
propagateConstant ::
    Program NoExpressionStatement -> Program NoExpressionStatement
propagateConstant (Program dataSegment functions) =
    Program dataSegment (function <$> functions)
