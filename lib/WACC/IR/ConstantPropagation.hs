module WACC.IR.ConstantPropagation where

import qualified Data.Map as M
import           Control.Monad.Trans.State.Lazy

import WACC.IR.Structure
import WACC.Syntax.Error (intLowerBound, intUpperBound)

data PropagatorState = PropagatorState {
    constantMapping :: Identifier `M.Map` Maybe Constant
}

data Constant
    = CNumber Int
    | CString String
    | CArray [Constant]
    | CPair Constant Constant
    deriving (Show, Eq)

toScalar :: Constant -> Scalar
toScalar = \case
    CNumber x -> Immediate x
    e -> error $ show e

scalar :: Scalar -> State PropagatorState (Maybe Constant)
scalar = \case
    Immediate x -> return $ Just $ CNumber x

    Variable var -> do
        mapping <- gets constantMapping
        return $ mapping M.! var

    -- _ -> return Nothing

expression :: Expression -> State PropagatorState (Maybe Constant)
expression = \case
    Scalar s -> scalar s

    Not e -> do
        e' <- scalar e
        case e' of
            Just (CNumber x) -> return $ Just $ CNumber $ if x == 0 then 1 else 0
            _ -> return Nothing

    Length e -> do
        e' <- scalar e
        case e' of
            Just (CArray xs) ->
                return $ Just $ CNumber (length xs)
            _ ->
                return Nothing

    Order e -> scalar e

    Character c -> do
        c' <- scalar c
        case c' of
            Just (CNumber x) | 0 <= x && x <= 255 ->
                return $ Just $ CNumber (toEnum x)
            _ ->
                return Nothing

    Add a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (Just (CNumber x), Just (CNumber y))
                | intLowerBound <= x + y && x + y <= intUpperBound ->
                return $ Just $ CNumber $ x + y
            _ -> return Nothing

    Subtract a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (Just (CNumber x), Just (CNumber y)) -> do
                let result = x - y
                return $
                    if intLowerBound <= result && result <= intUpperBound
                    then Just $ CNumber result
                    else Nothing
            _ -> return Nothing

    Multiply a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (Just (CNumber x), Just (CNumber y)) -> do
                let result = x * y
                return $
                    if intLowerBound <= result && result <= intUpperBound
                    then Just $ CNumber result
                    else Nothing
            _ -> return Nothing

    Divide a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (_, Just (CNumber 0)) -> return Nothing
            (Just (CNumber x), Just (CNumber y)) -> do
                let result = x `div` y
                return $ Just $ CNumber result
            _ -> return Nothing

    Remainder a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (_, Just (CNumber 0)) -> return Nothing
            (Just (CNumber x), Just (CNumber y)) -> do
                let result = x `mod` y
                return $ Just $ CNumber result
            _ -> return Nothing

    Greater _ a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (Just (CNumber x), Just (CNumber y)) ->
                return $ Just $ CNumber $ if x > y then 1 else 0
            _ ->
                return Nothing

    GreaterEqual _ a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (Just (CNumber x), Just (CNumber y)) ->
                return $ Just $ CNumber $ if x >= y then 1 else 0
            _ ->
                return Nothing

    Less _ a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (Just (CNumber x), Just (CNumber y)) ->
                return $ Just $ CNumber $ if x < y then 1 else 0
            _ ->
                return Nothing

    LessEqual _ a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (Just (CNumber x), Just (CNumber y)) ->
                return $ Just $ CNumber $ if x <= y then 1 else 0
            _ ->
                return Nothing

    Equal _ a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (Just x, Just y) -> return $ Just $ CNumber $ if x == y then 1 else 0
            _ -> return Nothing

    NotEqual _ a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (Just x, Just y) -> return $ Just $ CNumber $ if x /= y then 1 else 0
            _ -> return Nothing

    -- _ -> return Nothing

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
                return $ Assign size var (Scalar $ toScalar c)

    PrintInt s -> do
        s' <- scalar s
        case s' of
            Just c -> return $ PrintInt $ toScalar c
            _ -> return $ PrintInt s

    PrintBool s -> do
        s' <- scalar s
        case s' of
            Just c -> return $ PrintBool $ toScalar c
            _ -> return $ PrintBool s

    PrintChar s -> do
        s' <- scalar s
        case s' of
            Just c -> return $ PrintChar $ toScalar c
            _ -> return $ PrintChar s

    PrintString s -> do
        s' <- scalar s
        case s' of
            Just c -> return $ PrintString $ toScalar c
            _ -> return $ PrintString s

    PrintAddress s -> do
        s' <- scalar s
        case s' of
            Just c -> return $ PrintAddress $ toScalar c
            _ -> return $ PrintAddress s

    Exit s -> do
        s' <- scalar s
        case s' of
            Just c -> return $ Exit $ toScalar c
            _ -> return $ Exit s

    s -> return s

statement :: NoExpressionStatement
    -> State PropagatorState [NoExpressionStatement]
statement = \case
    NE s -> do
        s' <- singleStatement s
        return [NE s']

    If condition thenClause elseClause -> do
        condition' <- scalar condition
        case condition' of
            Just (CNumber 1) -> return thenClause
            Just (CNumber 0) -> return elseClause
            -- _ -> do
            --     oldState <- get
            --     thenClause' <- traverse statement thenClause
            --     thenState <- get
            --     put oldState
            --     elseClause' <- traverse statement elseClause
            --     elseState <- get

            --     return $ If
            --         (case condition' of
            --             Just (CBool True) -> Immediate 1
            --             Just (CBool False) -> Immediate 0
            --             _ -> condition
            --         )
            --         thenClause'
            --         elseClause'

    s -> return [s]

function :: Function NoExpressionStatement
    -> State PropagatorState (Function NoExpressionStatement)
function (Function name params statements) =
    Function name params . concat <$> traverse statement statements

propagateConstant ::
    Program NoExpressionStatement -> Program NoExpressionStatement
propagateConstant (Program dataSegment functions) = Program dataSegment
    $ evalState (traverse function functions) (PropagatorState M.empty)
