module WACC.IR.ConstantPropagation where

import qualified Data.Map as M
import           Control.Monad.Trans.State.Lazy

import WACC.IR.Structure
import WACC.Syntax.Error (intLowerBound, intUpperBound)

newtype PropagatorState = PropagatorState {
    constantMapping :: Identifier `M.Map` Maybe Int
}

scalar :: Scalar -> State PropagatorState (Maybe Int)
scalar = \case
    Immediate x -> return $ Just x

    Variable var -> do
        mapping <- gets constantMapping
        return $ mapping M.! var

    _ -> return Nothing

expression :: Expression -> State PropagatorState (Maybe Int)
expression = \case
    Scalar s -> scalar s

    Not e -> do
        e' <- scalar e
        case e' of
            Just x -> return $ Just $ if x == 0 then 1 else 0
            _ -> return Nothing

    Order e -> scalar e

    Character c -> do
        c' <- scalar c
        case c' of
            Just x | 0 <= x && x <= 255 ->
                return $ Just (toEnum x)
            _ ->
                return Nothing

    Add a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (Just x, Just y)
                | intLowerBound <= x + y && x + y <= intUpperBound ->
                return $ Just $ x + y
            _ -> return Nothing

    Subtract a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (Just x, Just y) -> do
                let result = x - y
                return $
                    if intLowerBound <= result && result <= intUpperBound
                    then Just result
                    else Nothing
            _ -> return Nothing

    Multiply a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (Just x, Just y) -> do
                let result = x * y
                return $
                    if intLowerBound <= result && result <= intUpperBound
                    then Just result
                    else Nothing
            _ -> return Nothing

    Divide a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (Just x, Just y) | y /= 0 -> do return $ Just ( x `div` y)
            _ -> return Nothing

    Remainder a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (Just x, Just y) | y /= 0 -> return $ Just (x `mod` y)
            _ -> return Nothing

    Greater _ a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (Just x, Just y) -> return $ Just $ if x > y then 1 else 0
            _ -> return Nothing

    GreaterEqual _ a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (Just x, Just y) -> return $ Just $ if x >= y then 1 else 0
            _ -> return Nothing

    Less _ a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (Just x, Just y) -> return $ Just $ if x < y then 1 else 0
            _ -> return Nothing

    LessEqual _ a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (Just x, Just y) -> return $ Just $ if x <= y then 1 else 0
            _ -> return Nothing

    Equal _ a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (Just x, Just y) -> return $ Just $ if x == y then 1 else 0
            _ -> return Nothing

    NotEqual _ a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (Just x, Just y) -> return $ Just $ if x /= y then 1 else 0
            _ -> return Nothing

    ReadInt -> return Nothing

    e -> error $ show e

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

    PrintInt s -> do
        s' <- scalar s
        case s' of
            Just c -> return $ PrintInt $ Immediate c
            _ -> return $ PrintInt s

    PrintBool s -> do
        s' <- scalar s
        case s' of
            Just c -> return $ PrintBool $ Immediate c
            _ -> return $ PrintBool s

    PrintChar s -> do
        s' <- scalar s
        case s' of
            Just c -> return $ PrintChar $ Immediate c
            _ -> return $ PrintChar s

    PrintString s -> do
        s' <- scalar s
        case s' of
            Just c -> return $ PrintString $ Immediate c
            _ -> return $ PrintString s

    PrintAddress s -> do
        s' <- scalar s
        case s' of
            Just c -> return $ PrintAddress $ Immediate c
            _ -> return $ PrintAddress s

    Exit s -> do
        s' <- scalar s
        case s' of
            Just c -> return $ Exit $ Immediate c
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
            Just 1 -> return thenClause
            Just 0 -> return elseClause
            _ -> do
                beforeIfState <- get

                thenClause' <- traverse statement thenClause
                thenMapping <- gets constantMapping

                put beforeIfState

                elseClause' <- traverse statement elseClause
                elseMapping <- gets constantMapping

                let mergedMapping =
                        (M.unionWith $ \a b -> case (a, b) of
                            (Just x, Just y) | x == y -> Just x
                            _ -> Nothing
                        ) thenMapping elseMapping

                modify $ \s -> s { constantMapping = mergedMapping }

                return [If condition (concat thenClause') (concat elseClause')]

    s -> return [s]

function :: Function NoExpressionStatement
    -> State PropagatorState (Function NoExpressionStatement)
function (Function name params statements) =
    Function name params . concat <$> traverse statement statements

propagateConstant ::
    Program NoExpressionStatement -> Program NoExpressionStatement
propagateConstant (Program dataSegment functions) = Program dataSegment $
    evalState (traverse function functions) (PropagatorState M.empty)
