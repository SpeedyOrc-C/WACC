module WACC.IR.ConstantPropagation where

import qualified Data.Map as M
import           Control.Monad.Trans.State.Lazy

import WACC.IR.Structure
import WACC.Syntax.Error (intLowerBound, intUpperBound)

data PropagatorState = PropagatorState {
    constantMapping :: Identifier `M.Map` Maybe Constant
}

data Constant
    = CInt Int
    | CBool Bool
    | CChar Char
    | CString String
    | CArray [Constant]
    | CPair Constant Constant
    | CNull

toExpression :: Constant -> Expression
toExpression = \case
    CInt x -> Scalar $ Immediate x

toScalar :: Constant -> Scalar
toScalar = \case
    CInt x -> Immediate x

scalar :: Scalar -> State PropagatorState (Maybe Constant)
scalar = \case
    Immediate x -> return $ Just $ CInt x

    Variable var -> do
        mapping <- gets constantMapping
        return $ mapping M.! var

    _ -> return Nothing

expression :: Expression -> State PropagatorState (Maybe Constant)
expression = \case
    Scalar s -> scalar s

    Add a b -> do
        a' <- scalar a
        b' <- scalar b
        case (a', b') of
            (Just (CInt x), Just (CInt y)) -> do
                let result = x + y
                return $
                    if intLowerBound <= result && result <= intUpperBound
                    then Just $ CInt result
                    else Nothing
            _ -> return Nothing

    _ -> return Nothing

singleStatement :: SingleStatement
    -> State PropagatorState SingleStatement
singleStatement = \case
    s@(Assign size var e) -> do
        e' <- expression e
        case e' of
            Nothing -> return s
            Just c -> do
                modify $ \s -> s {
                    constantMapping = M.insert var (Just c) (constantMapping s)
                }
                return $ Assign size var $ toExpression c

    PrintInt s -> do
        s' <- scalar s
        case s' of
            Just c -> return $ PrintInt $ toScalar c
            _ -> return $ PrintInt s

    s -> return s

statement :: NoExpressionStatement
    -> State PropagatorState NoExpressionStatement
statement = \case
    NE s -> NE <$> singleStatement s
    s -> return s

function :: Function NoExpressionStatement
    -> State PropagatorState (Function NoExpressionStatement)
function (Function name params statements) =
    Function name params <$> traverse statement statements

propagateConstant ::
    Program NoExpressionStatement -> Program NoExpressionStatement
propagateConstant (Program dataSegment functions) = Program dataSegment
    $ evalState (traverse function functions) (PropagatorState M.empty)
