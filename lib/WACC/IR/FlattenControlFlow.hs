module WACC.IR.FlattenControlFlow where

import Control.Monad.Trans.State.Lazy
import Data.Functor.Identity

import WACC.IR.Structure

newtype FlattenerState = FlattenerState { labelCounter :: Int }

initialState :: FlattenerState
initialState = FlattenerState { labelCounter = 1 }

nextLabel :: FlattenerState -> FlattenerState
nextLabel s = s { labelCounter = labelCounter s + 1}

newLabel :: State FlattenerState String
newLabel = state $ \s ->
    (show (labelCounter s), s {labelCounter = labelCounter s + 1})

noExpressionStatement :: NoExpressionStatement -> State FlattenerState [NoControlFlowStatement]
noExpressionStatement = \case
    NE statement ->
        return [NCF statement]

    If condition thenClause elseClause -> do
        thenClause' <- noExpressionStatements thenClause
        elseLabel <- ("else." ++) <$> newLabel
        elseClause' <- noExpressionStatements elseClause
        fiLabel <- ("fi." ++) <$> newLabel

        return $
            [GotoIfNot condition elseLabel] ++
            thenClause' ++
            [Goto fiLabel, Label elseLabel] ++
            elseClause' ++
            [Label fiLabel]

    While (condition, evaluateCondition) body refs -> do
        whileLabel <- ("while." ++) <$> newLabel
        body' <- noExpressionStatements body
        doneLabel <- ("done." ++) <$> newLabel

        return $
            [Label whileLabel] ++
            map NCF evaluateCondition ++
            [GotoIfNot condition doneLabel] ++
            body' ++
            [Goto whileLabel, Label doneLabel, WhileReference refs]

noExpressionStatements :: [NoExpressionStatement] -> State FlattenerState [NoControlFlowStatement]
noExpressionStatements = (concat <$>) <$> traverse noExpressionStatement

functionNoExpressionStatement :: Function NoExpressionStatement -> State FlattenerState (Function NoControlFlowStatement)
functionNoExpressionStatement (Function name params statements) =
    Function name params <$> noExpressionStatements statements

functionsNoExpressionStatement :: [Function NoExpressionStatement] -> State FlattenerState [Function NoControlFlowStatement]
functionsNoExpressionStatement = traverse functionNoExpressionStatement

programNoExpressionStatement :: Program NoExpressionStatement -> State FlattenerState (Program NoControlFlowStatement)
programNoExpressionStatement (Program dataSegment functions) =
    Program dataSegment <$> functionsNoExpressionStatement functions

flattenControlFlow ::
    Program NoExpressionStatement -> Program NoControlFlowStatement
flattenControlFlow program = runIdentity $ evalStateT
    (programNoExpressionStatement program) initialState
