module WACC.IR.FlattenControlFlow where

import Control.Monad.Trans.State.Lazy
import Data.Functor.Identity

import WACC.IR.Structure
import Debug.Trace (traceShowId)

{- Define the state used by the flattener, containing the label counter. -}
newtype FlattenerState = FlattenerState { labelCounter :: Int }

{- Define the initial state for the flattener. Initial labelCounter is 1. -}
initialState :: FlattenerState
initialState = FlattenerState { labelCounter = 1 }

{- Function to update the state to the next label. -}
nextLabel :: FlattenerState -> FlattenerState
nextLabel s = s { labelCounter = labelCounter s + 1}

{- Generate a new label. -}
newLabel :: State FlattenerState String
newLabel = state $ \s ->
    (show (labelCounter s), s {labelCounter = labelCounter s + 1})

{- Convert a no-expression statement to a list of
   no-control-flow statements. -}
noExpressionStatement :: NoExpressionStatement -> State FlattenerState
                        [NoControlFlowStatement]
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

        return  $
            [Goto doneLabel] ++
            [Label whileLabel] ++
            body' ++
            [Label doneLabel] ++
            map NCF evaluateCondition ++
            [GotoIf condition whileLabel] ++
            [WhileReference refs]

{- Convert a list of no-expression statements to a list of no-control-flow
   statements. -}
noExpressionStatements :: [NoExpressionStatement] -> State FlattenerState
                        [NoControlFlowStatement]
noExpressionStatements xs = concat <$> traverse noExpressionStatement xs

{- Convert a function of no-expression statements to a function of
   no-control-flow statements. -}
functionNoExpressionStatement :: Function NoExpressionStatement -> State
                                FlattenerState (Function NoControlFlowStatement)
functionNoExpressionStatement (Function name params statements) =
    Function name params <$> noExpressionStatements statements

functionsNoExpressionStatement :: [Function NoExpressionStatement] -> State
                                FlattenerState [Function NoControlFlowStatement]
functionsNoExpressionStatement = traverse functionNoExpressionStatement

{- Convert a program of no-expression statements to a program of
   no-control-flow statements. -}
programNoExpressionStatement :: Program NoExpressionStatement -> State
                                FlattenerState (Program NoControlFlowStatement)
programNoExpressionStatement (Program dataSegment functions) =
    Program dataSegment <$> functionsNoExpressionStatement functions

{- Flatten control flow in an IR program. -}
flattenControlFlow ::
    Program NoExpressionStatement -> Program NoControlFlowStatement
flattenControlFlow program = runIdentity $ evalStateT
    (programNoExpressionStatement program) initialState
