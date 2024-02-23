module WACC.IR.FlattenControlFlow where

import WACC.IR.Structure

newtype FlattenerState = FlattenerState {
    labelCounter :: Int
}

initialState :: FlattenerState
initialState = FlattenerState { labelCounter = 1 }

nextLabel :: FlattenerState -> FlattenerState
nextLabel state = state { labelCounter = labelCounter state + 1}

class FlattenControlFlow a b where
    flatten :: FlattenerState -> a -> (FlattenerState, b)

instance FlattenControlFlow
    NoExpressionStatement [NoControlFlowStatement] where
    flatten ::
        FlattenerState -> NoExpressionStatement
        -> (FlattenerState, [NoControlFlowStatement])
    flatten state = \case
        NE s -> (state, [NCF s])

        If condition thenClause elseClause ->
            let
            (state', thenClause') = flatten state thenClause
            labelElse = "else" ++ show (labelCounter state')
            (state'', elseClause') = flatten (nextLabel state') elseClause
            labelEndIf = "fi" ++ show (labelCounter state)
            in
            ( nextLabel state''
            , [GotoIfNot condition labelElse] ++
              thenClause' ++
              [ Goto labelEndIf
              , Label labelElse] ++
              elseClause' ++
              [Label labelEndIf]
            )

        While condition body refs ->
            let
            labelWhile = "while" ++ show (labelCounter state)
            (state', body') = flatten (nextLabel state) body
            labelDone = "done" ++ show (labelCounter state')
            in
            ( nextLabel state'
            , [ Label labelWhile
              , GotoIfNot condition labelDone] ++
              body' ++
              [ Goto labelWhile
              , Label labelDone
              , WhileReference refs]
            )

instance FlattenControlFlow
    [NoExpressionStatement] [NoControlFlowStatement] where
    flatten ::
        FlattenerState -> [NoExpressionStatement]
        -> (FlattenerState, [NoControlFlowStatement])
    flatten state = \case
        [] -> (state, [])
        s:ss ->
            let
            (state', statements) = flatten state s
            (state'', statements') = flatten state' ss
            in
            (state'', statements ++ statements')

instance FlattenControlFlow
    (Function NoExpressionStatement) (Function NoControlFlowStatement) where
    flatten ::
        FlattenerState -> Function NoExpressionStatement
        -> (FlattenerState, Function NoControlFlowStatement)
    flatten state (Function name params statements) =
        let (state', statements') = flatten state statements
        in (state', Function name params statements')

instance FlattenControlFlow
    [Function NoExpressionStatement] [Function NoControlFlowStatement] where
    flatten ::
        FlattenerState -> [Function NoExpressionStatement]
        -> (FlattenerState, [Function NoControlFlowStatement])
    flatten state = \case
        [] -> (state, [])
        f:fs ->
            let
            (state', f') = flatten state f
            (state'', fs') = flatten state' fs
            in
            (state'', f':fs')

instance FlattenControlFlow
    (Program NoExpressionStatement) (Program NoControlFlowStatement) where
    flatten ::
        FlattenerState -> Program NoExpressionStatement
        -> (FlattenerState, Program NoControlFlowStatement)
    flatten state (Program dataSegment functions) =
        let (state', functions') = flatten state functions
        in (state', Program dataSegment functions')

flattenControlFlow ::
    Program NoExpressionStatement -> Program NoControlFlowStatement
flattenControlFlow = snd . flatten initialState
