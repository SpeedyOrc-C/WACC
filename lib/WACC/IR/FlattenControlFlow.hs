module WACC.IR.FlattenControlFlow where

import qualified Data.Set as S
import           Data.Foldable
import           Control.Arrow

import qualified WACC.Syntax.Parser as Parser
import           Text.Parser
import           WACC.IR.Structure
import           WACC.Semantics.Utils
import           WACC.Semantics.Checker
import           WACC.IR.FlattenExpression (flattenExpression, reference)

db = do
    raw <- readFile "draft.wacc"
    let Right (Parsed _ ast _) = parseString Parser.program raw
    let Ok ast' = checkProgram ast
    let Program _ functions = flattenControlFlow $ flattenExpression ast'
    for_ functions $ \(Function name params statements) -> do
        putStrLn $ "<" ++ name ++ ">" ++ " " ++ show params
        for_ statements $ \statement -> do
            print statement
        putStrLn ""

newtype FlattenerState = FlattenerState {
    labelCounter :: Int
}

initialState :: FlattenerState
initialState = FlattenerState { labelCounter = 1 }

nextLabel :: FlattenerState -> FlattenerState
nextLabel state = state { labelCounter = labelCounter state + 1}

class FlattenControlFlow a b where
    flatten :: FlattenerState -> a -> (FlattenerState, b)

instance FlattenControlFlow NoExpressionStatement [NoControlFlowStatement] where
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

instance FlattenControlFlow [NoExpressionStatement] [NoControlFlowStatement] where
    flatten state = \case
        [] -> (state, [])
        s:ss ->
            let
            (state', statements) = flatten state s
            (state'', statements') = flatten state' ss
            in
            (state'', statements ++ statements')

instance FlattenControlFlow (Function NoExpressionStatement) (Function NoControlFlowStatement) where
    flatten state (Function name params statements) =
        let (state', statements') = flatten state statements
        in (state', Function name params statements')

instance FlattenControlFlow [Function NoExpressionStatement] [Function NoControlFlowStatement] where
    flatten state = \case
        [] -> (state, [])
        f:fs ->
            let
            (state', f') = flatten state f
            (state'', fs') = flatten state' fs
            in
            (state'', f':fs')

instance FlattenControlFlow (Program NoExpressionStatement) (Program NoControlFlowStatement) where
    flatten state (Program dataSegment functions) =
        let (state', functions') = flatten state functions
        in (state', Program dataSegment functions')

data FreeingVariableState = FreeingVariableState {
    freed :: S.Set Identifier
}

initialFreeingVariableState :: FreeingVariableState
initialFreeingVariableState = FreeingVariableState {
    freed = S.empty
}

free :: [NoControlFlowStatement] -> [NoControlFlowStatement]
free = reverse . snd . free' initialFreeingVariableState . reverse

free' :: FreeingVariableState -> [NoControlFlowStatement] -> (FreeingVariableState, [NoControlFlowStatement])

free' state = \case
    [] -> (state, [])

    statement@(NCF s) : ss ->
        let
        toBeFreed = reference s S.\\ freed state
        state' = state {freed = freed state `S.union` toBeFreed}
        (state'', statements) = free' state' ss
        in
        ( state''
        , [FreeVariable var | var <- S.toList toBeFreed] ++
          statement : statements
        )

    WhileReference refs : ss ->
        let
        toBeFreed = refs S.\\ freed state
        state' = state {freed = freed state `S.union` toBeFreed}
        -- Stop freeing in the while loop.
        (state'', statements) = free' state' ss
        in
        ( state''
        , [FreeVariable var | var <- S.toList toBeFreed] ++
          statements
        )

    statement@(GotoIfNot (Variable scalar) _) : ss ->
        let
        toBeFreed = S.singleton scalar S.\\ freed state
        state' = state {freed = freed state `S.union` toBeFreed}
        (state'', statements) = free' state' ss
        in
        ( state''
        , [FreeVariable var | var <- S.toList toBeFreed] ++
          statement : statements
        )

    s@(Label {}) : ss -> second (s :) (free' state ss)
    s@(Goto {}) : ss -> second (s :) (free' state ss)

    _:ss -> free' state ss

flattenControlFlow :: Program NoExpressionStatement -> Program NoControlFlowStatement
flattenControlFlow program =
    Program
        dataSegment
        ([Function name params (free body)
            | Function name params body <- functions])
    where
    (_, Program dataSegment functions) = flatten initialState program
