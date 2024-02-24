module WACC.IR.LiveRange where

import qualified Data.Set as S
import           Control.Arrow

import WACC.IR.Structure
import WACC.IR.FlattenExpression (reference)

newtype FreeingVariableState = FreeingVariableState {
    freed :: S.Set Identifier
}

initialFreeingVariableState :: FreeingVariableState
initialFreeingVariableState = FreeingVariableState {
    freed = S.empty
}

putFreeVariableDirective :: [NoControlFlowStatement] -> [NoControlFlowStatement]
putFreeVariableDirective =
        reverse
    >>> free initialFreeingVariableState
    >>> snd
    >>> reverse

free :: FreeingVariableState -> [NoControlFlowStatement]
        -> (FreeingVariableState, [NoControlFlowStatement])
free state = \case
    [] -> (state, [])

    statement@(NCF s) : ss ->
        let
        toBeFreed = reference s S.\\ freed state
        state' = state {freed = freed state `S.union` toBeFreed}
        (state'', statements) = free state' ss
        in
        ( state''
        , [FreeVariable var | var <- S.toList toBeFreed] ++
          statement : statements
        )

    WhileReference refs : ss ->
        let
        toBeFreed = refs S.\\ freed state
        state' = state {freed = freed state `S.union` toBeFreed}
        (state'', statements) = free state' ss
        in
        ( state''
        , [FreeVariable var | var <- S.toList toBeFreed] ++
          statements
        )

    statement@(GotoIfNot variable _) : ss ->
        let
        toBeFreed = reference variable S.\\ freed state
        state' = state {freed = freed state `S.union` toBeFreed}
        (state'', statements) = free state' ss
        in
        ( state''
        , [FreeVariable var | var <- S.toList toBeFreed] ++
          statement : statements
        )

    s@(Label {}) : ss -> second (s :) (free state ss)
    s@(Goto {}) : ss -> second (s :) (free state ss)
    s@(GotoIfNot {}) : ss -> second (s :) (free state ss)

    _:ss -> free state ss

analyseLiveRange ::
    Program NoControlFlowStatement -> Program NoControlFlowStatement
analyseLiveRange (Program dataSegment functions) =
    Program dataSegment
        [Function name params (putFreeVariableDirective body)
            | Function name params body <- functions]
