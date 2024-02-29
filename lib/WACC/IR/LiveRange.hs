module WACC.IR.LiveRange where

import qualified Data.Set as S
import           Control.Arrow

import WACC.IR.Structure
import WACC.IR.FlattenExpression (reference)

{- Set of identifiers that have been freed. -}
newtype FreeingVariableState = FreeingVariableState {
    freed :: S.Set Identifier
}

{- Initially the freed set is empty. -}
initialFreeingVariableState :: FreeingVariableState
initialFreeingVariableState = FreeingVariableState {
    freed = S.empty
}

{- Function to insert free variable directives into a list of no-control-flow
   statements. -}
putFreeVariableDirective :: [NoControlFlowStatement] -> [NoControlFlowStatement]
putFreeVariableDirective =
        reverse
    >>> free initialFreeingVariableState
    >>> snd
    >>> reverse

{- Function to perform the freeing variable analysis on a list of
   no-control-flow statements. -}
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

    statement@(GotoIf variable _) : ss ->
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

    (WhileReference refs):ss ->
        let
        toBeFreed = refs S.\\ freed state
        state' = state {freed = freed state `S.union` toBeFreed}
        (state'', statements) = free state' ss
        in
        ( state'', WhileReference toBeFreed : statements )

    FreeVariable {}:ss -> free state ss

{- Function to perform live range analysis on a program. -}
analyseLiveRange ::
    Program NoControlFlowStatement -> Program NoControlFlowStatement
analyseLiveRange (Program dataSegment functions) =
    Program dataSegment
        [Function name params (putFreeVariableDirective body)
            | Function name params body <- functions]
