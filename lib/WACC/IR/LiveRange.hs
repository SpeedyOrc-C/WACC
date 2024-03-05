module WACC.IR.LiveRange where

import qualified Data.Set as S
import           Control.Monad.Trans.State.Lazy
import           Control.Arrow

import WACC.IR.Structure
import WACC.IR.FlattenExpression (reference)

newtype FreeingVariableState = FreeingVariableState
    { freed :: S.Set Identifier }

initialFreeingVariableState :: FreeingVariableState
initialFreeingVariableState = FreeingVariableState
    { freed = S.empty }

hasSideEffect :: SingleStatement -> Bool
hasSideEffect = \case
    Assign _ _ (Call {}) -> True
    _ -> False

noControlFlowStatement :: NoControlFlowStatement
    -> State FreeingVariableState [NoControlFlowStatement]
noControlFlowStatement = \case
    NCF statement -> do
        s <- get
        let toBeFreed = reference statement S.\\ freed s
        put s {freed = freed s `S.union` toBeFreed}
        return $ (FreeVariable <$> S.toList toBeFreed) ++ [NCF statement]

    GotoIfNot variable label -> do
        s <- get
        let toBeFreed = reference variable S.\\ freed s
        put s {freed = freed s `S.union` toBeFreed}
        return $ (FreeVariable <$> S.toList toBeFreed)
                    ++ [GotoIfNot variable label]

    GotoIf variable label -> do
        s <- get
        let toBeFreed = reference variable S.\\ freed s
        put s {freed = freed s `S.union` toBeFreed}
        return $ (FreeVariable <$> S.toList toBeFreed)
                    ++ [GotoIf variable label]

    WhileReference refs -> do
        s <- get
        let toBeFreed = refs S.\\ freed s
        put s {freed = freed s `S.union` toBeFreed}
        return [WhileReference toBeFreed]

    Label label -> return [Label label]
    Goto label -> return [Goto label]
    FreeVariable {} -> return []

statements :: [NoControlFlowStatement]
    -> State FreeingVariableState [NoControlFlowStatement]
statements xs = concat <$> traverse noControlFlowStatement xs

analyse :: [NoControlFlowStatement] -> [NoControlFlowStatement]
analyse xs = evalState (statements xs) initialFreeingVariableState

putFreeVariableDirective :: [NoControlFlowStatement]
                                -> [NoControlFlowStatement]
putFreeVariableDirective = reverse >>> analyse >>> reverse

analyseLiveRange ::
    Program NoControlFlowStatement -> Program NoControlFlowStatement
analyseLiveRange (Program dataSegment functions) =
    Program dataSegment
        [Function name params (putFreeVariableDirective body)
            | Function name params body <- functions]
