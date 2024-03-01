module WACC.IR.LiveRange where

import qualified Data.Set as S
import           Control.Monad.Trans.State.Lazy
import           Control.Arrow

import WACC.IR.Structure
import WACC.IR.FlattenExpression (reference)

{- Here is the set of all freed identifiers. -}
newtype FreeingVariableState = FreeingVariableState
    { freed :: S.Set Identifier }

initialFreeingVariableState :: FreeingVariableState
initialFreeingVariableState = FreeingVariableState
    { freed = S.empty }

{- Here we define different kinds of noControlFlowStatement. -}
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

{- Define a function to analyze a list of control flow statements
   by using the State monad to track the state of freeing variables. -}
statements :: [NoControlFlowStatement]
    -> State FreeingVariableState [NoControlFlowStatement]
statements xs = concat <$> traverse noControlFlowStatement xs

{- Define a function to apply the analysis to a list of statements
   and return the analyzed statements. -}
analyse :: [NoControlFlowStatement] -> [NoControlFlowStatement]
analyse xs = evalState (statements xs) initialFreeingVariableState

{- Define a function to manipulate control flow statements by adding
   directives related to free variables based on live range analysis. -}
putFreeVariableDirective :: [NoControlFlowStatement] 
                                -> [NoControlFlowStatement]
putFreeVariableDirective = reverse >>> analyse >>> reverse

{- Define a function to analyze live ranges in a program. -}
analyseLiveRange ::
    Program NoControlFlowStatement -> Program NoControlFlowStatement
analyseLiveRange (Program dataSegment functions) =
    Program dataSegment
        [Function name params (putFreeVariableDirective body)
            | Function name params body <- functions]
