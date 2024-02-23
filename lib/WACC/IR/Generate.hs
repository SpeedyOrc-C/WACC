module WACC.IR.Generate where

import Control.Arrow

import qualified WACC.Semantics.Structure as Semantics
import qualified WACC.IR.Structure as IR

import           WACC.IR.LiteralString (createDataSegments)
import           WACC.IR.FlattenExpression (flattenExpression)
import           WACC.IR.FlattenControlFlow (flattenControlFlow)
import           WACC.IR.LiveRange (analyseLiveRange)

generateIR :: Semantics.Program -> IR.Program IR.NoControlFlowStatement
generateIR =
        createDataSegments &&& id
    >>> flattenExpression
    >>> flattenControlFlow
    >>> analyseLiveRange