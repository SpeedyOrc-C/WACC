module WACC.IR.Generate where

import Control.Arrow

import qualified WACC.Semantics.Structure as Semantics
import qualified WACC.IR.Structure as IR

import           WACC.IR.LiteralString (createDataSegments)
import           WACC.IR.FlattenExpression (flattenExpression)
import           WACC.IR.FlattenControlFlow (flattenControlFlow)
import           WACC.IR.LiveRange (analyseLiveRange)

-- db = do
--     raw <- readFile "draft.wacc"
--     let Right (Parsed _ ast _) = parseString Parser.program raw
--     let Ok ast' = checkProgram ast
--     let Program _ functions = flattenControlFlow $ flattenExpression ast'
--     for_ functions $ \(Function name params statements) -> do
--         putStrLn $ "<" ++ name ++ ">" ++ " " ++ show params
--         for_ statements $ \statement -> do
--             print statement
--         putStrLn ""

generateIR :: Semantics.Program -> IR.Program IR.NoControlFlowStatement
generateIR =
        createDataSegments &&& id
    >>> flattenExpression
    >>> flattenControlFlow
    >>> analyseLiveRange