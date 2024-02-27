module WACC.IR.Generate where

import Control.Arrow

import qualified Data.Map as M
import           Data.Foldable

import qualified WACC.Semantics.Structure as Semantics
import qualified WACC.IR.Structure as IR

import           WACC.IR.LiteralString (createDataSegments)
import           WACC.IR.FlattenExpression (flattenExpression)
import           WACC.IR.FlattenControlFlow (flattenControlFlow)
import           WACC.IR.LiveRange (analyseLiveRange)

{- Function to generate intermediate representation (IR) from semantic analysis 
   results. -}
generateIR :: Semantics.Program -> IR.Program IR.NoControlFlowStatement
generateIR =
        createDataSegments &&& id
    >>> flattenExpression
    >>> flattenControlFlow
    >>> analyseLiveRange

{- Function for debugging purposes to print the generated IR. -}
debug :: IR.Program IR.NoControlFlowStatement -> IO ()
debug (IR.Program (M.toList -> dataSegment) functions) = do
    for_ dataSegment $ \(string, number) -> do
        putStrLn $ show number ++ "\t" ++ show string
    putStrLn ""

    for_ functions $ \(IR.Function name params statements) -> do
        putStrLn $ "<" ++ name ++ ">" ++ " " ++ show params
        for_ statements $ \statement -> do
            putStrLn $ "    " ++ show statement
        putStrLn ""