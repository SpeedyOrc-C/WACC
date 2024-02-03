module Main where

import System.Environment (getArgs)
import Text.SourceCode ( textPosition, underlineTextSection )
import Data.Foldable (traverse_, for_)
import Text.AnsiEscape ( red, bold )
import System.Exit (exitWith, ExitCode (ExitFailure), exitSuccess)
import qualified Text.Parser
import qualified WACC.Syntax.Parser
import qualified WACC.Syntax.Structure
import qualified WACC.Semantics.Checker
import qualified WACC.Semantics.Utils

syntaxErrorExit :: IO ()
syntaxErrorExit = exitWith $ ExitFailure 100

semanticErrorExit :: IO ()
semanticErrorExit = exitWith $ ExitFailure 200

main :: IO ()
main = do
    args <- getArgs
    case args of

        [] -> putStrLn "Please provide a .wacc file to compile."

        _:_:_ -> putStrLn "Too many arguments provided (1 accepted)."

        path:_ -> do
            sourceCode <- readFile path
            processSourceCode sourceCode

processSourceCode :: String -> IO ()
processSourceCode sourceCode =
    case Text.Parser.parseString WACC.Syntax.Parser.program sourceCode of

    Left Nothing -> do
        putStrLn "Unknown syntax error."
        syntaxErrorExit

    Left (Just (Text.Parser.SyntaxError pos error')) -> do

        putStrLn ""

        putStrLn `traverse_`
            underlineTextSection pos (pos+1) (2, '^', red) sourceCode

        putStrLn ""

        let (row, col) = textPosition sourceCode pos
        putStrLn $
            red "[Syntax] " ++
            bold (show (row + 1) ++ ":" ++ show (col + 1)) ++ " " ++
            show error'

        putStrLn ""

        syntaxErrorExit

    Right (Text.Parser.Parsed _ ast _) -> semanticCheck ast sourceCode

semanticCheck :: WACC.Syntax.Structure.Program -> String -> IO ()
semanticCheck program sourceCode =
    case WACC.Semantics.Checker.checkProgram program of

    WACC.Semantics.Utils.Ok {} -> do
        exitSuccess

    WACC.Semantics.Utils.Log semanticErrors -> do

        putStrLn ""

        for_ semanticErrors $ \(WACC.Semantics.Utils.SemanticError range error) -> do
            let (from, to) = range
            let (fromRow, fromCol) = textPosition sourceCode from

            putStrLn `traverse_`
                underlineTextSection from to (2, '^', red) sourceCode

            putStrLn ""

            putStrLn $
                red "[Semantics] " ++
                bold (show (fromRow + 1) ++ ":" ++ show (fromCol + 1)) ++ " " ++
                show error

            putStrLn ""

        semanticErrorExit


