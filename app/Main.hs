module Main where

import Prelude hiding (error)

import System.Environment ( getArgs )
import System.Exit ( ExitCode(ExitFailure), exitSuccess, exitWith )

import Data.Foldable ( for_, traverse_ )

import Text.SourceCode ( textPosition, underlineTextSection, removeTabs )
import Text.AnsiEscape ( bold, red )
import qualified Text.Parser            as Parser
import qualified WACC.Syntax.Parser     as Syntax.Parser
import qualified WACC.Syntax.Structure  as Syntax.Structure
import qualified WACC.Semantics.Checker as Semantics.Checker
import qualified WACC.Semantics.Utils   as Semantics.Utils

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
processSourceCode (removeTabs -> sourceCode) =
    case Parser.parseString Syntax.Parser.program sourceCode of

    Left Nothing -> do
        putStrLn "Unknown syntax error."
        syntaxErrorExit

    Left (Just (Parser.SyntaxError pos error')) -> do

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

    Right (Parser.Parsed _ ast _) -> semanticCheck ast sourceCode

semanticCheck :: Syntax.Structure.Program -> String -> IO ()
semanticCheck program sourceCode =
    case Semantics.Checker.checkProgram program of

    Semantics.Utils.Ok {} -> do
        exitSuccess

    Semantics.Utils.Log semanticErrors -> do

        putStrLn ""

        for_ semanticErrors $ \(Semantics.Utils.SemanticError range error) -> do
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


