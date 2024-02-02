module Main where

import System.Environment (getArgs)
import Text.SourceCode ( textPosition, underlineTextSection )
import Text.Parser
    ( parseString, SyntaxError(SyntaxError), Parsed (Parsed) )
import WACC.Syntax.Parser (program)
import Data.Foldable (traverse_)
import Text.AnsiEscape ( red, bold )
import System.Exit (exitWith, ExitCode (ExitFailure), exitSuccess)
import qualified WACC.Syntax.Structure as Syntax
import WACC.Syntax.Structure

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
processSourceCode sourceCode = case parseString program sourceCode of
    Left Nothing -> do
        putStrLn "Unknown syntax error."
        syntaxErrorExit

    Left (Just (SyntaxError pos error')) -> do

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

    Right (Parsed _ ast _) -> semanticCheck ast

semanticCheck :: Syntax.Program -> IO ()
semanticCheck = \case
    Program ([], [Print (LiteralString "Hello World!" _) _]) _ -> do
        putStrLn "Hello Carrot!"
        exitSuccess
    _ -> do
        semanticErrorExit
    
