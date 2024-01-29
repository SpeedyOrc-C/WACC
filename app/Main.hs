module Main where

import System.Environment (getArgs)
import Text.Parser
    ( parseString, SyntaxError(SyntaxError), textPosition )
import Text.Parser.WACC (program)
import Data.Foldable (traverse_)
import Text.AnsiEscape ( gray, red, bold )
import System.Exit (exitWith, ExitCode (ExitFailure))

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "Please provide a .wacc file to compile."
        _:_:_ -> do
            putStrLn "Too many arguments provided (1 accepted)."
        path:_ -> do
            raw <- readFile path
            case parseString program raw of

                Left Nothing -> do
                    putStrLn "Unknown syntax error."
                    exitWith $ ExitFailure 100
                
                Left (Just (SyntaxError pos error')) -> do
                    let (row, col) = textPosition raw pos
                        ls = lines raw
                        distance = 2
                        upperLines = take distance $ drop (row - distance) ls
                        lowerLines = take distance $ drop (row + distance - 1) ls

                    putStrLn ""
                    (putStrLn . gray) `traverse_` upperLines
                    putStrLn $ ls !! row
                    putStrLn $ red (replicate col ' ' ++ "^")
                    (putStrLn . gray) `traverse_` lowerLines
                    putStrLn ""
                    putStrLn $ 
                        red "[Syntax Error] " ++
                        bold (show (row + 1) ++ ":" ++ show (col + 1)) ++ " " ++
                        show error'
                    putStrLn ""

                    exitWith $ ExitFailure 100

                Right {} -> do
                    putStrLn "Successfully parsed."

