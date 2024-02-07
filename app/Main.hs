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

splitFlags :: [String] -> ([String], [String])
splitFlags args = (filter ((== "--") . take 2) args, filter ((/= "--") . take 2) args)

data Flags = Flags {
    noTextDecoration :: Bool
} deriving Show

flagsFromArgs :: [String] -> Flags
flagsFromArgs args = Flags {
    noTextDecoration = "--no-text-deco" `elem` args
}

preventTextDecoration :: Bool -> (a -> a) -> a -> a
preventTextDecoration False color = color
preventTextDecoration True _ = id

main :: IO ()
main = do
    (splitFlags -> (flagsArgs, args)) <- getArgs

    let flags = flagsFromArgs flagsArgs

    case args of

        [] -> putStrLn "Please provide a .wacc file to compile."

        _:_:_ -> putStrLn "Too many arguments provided (1 accepted)."

        path:_ -> do
            sourceCode <- readFile path
            processSourceCode flags sourceCode

processSourceCode :: Flags -> String -> IO ()
processSourceCode flags (removeTabs -> sourceCode) =
    case Parser.parseString Syntax.Parser.program sourceCode of

    (_, Left Nothing) -> do
        putStrLn "Unknown syntax error."
        syntaxErrorExit

    (expectedHints, Left (Just (Parser.SyntaxError pos error'))) -> do

        putStrLn ""

        putStrLn `traverse_`
            underlineTextSection pos (pos+1)
                (2, '^', preventTextDecoration (noTextDecoration flags) red)
                sourceCode

        putStrLn (show pos)
        putStrLn ""
        let (row, col) = textPosition sourceCode pos
        putStrLn $
            preventTextDecoration (noTextDecoration flags) red
                "[Syntax] " ++
            preventTextDecoration (noTextDecoration flags) bold
                (show (row + 1) ++ ":" ++ show (col + 1)) ++ " " ++
            show error'
        Parser.printHints expectedHints pos

        putStrLn ""

        syntaxErrorExit

    (_, Right (Parser.Parsed _ ast _)) -> semanticCheck flags ast sourceCode

semanticCheck :: Flags -> Syntax.Structure.Program -> String -> IO ()
semanticCheck flags program sourceCode =
    case Semantics.Checker.checkProgram program of

    Semantics.Utils.Ok {} -> do
        exitSuccess

    Semantics.Utils.Log semanticErrors -> do

        putStrLn ""

        for_ semanticErrors $ \(Semantics.Utils.SemanticError range error) -> do
            let (from, to) = range
            let (fromRow, fromCol) = textPosition sourceCode from

            putStrLn `traverse_`
                underlineTextSection from to
                    (2, '^', preventTextDecoration (noTextDecoration flags) red)
                    sourceCode

            putStrLn ""

            putStrLn $
                preventTextDecoration (noTextDecoration flags)
                    red "[Semantics] " ++
                preventTextDecoration (noTextDecoration flags) bold
                    (show (fromRow + 1) ++ ":" ++ show (fromCol + 1)) ++ " " ++
                show error

            putStrLn ""

        semanticErrorExit
