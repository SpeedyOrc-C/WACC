module Main where

import Prelude hiding (error)
import qualified Prelude as P

import System.Environment ( getArgs )
import System.FilePath ( takeBaseName )
import System.Exit ( ExitCode(ExitFailure), exitSuccess, exitWith )

import Control.Monad ( when )
import Data.Foldable ( for_, traverse_ )
import Data.Function ((&))
import qualified Data.Sequence as Sq

import Text.SourceCode ( textPosition, underlineTextSection, removeTabs )
import Text.AnsiEscape ( bold, red )
import qualified Text.Parser              as Parser
import qualified WACC.Syntax.Parser       as Syntax.Parser
import qualified WACC.Syntax.Structure    as Syntax.Structure
import qualified WACC.Semantics.Checker   as Semantics.Checker
import qualified WACC.Semantics.Structure as Semantics.Structure
import qualified WACC.Semantics.Utils     as Semantics.Utils
import qualified WACC.Semantics.Error     as Semantics.Error
import qualified WACC.IR.Generate as IR
import qualified WACC.Backend.X64.Generate as X64
import qualified WACC.Backend.X64.ATnT as ATnT

import qualified WACC.Backend.X64.Unix.Config as Unix
import qualified WACC.Backend.X64.Windows.Config as Windows

syntaxErrorExit :: IO ()
syntaxErrorExit = exitWith $ ExitFailure 100

semanticErrorExit :: IO ()
semanticErrorExit = exitWith $ ExitFailure 200

splitFlags :: [String] -> ([String], [String])
splitFlags args = (filter ((== "--") . take 2) args, filter ((/= "--") . take 2) args)

data Target = Unix | Windows deriving Show

data Flags = Flags {
    noTextDecoration :: Bool,
    showIR :: Bool,
    target :: Target
} deriving Show

flagsFromArgs :: [String] -> Flags
flagsFromArgs args = Flags {
    noTextDecoration = "--no-text-deco" `elem` args,
    showIR = "--show-ir" `elem` args,
    target = case ("--target-unix" `elem` args, "--target-windows" `elem` args) of
        (True, False) -> Unix
        (False, True) -> Windows
        (False, False) -> P.error "Target is not specified."
        (True, True) -> P.error "So confusing. Please only choose one target."
}

preventTextDecoration :: Bool -> (a -> a) -> a -> a
preventTextDecoration False color = color
preventTextDecoration True _ = id

main :: IO ()
main = do
    (splitFlags -> (flagsArgs, args)) <- getArgs

    case args of
        [] -> putStrLn "Please provide a .wacc file to compile."

        _:_:_ -> putStrLn "Too many arguments provided (1 accepted)."

        path:_ -> do
            let targetUnix = "--target-unix" `elem` flagsArgs
            let targetWindows = "--target-windows" `elem` flagsArgs

            let flags = flagsFromArgs flagsArgs

            case (targetUnix, targetWindows) of
                (True, False) -> do
                    sourceCode <- readFile path
                    processSourceCode path flags {target=Unix} sourceCode
                (False, True) -> do
                    sourceCode <- readFile path
                    processSourceCode path flags {target=Windows} sourceCode
                (True, True) ->
                    putStrLn "So confusing. Please specify only one target."
                (False, False) ->
                    putStrLn "Please specify a target."

processSourceCode :: FilePath -> Flags -> String -> IO ()
processSourceCode path flags (removeTabs -> sourceCode) =
    case Parser.parseString Syntax.Parser.program sourceCode of

    Left Nothing -> do
        putStrLn "Unknown syntax error."
        syntaxErrorExit

    Left (Just (Parser.SyntaxError pos error')) -> do
        let (row, col) = textPosition sourceCode pos

        putStrLn ""

        putStrLn $
            preventTextDecoration (noTextDecoration flags) red
                "[Syntax Error] " ++
            preventTextDecoration (noTextDecoration flags) bold
                (show (row + 1) ++ ":" ++ show (col + 1))

        print error'

        (putStrLn . ("|   " ++ )) `traverse_`
            underlineTextSection pos (pos+1)
                (2, '^', preventTextDecoration (noTextDecoration flags) red)
                sourceCode

        putStrLn ""

        syntaxErrorExit

    Right (Parser.Parsed _ ast _) -> semanticCheck path flags ast sourceCode

semanticCheck :: FilePath -> Flags -> Syntax.Structure.Program -> String -> IO ()
semanticCheck path flags program sourceCode =
    case Semantics.Checker.checkProgram program of

    Semantics.Utils.Ok ast -> do
        generateCode path flags ast

    Semantics.Utils.Log semanticErrors -> do

        putStrLn ""

        for_ semanticErrors $ \(Semantics.Utils.SemanticError range error) -> do
            let (from, to) = range
            let (fromRow, fromCol) = textPosition sourceCode from

            putStrLn $
                preventTextDecoration (noTextDecoration flags)
                    red "[Semantic Error] " ++
                preventTextDecoration (noTextDecoration flags) bold
                    (show (fromRow + 1) ++ ":" ++ show (fromCol + 1)) ++ " "

            error & \case
                Semantics.Error.RedefinedIdentifier
                    name (textPosition sourceCode -> (pos, _))
                    -> putStrLn $
                        "Variable \"" ++ name ++ "\" is declared again.\n" ++
                        "Previously defined at line " ++ show (pos + 1) ++ "."
                it -> print it

            (putStrLn . ("|   " ++)) `traverse_`
                underlineTextSection from to
                    (2, '^', preventTextDecoration (noTextDecoration flags) red)
                    sourceCode

            putStrLn ""

        semanticErrorExit

generateCode :: FilePath -> Flags -> Semantics.Structure.Program -> IO ()
generateCode path flags ast = do
    let name = takeBaseName path

    let ir = IR.generateIR ast
    when (showIR flags) $ IR.debug ir

    let asm = (`X64.generateX64` ir) $ case target flags of
            Unix -> Unix.config
            Windows -> Windows.config

    let output = asm >>= \i -> ATnT.atnt i Sq.|> "\n"
    let fileName = name ++ ".S"

    writeFile fileName ""
    for_ output $ appendFile fileName

    exitSuccess
