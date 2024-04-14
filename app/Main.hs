module Main where

import System.Environment
import System.Exit
import System.FilePath
import System.Info

import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.List

import Text.SourceCode
import PrettyPrint

import Text.Parser                     qualified as Parser
import WACC.Syntax.Parser              qualified as Syntax.Parser
import WACC.Syntax.Structure           qualified as Syntax.Structure
import WACC.Semantics.Checker          qualified as Semantics.Checker
import WACC.Semantics.Structure        qualified as Semantics.Structure
import WACC.Semantics.Utils            qualified as Semantics.Utils
import WACC.IR.Generate                qualified as IR
import WACC.Backend.X64.Generate       qualified as X64
import WACC.Backend.X64.ATnT           qualified as ATnT
import WACC.Backend.X64.Unix.Config    qualified as Unix
import WACC.Backend.X64.Windows.Config qualified as Windows

main :: IO ()
main = do
    (splitFlags -> (flagsArgs, args)) <- getArgs

    case args of
        [] -> putStrLn "Please provide a .wacc file to compile."

        _:_:_ -> putStrLn "Too many arguments provided (1 accepted)."

        [path] -> do
            sourceCode <- readFile path

            let flags = flagsFromArgs flagsArgs
            let filBaseName = takeBaseName path
            let frontendResult =
                    checkSyntax flags sourceCode >>=
                    checkSemantics flags sourceCode

            case frontendResult of
                Left (exit, errors) -> do
                    traverse_ putStrLn errors
                    exit

                Right program -> generateCode filBaseName flags program

--------------------------------------------------------------------------------

type FileBaseName = String

data Target = Unix | Windows deriving Show

data Flags = Flags {
    noTextDecoration :: Bool,
    showIR :: Bool,
    targetWindows :: Bool,
    targetUnix :: Bool
} deriving Show

syntaxErrorExit :: IO ()
syntaxErrorExit = exitWith $ ExitFailure 100

semanticErrorExit :: IO ()
semanticErrorExit = exitWith $ ExitFailure 200

splitFlags :: [String] -> ([String], [String])
splitFlags = filter ((== "--") . take 2) &&& filter ((/= "--") . take 2)

flagsFromArgs :: [String] -> Flags
flagsFromArgs args = Flags {
    noTextDecoration = "--no-text-deco" `elem` args,
    showIR = "--show-ir" `elem` args,
    targetUnix = "--target-unix" `elem` args,
    targetWindows = "--target-windows" `elem` args
}

preventTextDecoration :: Bool -> (a -> a) -> a -> a
preventTextDecoration False color = color
preventTextDecoration True _ = id

checkSyntax :: Flags -> SourceCode -> Either (IO (), [String]) Syntax.Structure.Program
checkSyntax flags (removeTabs -> sourceCode) =
    case Parser.parseString Syntax.Parser.program sourceCode of
        Right (Parser.Parsed _ ast _) -> Right ast
        Left Nothing -> Left
            (syntaxErrorExit, return "Unknown syntax error.")
        Left (Just e) -> Left
            (syntaxErrorExit, prettyPrintError (noTextDecoration flags) sourceCode e)

checkSemantics :: Flags -> SourceCode -> Syntax.Structure.Program
    -> Either (IO (), [String]) Semantics.Structure.Program
checkSemantics flags sourceCode program =
    case Semantics.Checker.checkProgram program of
        Semantics.Utils.Ok ast -> Right ast
        Semantics.Utils.Log semanticErrors -> Left $ (semanticErrorExit, ) $
            intercalate [""] $
                prettyPrintError (noTextDecoration flags) sourceCode <$>
                    semanticErrors

generateCode :: FileBaseName -> Flags -> Semantics.Structure.Program -> IO ()
generateCode fileBaseName flags program = do
    let ir = IR.generateIR program
    when (showIR flags) $ IR.debug ir

    case eitherTarget of
        Left msg -> putStrLn msg
        Right target -> do
            let asm = X64.generateX64 target ir
            let atntLines = ATnT.atnt <$> asm

            let asmFileName = fileBaseName ++ ".S"
            writeFile asmFileName ""
            for_ atntLines $ \line -> do
                traverse_ (appendFile asmFileName) line
                appendFile asmFileName "\n"
    where
    eitherTarget = case (targetUnix flags, targetWindows flags) of
        (True, False) -> Right Unix.config
        (False, True) -> Right Windows.config
        (True, True) -> Left
            "So confusing. Please specify only one target."
        (False, False) -> case os of
            "mingw32" -> Right Windows.config
            "linux" -> Right Unix.config
            "darwin" -> Right Unix.config
            _ -> Left $
                "Please specify a target, " ++
                "this OS (" ++ os ++ ") is not supported."
