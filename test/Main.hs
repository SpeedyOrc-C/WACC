import Data.Traversable

import Prelude hiding (error)

import System.Exit ( exitFailure, exitSuccess )
import System.Directory
    ( doesDirectoryExist,
      doesFileExist,
      getCurrentDirectory,
      getDirectoryContents,
      setCurrentDirectory )
import Text.Parser ( parseString, SyntaxError(SyntaxError), Parsed (Parsed) )
import WACC.Syntax.Parser ( program )
import Control.Monad ( filterM )
import Text.AnsiEscape ( red, orange, green, gray )
import Text.SourceCode (textPosition)
import Test.WACC.Syntax (syntaxUnitTests)
import Test.WACC.Semantics (semanticsUnitTests)
import WACC.Semantics.Checker (checkProgram)
import WACC.Semantics.Utils ( LogEither(..), SemanticError(..) )
import Data.Foldable (for_)
import Data.List (sort)
import Test.WACC.Backend.IR (irUnitTests)

getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' path =
    filter (`notElem` [".", "..", ".DS_Store"]) <$> getDirectoryContents path

allFilesRecursive :: FilePath -> IO [FilePath]
allFilesRecursive path = do
    (sort -> contents) <- getDirectoryContents' path
    (sort -> files) <- filterM doesFileExist $ ((path ++ "/") ++) <$> contents
    (sort -> dirs) <- filterM doesDirectoryExist $ ((path ++ "/") ++ ) <$> contents
    (sort -> files') <- for dirs allFilesRecursive

    return $ files ++ concat files'

humanTextPosition :: (Int, Int) -> (Int, Int)
humanTextPosition (row, col) = (row + 1, col + 1)

testSyntaxError :: IO Bool
testSyntaxError = do
    putStrLn "# Syntax Error"

    oldPwd <- getCurrentDirectory
    setCurrentDirectory "example/invalid/syntaxErr"

    tests <- allFilesRecursive "."
    result <- for tests $ \test -> do

        raw <- readFile test
        case parseString program raw of

            Right {} -> do
                putStrLn $ red    "    ! " ++ test
                putStrLn $        "        " ++ red "Parser didn’t fail"
                return False

            Left Nothing -> do
                putStrLn $ orange "    ? " ++ test
                putStrLn $        "        " ++ orange "No error message"
                return False

            Left (Just (SyntaxError pos msg)) -> do
                putStrLn $ green  "    * " ++ test
                putStrLn $ "        " ++ gray (
                    show (humanTextPosition $ textPosition raw pos) ++
                    " : " ++ show msg)
                return True

    setCurrentDirectory oldPwd
    let testNum = length tests
        passedNum = length $ filter id result
    putStrLn $ "Syntax Error Test Result: " ++ show passedNum ++ "/" ++ show testNum ++ " passed"

    return $ and result

testSemanticError :: IO Bool
testSemanticError = do
    putStrLn "# Semantic Error"

    oldPwd <- getCurrentDirectory
    setCurrentDirectory "example/invalid/semanticErr"

    tests <- allFilesRecursive "."
    result <- for tests $ \test -> do

        raw <- readFile test
        case parseString program raw of

            Left {} -> do
                putStrLn $ red $ "    ! " ++ test
                putStrLn $ red   "        Syntax error found"

                return False

            Right (Parsed _ ast _) -> do
                case checkProgram ast of

                    Log errors -> do
                        putStrLn $ green "    * " ++ test

                        for_ errors $ \(SemanticError
                            (humanTextPosition . textPosition raw -> from
                            ,humanTextPosition . textPosition raw -> to
                            ) error ) -> do

                            putStrLn . gray $ "        " ++
                                show from ++ "-" ++ show to ++ " : " ++ show error

                        return True

                    Ok {} -> do
                        putStrLn $ red "    ! " ++ test
                        putStrLn $ red "        Semantic check didn't failed"

                        return False

    setCurrentDirectory oldPwd
    let testNum = length tests
        passedNum = length $ filter id result
    putStrLn $ "Semantic Error Test Result: " ++ show passedNum ++ "/" ++ show testNum ++ " passed"

    return $ and result

testValid :: IO Bool
testValid = do
    putStrLn "# Valid"

    oldPwd <- getCurrentDirectory
    setCurrentDirectory "example/valid"

    tests <- allFilesRecursive "."
    result <- for tests $ \test -> do

        raw <- readFile test
        case parseString program raw of

            Left {} -> do
                putStrLn $ red "    ! " ++ test
                putStrLn $ red "        Syntax error found"

                return False

            Right (Parsed _ ast _) -> do
                case checkProgram ast of

                    Log errors -> do
                        putStrLn $ red "    ! " ++ test

                        for_ errors $ \(SemanticError
                            (humanTextPosition . textPosition raw -> from
                            ,humanTextPosition . textPosition raw -> to
                            ) error ) -> do

                            putStrLn . gray $ "        " ++
                                show from ++ "-" ++ show to ++ " : " ++ show error

                        return False

                    Ok {} -> do
                        putStrLn $ green "    * " ++ test
                        return True

    setCurrentDirectory oldPwd
    let testNum = length tests
        passedNum = length $ filter id result
    putStrLn $ "Valid Test Result: " ++ show passedNum ++ "/" ++ show testNum ++ " passed"

    return $ and result

getTestResult :: String -> IO [Bool] -> IO Bool
getTestResult testName tests = do
    bools <- tests
    putStrLn $ testName ++ " Result: " ++ show (length (filter id bools)) ++ "/" ++ show (length bools) ++ " passed"
    return $ and bools

main :: IO ()
main = do
    succeed <- and <$> mapM (<* putStrLn "") [
            testSyntaxError,
            testSemanticError,
            testValid,
            getTestResult "Syntax Unit Test" syntaxUnitTests,
            getTestResult "Semantics Unit Test" semanticsUnitTests,
            getTestResult "IR Unit Test" irUnitTests
            ]

    if succeed then exitSuccess else exitFailure
