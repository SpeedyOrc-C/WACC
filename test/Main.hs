import Data.Traversable

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
import Test.WACC.Syntax (syntaxTests)
import WACC.Semantics.Checker (checkProgram)
import WACC.Semantics.Utils (LogEither(..))
import Data.Foldable (for_)
import WACC.Semantics.Utils (SemanticError(..))

getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' path =
    filter (`notElem` [".", "..", ".DS_Store"]) <$> getDirectoryContents path

allFilesRecursive :: FilePath -> IO [FilePath]
allFilesRecursive path = do
    contents <- getDirectoryContents' path
    files <- filterM doesFileExist $ ((path ++ "/") ++) <$> contents
    dirs <- filterM doesDirectoryExist $ ((path ++ "/") ++ ) <$> contents
    files' <- for dirs allFilesRecursive

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

            Left (Just (SyntaxError pos msg)) -> do
                putStrLn $ red $ "    ! " ++ test
                putStrLn $ red   "        Syntax error found"
                print raw
                return False

            Left Nothing -> do
                putStrLn $ red   "    ! " ++ test
                return False

            Right (Parsed _ ast _) -> do
                putStrLn $ green "    * " ++ test

                case checkProgram ast of

                    Log errors -> do
                        for_ errors $ \(SemanticError
                            (humanTextPosition . textPosition raw -> from
                            ,humanTextPosition . textPosition raw -> to
                            ) error ) -> do

                            putStrLn . gray $ "        " ++
                                show from ++ "-" ++ show to ++ " : " ++ show error

                        return True

                    Ok {} -> do
                        return False

    setCurrentDirectory oldPwd

    return $ and result



main :: IO ()
main = do
    resultSyntaxError <- testSyntaxError
    resultSemanticError <- testSemanticError
    resultSyntax <- syntaxTests

    let succeed = resultSyntaxError && resultSemanticError && and resultSyntax
    if succeed then exitSuccess else exitFailure
