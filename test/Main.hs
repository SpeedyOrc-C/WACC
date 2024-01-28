import Data.Foldable
import Data.Traversable

import System.Exit
import System.Directory

import Text.Parser
import Text.Parser.WACC

getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' path = filter (`notElem` [".", "..", ".DS_Store"]) <$>
    getDirectoryContents path

syntaxErrorTestPath :: FilePath
syntaxErrorTestPath = "example/invalid/syntaxErr"

syntaxErrorTestCategories :: IO [FilePath]
syntaxErrorTestCategories = do
    dir <- getCurrentDirectory
    getDirectoryContents' $ dir ++ "/" ++ syntaxErrorTestPath

syntaxErrorTestsPaths :: IO [(String, [(String, FilePath)])]
syntaxErrorTestsPaths = do
    categories <- syntaxErrorTestCategories
    for categories $ \category -> do
        tests <- do
            paths <- getDirectoryContents' (syntaxErrorTestPath ++ "/" ++ category)
            for paths $ \path -> do
                return (path, syntaxErrorTestPath ++ "/" ++ category ++ "test")
        return (category, tests)

testSyntaxError = do
    putStrLn "TEST [Syntax Error]"
    categories <- syntaxErrorTestsPaths
    for categories $ \(category, tests) -> do
        putStrLn $ "  * " ++ category
        for tests $ \(test, path) -> do
            putStrLn $ "    - " ++ test
            raw <- readFile path
            let result = parseString statements raw
            putStrLn $ "        " ++ show result

main :: IO ()
main = do
    testSyntaxError
    exitFailure
