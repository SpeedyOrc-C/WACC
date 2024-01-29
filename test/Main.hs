import Data.Foldable
import Data.Traversable

import System.Exit
import System.Directory

import Text.Parser ( parseString, SyntaxError(SyntaxError), textPosition )
import Text.Parser.WACC
import Control.Monad

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

red :: String -> String
red str = "\x1b[31m" ++ str ++ "\x1b[0m"

orange :: String -> String
orange str = "\x1b[33m" ++ str ++ "\x1b[0m"

green :: String -> String
green str = "\x1b[32m" ++ str ++ "\x1b[0m"

humanTextPosition :: (Int, Int) -> (Int, Int)
humanTextPosition (row, col) = (row + 1, col + 1)

testSyntaxError = do
    putStrLn "# Syntax Error"

    oldPwd <- getCurrentDirectory
    setCurrentDirectory "example/invalid/syntaxErr"

    tests <- allFilesRecursive "."
    for_ tests $ \test -> do
        putStrLn $ "    " ++ test

        raw <- readFile test

        putStrLn $ "        " ++ 
            case parseString program raw of
                Right {} ->
                    red "[ERROR] Should have failed"
                Left Nothing ->
                    orange "[WELL] No error message"
                Left (Just (SyntaxError pos msg)) ->
                    green $ "[OK] " ++
                        show (humanTextPosition $ textPosition raw pos) ++
                        " : " ++ show msg

    setCurrentDirectory oldPwd

testParseValid = do
    putStrLn "# Valid"

    oldPwd <- getCurrentDirectory
    setCurrentDirectory "example/valid"

    tests <- allFilesRecursive "."
    for_ tests $ \test -> do
        putStrLn $ "    " ++ test

        raw <- readFile test
        case parseString program raw of
            Right {} ->
                putStrLn $ "        " ++ green "[OK]"
            Left (Just (SyntaxError pos msg)) -> do
                putStrLn $ "        " ++ red "[ERROR] " ++
                    show (humanTextPosition $ textPosition raw pos) ++
                    " : " ++ show msg
                print raw
            Left Nothing ->
                putStrLn $ "        " ++ red "[ERROR]"

    setCurrentDirectory oldPwd

main :: IO ()
main = do
    testSyntaxError
    testParseValid
    exitFailure
