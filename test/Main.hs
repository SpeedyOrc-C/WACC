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

gray :: String -> String
gray str = "\x1b[90m" ++ str ++ "\x1b[0m"

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

testNoSyntaxError :: IO Bool
testNoSyntaxError = do
    putStrLn "# Valid"

    oldPwd <- getCurrentDirectory
    setCurrentDirectory "example"

    validTests <- allFilesRecursive "./valid"
    syntaticallyValidTests <- allFilesRecursive "./invalid/semanticErr"

    result <- for (validTests ++ syntaticallyValidTests) $ \test -> do
        raw <- readFile test
        case parseString program raw of
            Right {} -> do
                putStrLn $ green "    * " ++ test
                return True
            Left Nothing -> do
                putStrLn $ red   "    ! " ++ test
                return False
            Left (Just (SyntaxError pos msg)) -> do
                putStrLn $ red $ "    ! " ++ test
                putStrLn $ red $ "        " ++
                    show (humanTextPosition $ textPosition raw pos) ++
                    " : " ++ show msg
                print raw
                return False

    setCurrentDirectory oldPwd

    return $ and result

main :: IO ()
main = do
    resultSyntaxError <- testSyntaxError
    resultNoSyntaxError <- testNoSyntaxError

    let succeed = resultSyntaxError && resultNoSyntaxError
    
    if succeed then exitSuccess else exitFailure
