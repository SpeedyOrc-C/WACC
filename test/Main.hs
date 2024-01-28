{-# LANGUAGE LambdaCase #-}
import Data.Foldable
import Data.Traversable

import System.Exit
import System.Directory

import Text.Parser
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

testSyntaxError = do
    putStrLn "# Syntax Error"

    oldPwd <- getCurrentDirectory
    setCurrentDirectory "example/invalid/syntaxErr"

    tests <- allFilesRecursive "."
    for_ tests $ \test -> do
        putStrLn $ "    " ++ test

        raw <- readFile test
        let result = parseString program raw

        putStrLn $ "        " ++ 
            case parseString program raw of
                Right {} ->
                    red "[ERROR] Should have failed"
                Left Nothing ->
                    orange "[WELL] No error message"
                Left (Just (SyntaxError pos msg)) ->
                    green $ "[OK] " ++ show pos ++ " : " ++ show msg

    setCurrentDirectory oldPwd

main :: IO ()
main = do
    testSyntaxError
    exitFailure
