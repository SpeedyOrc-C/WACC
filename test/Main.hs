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

testSyntaxError = do
    putStrLn "# Syntax Error"

    oldPwd <- getCurrentDirectory
    setCurrentDirectory "example/invalid/syntaxErr"

    tests <- allFilesRecursive "."
    for_ tests $ \test -> do
        putStrLn $ "    " ++ test

        raw <- readFile test
        let result = parseString program raw
        putStrLn $ "        " ++ show result

    setCurrentDirectory oldPwd

main :: IO ()
main = do
    testSyntaxError
    exitFailure
