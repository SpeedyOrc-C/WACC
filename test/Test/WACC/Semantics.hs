module Test.WACC.Semantics where

import Text.Parser

import qualified Data.Map as M

import WACC.Syntax.Parser
import WACC.Syntax.Structure
import WACC.Semantics.Error
import WACC.Semantics.Utils
import WACC.Semantics.Structure
import WACC.Semantics.Checker

testStatements testName input =
    case parseString statements input of
        Right (Parsed _ result (_, [])) ->
            case check (CheckerState Nothing M.empty [M.empty]) result of
                Ok {} -> do
                    putStrLn $ "[PASS] " ++ testName
                    return True
                _ -> do
                    putStrLn $ "[FAIL] " ++ testName
                    return False
        _ -> do
            putStrLn $ "[NO PARSE] " ++ testName
            return False

testStatementsInvalid testName input =
    case parseString statements input of
        Right (Parsed _ result (_, [])) ->
            case check (CheckerState Nothing M.empty [M.empty]) result of
                Log {} -> do
                    putStrLn $ "[PASS] " ++ testName
                    return True
                _ -> do
                    putStrLn $ "[FAIL] " ++ testName
                    return False
        _ -> do
            putStrLn $ "[NO PARSE] " ++ testName
            return False

testDeclare1 = testStatements "declare 1"
    "int a = 1; int b = 2"

testDeclare2 = testStatementsInvalid "declare 2"
    "int a = 1; int a = 2"

semanticsUnitTests :: IO [Bool]
semanticsUnitTests = sequence [
    testDeclare1,
    testDeclare2
    ]
