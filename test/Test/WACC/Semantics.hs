module Test.WACC.Semantics where

import Text.Parser

import qualified Data.Map as M

import WACC.Syntax.Parser
import WACC.Syntax.Structure
import WACC.Semantics.Error
import WACC.Semantics.Utils
import WACC.Semantics.Structure
import WACC.Semantics.Checker
import Test.WACC.Syntax (testArrayElement1, testPrint, testScope2)
import WACC.Syntax.Parser (statement, statements)

testStatements testName input parser =
    case parseString parser input of
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

testStatementsInvalid testName input parser =
    case parseString parser input of
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
    statements

testDeclare2 = testStatementsInvalid "declare 2"
    "int a = 1; int a = 2"
    statements

testArray1 = testStatements "array1"
    "int[] a = [1, 2]"
    statements

testArray2 = testStatementsInvalid "array2"
    "int[] a = [1, 2]; int b = a[1 - \"horse\"]"
    statements

testExit1 = testStatements "exit1"
    "exit 1"
    statements

testExit2 = testStatementsInvalid "exit2"
    "exit \'a\'"
    statements

testExpressions1 = testStatements "expression1"
    "bool a = false; bool b = false; println a && b"
    statements

testExpressions2 = testStatementsInvalid "expression2"
    "int b = 15 + 6 || 19"
    statements

testFunction1 = testStatements "function1"
    "int foo(int x) is return x end"
    function

testFunction2 = testStatementsInvalid "function2"
    "int foo(int x, int x) is return x end"
    function

testIf1 = testStatements "if1"
    "if true then skip else skip fi"
    statements

testIf2 = testStatementsInvalid "if2"
    "if 15 + 6 then skip else skip fi"
    statements

testIO1 = testStatements "IO1"
    "int b = 0; read b"
    statements

testIO2 = testStatementsInvalid "IO2"
    "bool b = true; read b"
    statements

testPair1 = testStatements "pair1"
    "pair(int, char) p = newpair(10, 'a')"
    statements

testPair2 = testStatementsInvalid "pair2"
    "int x = newpair(10, 20)"
    statements

testPrint1 = testStatements "print1"
    "int x = 4; print x"
    statements

testPrint2 = testStatementsInvalid "print2"
    "int x = 4; char y = 'a'; print x + y"
    statements

testRead1 = testStatements "read1"
    "int x = 1; read x"
    statements

testRead2 = testStatementsInvalid "read2"
    "pair(bool, bool) p = null; read fst p"
    statements

testScopeSemantic1 = testStatements "scope1"
    "int a = 0; print a"
    statements

testScopeSemantic2 = testStatementsInvalid "scope2"
    "int x = 12; begin bool x = true; x = 5 end; exit x"
    statements

testVariable1 = testStatements "variable1"
    "int a = 1"
    statements

testVariable2 = testStatementsInvalid "variable2"
    "bool a = 1"
    statements

testWhileSemantic1 = testStatements "while1"
    "while true do skip done"
    statements

testWhileSemantic2 = testStatementsInvalid "while2"
    "while tru do skip done"
    statements

semanticsUnitTests :: IO [Bool]
semanticsUnitTests = sequence [
    testDeclare1,
    testDeclare2,
    testArray1,
    testArray2,
    testExit1,
    testExit2,
    testExpressions1,
    testExpressions2,
    testFunction1,
    testFunction2,
    testIf1,
    testIf2,
    testIO1,
    testIO2,
    testPair1,
    testPair2,
    testPrint1,
    testPrint2,
    testRead1,
    testRead2,
    testScopeSemantic1,
    testScopeSemantic2,
    testVariable1,
    testVariable2,
    testWhileSemantic1,
    testWhileSemantic2
    ]
