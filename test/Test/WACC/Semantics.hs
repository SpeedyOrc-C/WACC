module Test.WACC.Semantics where

import qualified Data.Map as M
import Text.Parser (parseString, Parsed(Parsed), Parser)
import WACC.Syntax.Parser (function, statements, program)
import WACC.Semantics.Utils (CheckerState(CheckerState), LogEither(Log, Ok))
import WACC.Semantics.Checker (CheckSemantics(..))
import Text.AnsiEscape

testValid :: CheckSemantics syntaxTree result =>
  [Char] -> String -> Parser error syntaxTree -> IO Bool
testValid testName input parser =
    case parseString parser input of
        Right (Parsed _ result (_, [])) ->
            case check (CheckerState Nothing M.empty [M.empty]) result of
                Ok {} -> do
                    putStrLn $ green "    * " ++ testName
                    return True
                _ -> do
                    putStrLn $ red "    ! " ++ testName
                    return False
        _ -> do
            putStrLn $ red "    ? " ++ testName
            return False

testInvalid :: CheckSemantics syntaxTree result =>
  [Char] -> String -> Parser error syntaxTree -> IO Bool
testInvalid testName input parser =
    case parseString parser input of
        Right (Parsed _ result (_, [])) ->
            case check (CheckerState Nothing M.empty [M.empty]) result of
                Log {} -> do
                    putStrLn $ green "    * " ++ testName
                    return True
                _ -> do
                    putStrLn $ red "    ! " ++ testName
                    return False
        _ -> do
            putStrLn $ red "? " ++ testName
            return False

testDeclare1 :: IO Bool
testDeclare1 = testValid "declare 1"
    "int a = 1; int b = 2"
    statements

testDeclare2 :: IO Bool
testDeclare2 = testInvalid "declare 2"
    "int a = 1; int a = 2"
    statements

testArray1 :: IO Bool
testArray1 = testValid "array1"
    "int[] a = [1, 2]"
    statements

testArray2 :: IO Bool
testArray2 = testInvalid "array2"
    "int[] a = [1, 2]; int b = a[1 - \"horse\"]"
    statements

testExit1 :: IO Bool
testExit1 = testValid "exit1"
    "exit 1"
    statements

testExit2 :: IO Bool
testExit2 = testInvalid "exit2"
    "exit \'a\'"
    statements

testExpressions1 :: IO Bool
testExpressions1 = testValid "expression1"
    "bool a = false; bool b = false; println a && b"
    statements

testExpressions2 :: IO Bool
testExpressions2 = testInvalid "expression2"
    "int b = 15 + 6 || 19"
    statements

testFunction1 :: IO Bool
testFunction1 = testValid "function1"
    "int foo(int x) is return x end"
    function

testFunction2 :: IO Bool
testFunction2 = testInvalid "function2"
    "int foo(int x, int x) is return x end"
    function

testIf1 :: IO Bool
testIf1 = testValid "if1"
    "if true then skip else skip fi"
    statements

testIf2 :: IO Bool
testIf2 = testInvalid "if2"
    "if 15 + 6 then skip else skip fi"
    statements

testIO1 :: IO Bool
testIO1 = testValid "IO1"
    "int b = 0; read b; print b"
    statements

testIO2 :: IO Bool
testIO2 = testInvalid "IO2"
    "bool b = true; read b"
    statements

testPair1 :: IO Bool
testPair1 = testValid "pair1"
    "pair(int, char) p = newpair(10, 'a')"
    statements

testPair2 :: IO Bool
testPair2 = testInvalid "pair2"
    "int x = newpair(10, 20)"
    statements

testPrint1 :: IO Bool
testPrint1 = testValid "print1"
    "int x = 4; print x"
    statements

testPrint2 :: IO Bool
testPrint2 = testInvalid "print2"
    "int x = 4; char y = 'a'; print x + y"
    statements

testRead1 :: IO Bool
testRead1 = testValid "read1"
    "int x = 1; read x"
    statements

testRead2 :: IO Bool
testRead2 = testInvalid "read2"
    "pair(bool, bool) p = null; read fst p"
    statements

testScopeSemantic1 :: IO Bool
testScopeSemantic1 = testValid "scope1"
    "int a = 0; print a"
    statements

testScopeSemantic2 :: IO Bool
testScopeSemantic2 = testInvalid "scope2"
    "int x = 12; begin bool x = true; x = 5 end; exit x"
    statements

testVariable1 :: IO Bool
testVariable1 = testValid "variable1"
    "int a = 1"
    statements

testVariable2 :: IO Bool
testVariable2 = testInvalid "variable2"
    "bool a = 1"
    statements

testWhileSemantic1 :: IO Bool
testWhileSemantic1 = testValid "while1"
    "while true do skip done"
    statements

testWhileSemantic2 :: IO Bool
testWhileSemantic2 = testInvalid "while2"
    "while tru do skip done"
    statements

testStringWeakening :: IO Bool
testStringWeakening
  = testValid "char[] can be assigned to string"
    "string s = ['a', 'b', 'c']"
    statements

testInvariant1 :: IO Bool
testInvariant1
  = testInvalid "char[][] cannot be assigned to string[]"
    "char[] c1 = ['a', 'b']; char[] c2 = ['c', 'd']; char[][] cs = [c1, c2]; string[] strs = cs"
    statements

testInvariant2 :: IO Bool
testInvariant2
  = testValid "char[][] with variables can be assigned to string[]"
    "char[] c1 = ['a', 'b']; char[] c2 = ['c', 'd']; string[] strs = [c1, c2]"
    statements

testTypeIncompatibleDeclare :: IO Bool
testTypeIncompatibleDeclare
  = testInvalid "int cannot be assigned to char"
    "char c = 10"
    statements

testTypeIncompatibleAssign :: IO Bool
testTypeIncompatibleAssign
  = testInvalid "int cannot be assigned to bool"
    "bool b = true; b = 0"
    statements

testFunctionReturnType :: IO Bool
testFunctionReturnType
  = testInvalid "value returned does not match the return type"
    "int foo() is return 'a' end"
    function

testParameterNameRepeatWithVariableName :: IO Bool
testParameterNameRepeatWithVariableName
  = testValid "declare the same variable in body as function parameters"
    "int foo(int x) is int x = 0; return x end"
    function

testFunctionNameRepeatWithVariableName :: IO Bool
testFunctionNameRepeatWithVariableName
  = testValid "declare the variable name as the same as the function name"
    "begin int foo() is return 0 end int foo = call foo() end"
    program

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
    testWhileSemantic2,
    testStringWeakening,
    testInvariant1,
    testInvariant2,
    testTypeIncompatibleDeclare,
    testTypeIncompatibleAssign,
    testFunctionReturnType,
    testParameterNameRepeatWithVariableName,
    testFunctionNameRepeatWithVariableName
    ]
