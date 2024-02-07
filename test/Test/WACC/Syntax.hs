module Test.WACC.Syntax where

import Text.Parser
import WACC.Syntax.Parser
import WACC.Syntax.Structure
import WACC.Syntax.Error (WaccSyntaxErrorType(..))

shouldSucceed :: String -> String -> Parser error t -> (t -> Bool) -> IO Bool
shouldSucceed testName input parser f
  = case parseString (strict parser) input of
    Right (Parsed _ ast _) ->
        if f ast then do
            putStrLn $ "[PASS] " ++ testName
            return True
        else do
            putStrLn $ "[FAIL] " ++ testName
            return False
    Left {} -> do
        putStrLn $ "[FAIL] " ++ testName
        return False

shouldFailNothing :: String -> String -> Parser error t -> IO Bool
shouldFailNothing testName input parser
  = case parseString (strict parser) input of
    Left Nothing -> do
        putStrLn $ "[PASS] " ++ testName
        return True
    _ -> do
      putStrLn $ "[FAIL] " ++ testName
      return False

shouldFailSyntaxError :: Eq error =>
  String -> String -> Parser error t -> error -> IO Bool
shouldFailSyntaxError testName input parser expectedError
  = case parseString (strict parser) input of
    Left (Just (SyntaxError _ actualError)) ->
      if actualError == expectedError then do
        putStrLn $ "[PASS] " ++ testName
        return True
      else do
        putStrLn $ "[FAIL] " ++ testName
        return False
    _ -> do
        putStrLn $ "[FAIL] " ++ testName
        return False

testIdentifier1 :: IO Bool
testIdentifier1
  = shouldSucceed "normal identifier"
    "aBW_311"
    expressionIdentifier
    (\case (Identifier s _) -> s == "aBW_311" ; _ -> False)

testIdentifier2 :: IO Bool
testIdentifier2
  = shouldFailNothing "error identifier with a colon"
    "aBW_311;"
    expressionIdentifier

testIdentifier3 :: IO Bool
testIdentifier3
  = shouldFailNothing "error identifier with invalid chars"
    "a#@BW_311$$"
    expressionIdentifier


testBoolLiteral1 :: IO Bool
testBoolLiteral1
  = shouldSucceed "normal bool literal"
    "true"
    expressionLiteralBool
    (\case (LiteralBool b _) -> b ; _ -> False)

testBoolLiteral2 :: IO Bool
testBoolLiteral2
  = shouldFailNothing "error bool literal in double quotes)"
    "\"false\""
    expressionLiteralBool

testNoSignIntLiteral1 :: IO Bool
testNoSignIntLiteral1
  = shouldSucceed "normal int literal without sign"
    "53"
    expressionNoSignLiteralInt
    (\case (LiteralInt i _) -> i == 53 ; _ -> False)

testNoSignIntLiteral2 :: IO Bool
testNoSignIntLiteral2
  = shouldFailNothing "int literal with sign"
    "-655"
    expressionNoSignLiteralInt

testIntLiteral1 :: IO Bool
testIntLiteral1
  = shouldSucceed "normal int literal with sign"
    "-2003"
    expressionLiteralInt
    (\case (LiteralInt i _) -> i == -2003 ; _ -> False)

testIntLiteral2 :: IO Bool
testIntLiteral2
  = shouldFailNothing "error int literal with invalid chars"
    "-655**"
    expressionLiteralInt

testCharLiteral1 :: IO Bool
testCharLiteral1
  = shouldSucceed "normal char literal"
    "\'x\'"
    expressionLiteralChar
    (\case (LiteralChar c _) -> c == 'x' ; _ -> False)

testCharLiteral2 :: IO Bool
testCharLiteral2
  = shouldFailSyntaxError "char literal without any char"
    "\'\'"
    expressionLiteralChar
    ExpectOneCharacter

testCharLiteral3 :: IO Bool
testCharLiteral3
  = shouldFailSyntaxError "error char literal without right quote"
    "\'x"
    expressionLiteralChar
    UnmatchedSingleQuote

testStringLiteral1 :: IO Bool
testStringLiteral1
  = shouldSucceed "normal string literal"
    "\"Hello\""
    expressionLiteralString
    (\case (LiteralString s _) -> s == "Hello" ; _ -> False)

testStringLiteral2 :: IO Bool
testStringLiteral2
  = shouldFailNothing "error string literal with two double quotes at the end"
    "\"Hello\"\""
    expressionLiteralString

testStringLiteral3 :: IO Bool
testStringLiteral3
  = shouldFailSyntaxError "error string literal with unmatched double quote"
    "\"Hello"
    expressionLiteralString
    UnmatchedDoubleQuote

testArrayLiteral1 :: IO Bool
testArrayLiteral1
  = shouldSucceed "normal array literal"
    "[1, 2]"
    expressionLiteralArray
    (\case (LiteralArray [LiteralInt n1 _, LiteralInt n2 _] _) -> 
            n1 == 1 && n2 == 2 ; _ -> False)

testArrayLiteral2 :: IO Bool
testArrayLiteral2
  = shouldFailNothing "error array literal with something after bracket"
    "[1, 2] xx"
    expressionLiteralArray

testPairLiteral1 :: IO Bool
testPairLiteral1
  = shouldSucceed "normal pair literal"
    "newpair (5, 3)"
    expressionLiteralPair
    (\case (LiteralPair (LiteralInt n1 _, LiteralInt n2 _) _) -> 
            n1 == 5 && n2 == 3 ; _ -> False)

testPairLiteral2 :: IO Bool
testPairLiteral2
  = shouldFailNothing "error pair literal with something after parenthesis"
    "newpair (5, 3) abs"
    expressionLiteralPair

testNullPairLiteral1 :: IO Bool
testNullPairLiteral1
  = shouldSucceed "normal null pair literal"
    "null"
    expressionLiteralPairNull
    (\case (LiteralPairNull () _) -> True ; _ -> False)

testNullPairLiteral2 :: IO Bool
testNullPairLiteral2
  = shouldFailNothing "error null pair literal with something after null"
    "nullavs"
    expressionLiteralPairNull

testFunctionCall1 :: IO Bool
testFunctionCall1
  = shouldSucceed "normal function call"
    "call increment(5)"
    expressionFunctionCall
    (\case (FunctionCall (fname, [LiteralInt n1 _]) _) -> 
            fname == "increment" && n1 == 5 ; _ -> False)

testFunctionCall2 :: IO Bool
testFunctionCall2
  = shouldFailNothing "error function call with something after parenthesis"
    "call increment(5);8965%$"
    expressionFunctionCall

syntaxUnitTests :: IO [Bool]
syntaxUnitTests = sequence [
    testIdentifier1,
    testIdentifier2,
    testIdentifier3,
    testBoolLiteral1,
    testBoolLiteral2,
    testNoSignIntLiteral1,
    testNoSignIntLiteral2,
    testIntLiteral1,
    testIntLiteral2,
    testCharLiteral1,
    testCharLiteral2,
    testCharLiteral3,
    testStringLiteral1,
    testStringLiteral2,
    testStringLiteral3,
    testArrayLiteral1,
    testArrayLiteral2,
    testPairLiteral1,
    testPairLiteral2,
    testNullPairLiteral1,
    testNullPairLiteral2,
    testFunctionCall1,
    testFunctionCall2
  ]
