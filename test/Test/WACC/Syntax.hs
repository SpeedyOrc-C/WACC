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

syntaxUnitTests :: IO [Bool]
syntaxUnitTests = sequence [
    testStringLiteral1,
    testStringLiteral2,
    testStringLiteral3
  ]
