module Test.WACC.Syntax where

import Text.Parser
import WACC.Syntax.Parser
import WACC.Syntax.Structure

test :: String -> String -> Parser error t -> (t -> Bool) -> IO Bool
test name input parser f = case parseString (strict parser) input of
    Right (Parsed _ ast _) ->
        if f ast then do
            putStrLn $ "[PASS] " ++ name
            return True
        else do
            putStrLn $ "[FAIL] " ++ name
            return False
    Left {} -> do
        putStrLn $ "[FAIL] " ++ name
        return False

testString1 :: IO Bool
testString1 = test "check normal string"
    "\"Hello\""
    expressionLiteralString
    (\case (LiteralString s _) -> s == "Hello" ; _ -> False)

-- testString2 :: IO Bool
-- testString2 = test "check error string"
--     "\"Hello\"\""
--     expressionLiteralString
--     (\case ())

syntaxTests :: IO [Bool]
syntaxTests = sequence [
    testString1
    ]
