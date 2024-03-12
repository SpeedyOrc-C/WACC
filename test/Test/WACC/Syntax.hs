module Test.WACC.Syntax where

import Text.Parser
import WACC.Syntax.Parser
import WACC.Syntax.Structure
import WACC.Syntax.Error (WaccSyntaxErrorType(..))
import Text.AnsiEscape

shouldSucceed :: String -> String -> Parser error t -> (t -> Bool) -> IO Bool
shouldSucceed testName input parser f
  = case parseString (strict parser) input of
    Right (Parsed _ ast _) ->
        if f ast then do
            putStrLn $ green "    * " ++ testName
            return True
        else do
            putStrLn $ red "    ! " ++ testName
            return False
    Left {} -> do
        putStrLn $ red "    ! " ++ testName
        return False

shouldFailNothing :: String -> String -> Parser error t -> IO Bool
shouldFailNothing testName input parser
  = case parseString (strict parser) input of
    Left Nothing -> do
        putStrLn $ green "    * " ++ testName
        return True
    _ -> do
      putStrLn $ red "    ! " ++ testName
      return False

shouldFailSyntaxError :: Eq error =>
  String -> String -> Parser error t -> error -> IO Bool
shouldFailSyntaxError testName input parser expectedError
  = case parseString (strict parser) input of
    Left (Just (SyntaxError _ actualError)) ->
      if actualError == expectedError then do
        putStrLn $ green "    * " ++ testName
        return True
      else do
        putStrLn $ red "    ! " ++ testName
        return False
    _ -> do
        putStrLn $ red "    ! " ++ testName
        return False

testIdentifier1 :: IO Bool
testIdentifier1
  = shouldSucceed "normal identifier"
    "aBW_311"
    expressionIdentifier
    (\case (Identifier "aBW_311" _) -> True ; _ -> False)

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
    (\case (LiteralBool True _) -> True ; _ -> False)

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
    (\case (LiteralInt 53 _) -> True ; _ -> False)

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
    (\case (LiteralInt (-2003) _) -> True ; _ -> False)

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
    (\case (LiteralChar 'x' _) -> True ; _ -> False)

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
    (\case (LiteralString "Hello" _) -> True ; _ -> False)

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
    (\case (LiteralArray [LiteralInt 1 _, LiteralInt 2 _] _) ->
            True; _ -> False)

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
    (\case (LiteralPair (LiteralInt 5 _, LiteralInt 3 _) _) ->
            True; _ -> False)

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
    (\case (FunctionCall ("increment", [LiteralInt 5 _]) _) ->
            True ; _ -> False)

testFunctionCall2 :: IO Bool
testFunctionCall2
  = shouldFailNothing "error function call with something after parenthesis"
    "call increment(5);8965%$"
    expressionFunctionCall

testArrayElement1 :: IO Bool
testArrayElement1
  = shouldSucceed "normal array element"
    "A [1]"
    expressionArrayElement
    (\case (ArrayElement (Identifier "A" _,LiteralInt 1 _) _)-> True ;
             _ -> False)

testArrayElement2 :: IO Bool
testArrayElement2
  = shouldFailSyntaxError "error array element without right bracket"
    "A [99"
    expressionArrayElement
    UnmatchedSquareBracket

testUnaryOperation1 :: IO Bool
testUnaryOperation1
  = shouldSucceed "normal unary operation"
    "! true"
    expressionUnaryOperation
    (\case (Not (LiteralBool True _) _)-> True ;
             _ -> False)

testUnaryOperation2 :: IO Bool
testUnaryOperation2
  = shouldFailSyntaxError "abnormal unary operation"
    "len "
    expressionUnaryOperation
    ExpectOperand

testBinaryOperation1 :: IO Bool
testBinaryOperation1
  = shouldSucceed "normal binary operation"
    "5 * 3"
    expressionBinaryOperation
    (\case (Multiply (LiteralInt 5 _, LiteralInt 3 _) _)-> True ;
             _ -> False)

testBinaryOperation2 :: IO Bool
testBinaryOperation2
  = shouldFailSyntaxError "abnormal binary operation"
    "true || "
    expressionBinaryOperation
    ExpectOperand

testexpressionWithBrackets1 :: IO Bool
testexpressionWithBrackets1
  = shouldSucceed "normal expression with brackets"
    "((5 + 3) / 4)"
    expressionWithBrackets
    (\case (Divide (Add (LiteralInt 5 _,LiteralInt 3 _) _,LiteralInt 4 _) _)
            -> True ; _ -> False)

testexpressionWithBrackets2 :: IO Bool
testexpressionWithBrackets2
  = shouldFailSyntaxError "abnormal expression with brackets"
    "(8 + 9"
    expressionWithBrackets
    UnmatchedBracket

testLeftValue1 :: IO Bool
testLeftValue1
  = shouldSucceed "normal left value"
    "Sora"
    leftValue
    (\case (Identifier "Sora" _) -> True ; _ -> False)

testLeftValue2 :: IO Bool
testLeftValue2
  = shouldFailNothing "error left value which is actually a right value"
    "(2003 + 11 + 20)"
    leftValue

testRightValue1 :: IO Bool
testRightValue1
  = shouldSucceed "normal right value"
    "1 + 2"
    rightValue
    (\case (Add (LiteralInt 1 _,LiteralInt 2 _) _) -> True ; _ -> False)

testStrictExpression1 :: IO Bool
testStrictExpression1
  = shouldSucceed "valid strict expression"
    "Sora"
    strictExpression
    (\case (Identifier "Sora" _) -> True ; _ -> False)

testStrictExpression2 :: IO Bool
testStrictExpression2
  = shouldFailSyntaxError "error strict value with something followed"
    "-2003**"
    strictExpression
    ExpectOperand

testSkip :: IO Bool
testSkip
  = shouldSucceed "normal skip statement"
    "skip"
    statementSkip
    (\case (Skip () _) -> True ; _ -> False)

testRead1 :: IO Bool
testRead1
  = shouldSucceed "normal read statement"
    "read num"
    statementRead
    (\case (Read (Identifier "num" _) _) -> True ; _ -> False)

testRead2 :: IO Bool
testRead2
  = shouldFailSyntaxError "error read statement for reading a non-lvalue"
    "read (1 + 2)"
    statementRead
    InvalidLeftValue

testFree1 :: IO Bool
testFree1
  = shouldSucceed "normal free statement"
    "free array"
    statementFree
    (\case (Free (Identifier "array" _) _) -> True ; _ -> False)

testFree2 :: IO Bool
testFree2
  = shouldFailSyntaxError "error free statement for freeing invalid expression"
    "free newpair(1, 2)"
    statementFree
    ExpectOneExpression

testExit :: IO Bool
testExit
  = shouldSucceed "normal exit statement"
    "exit 1"
    statementExit
    (\case (Exit (LiteralInt 1 _) _) -> True ; _ -> False)

testPrint :: IO Bool
testPrint
  = shouldSucceed "normal print statement"
    "print 3"
    statementPrint
    (\case (Print (LiteralInt 3 _) _) -> True ; _ -> False)

testPrintLine :: IO Bool
testPrintLine
  = shouldSucceed "normal println statement"
    "println 3"
    statementPrintLine
    (\case (PrintLine (LiteralInt 3 _) _) -> True ; _ -> False)

testReturn :: IO Bool
testReturn
  = shouldSucceed "normal return statement"
    "return 0"
    statementReturn
    (\case (Return (LiteralInt 0 _) _) -> True ; _ -> False)

testIf1 :: IO Bool
testIf1
  = shouldSucceed "normal if statement"
    "if 1 < 2 then int a = 0; return a else return 1 fi"
    statementIf
    (\case (If (Less (LiteralInt 1 _, LiteralInt 2 _) _,
            [Declare (Int () _, "a", LiteralInt 0 _) _,
            Return (Identifier "a" _) _],
            [Return (LiteralInt 1 _) _]) _) -> True ; _ -> False)

testIf2 :: IO Bool
testIf2
  = shouldFailSyntaxError "error if statement without else"
    "if 1 < 2 then int a = 0; return a fi"
    statementIf
    ExpectElse

testIf3 :: IO Bool
testIf3
  = shouldFailSyntaxError "error if statement without else clause"
    "if 1 < 2 then int a = 0; return a else fi"
    statementIf
    ExpectOneStatement

testWhile1 :: IO Bool
testWhile1
  = shouldSucceed "normal while statement"
    "while a < 10 do a = a + 1 done"
    statementWhile
    (\case (While (Less (Identifier "a" _, LiteralInt 10 _) _,
            [Assign (Identifier "a" _, Add (Identifier "a" _, LiteralInt 1 _)
            _) _]) _) -> True ; _ -> False)

testWhile2 :: IO Bool
testWhile2
  = shouldFailSyntaxError "error while statement without do clause"
    "while a < 10 do done"
    statementWhile
    ExpectOneStatement

testScope1 :: IO Bool
testScope1
  = shouldSucceed "normal scope"
    "begin int a = 0; return a end"
    statementScope
    (\case (Scope [Declare (Int () _, "a", LiteralInt 0 _) _,
            Return (Identifier "a" _) _] _) -> True ; _ -> False)

testScope2 :: IO Bool
testScope2
  = shouldFailSyntaxError "error scope without a scope body"
    "begin end"
    statementScope
    ExpectOneStatement

testType1 :: IO Bool
testType1
  = shouldSucceed "normal pair array type"
    "pair(int, char)[]"
    type'
    (\case (Array (Pair (Just (Int () _, Char () _)) _) _) -> True ; _ -> False)

testType2 :: IO Bool
testType2
  = shouldSucceed "normal pair of array type"
    "pair(int[], char[])"
    type'
    (\case (Pair (Just (Array (Int () _) _, Array (Char () _) _)) _) -> True ;
            _ -> False)

testType3 :: IO Bool
testType3
  = shouldSucceed "normal pair of pair type"
    "pair(pair, pair)"
    type'
    (\case (Pair (Just (Pair Nothing _, Pair Nothing _)) _) -> True ;
            _ -> False)

testType4 :: IO Bool
testType4
  = shouldFailSyntaxError "error erased pair array type"
    "pair[]"
    type'
    PairTypeErased

testType5 :: IO Bool
testType5
  = shouldFailSyntaxError "error non-erased pair type in pair type"
    "pair(pair(int, char), pair(int[], char[]))"
    type'
    PairTypeInPairTypeNotErased

testDeclare1 :: IO Bool
testDeclare1
  = shouldSucceed "normal declare statement"
    "int x = 1"
    statementDeclare
    (\case (Declare (Int () _, "x", LiteralInt 1 _) _) -> True ; _ -> False)

testDeclare2 :: IO Bool
testDeclare2
  = shouldFailSyntaxError "error declare without an identifier"
    "int = 1"
    statementDeclare
    ExpectIdentifierInDeclaration

testAssign1 :: IO Bool
testAssign1
  = shouldSucceed "normal assign statement"
    "x = 1"
    statementAssign
    (\case (Assign (Identifier "x" _, LiteralInt 1 _) _) -> True ; _ -> False)

testAssign2 :: IO Bool
testAssign2
  = shouldFailSyntaxError "error assign statement with no equal sign"
    "x  1"
    statementAssign
    ExpectAssignEqualSign

testStatements1 :: IO Bool
testStatements1
  = shouldSucceed "normal statements"
    "int a = 0; return a"
    statements
    (\case [Declare (Int () _, "a", LiteralInt 0 _) _,
            Return (Identifier "a" _) _] -> True ; _ -> False)

testStatements2 :: IO Bool
testStatements2
  = shouldFailNothing "error statements without semicolon"
    "int a = 0  \n  return a"
    statements

testFunction1 :: IO Bool
testFunction1
  = shouldSucceed "normal function"
    "int foo(int a) is a = 0; return a end"
    function
    (\case (Function (Int () (0, 3), Name "foo" (4, 7),
            [(Name "a" (12, 13), Int () (8, 11))],
            [Assign (Identifier "a" (18, 19), LiteralInt 0 (22, 23)) (18, 23),
            Return (Identifier "a" (32, 33)) (25, 33)]) (0, 37))
            -> True; _ -> False)

testFunction2 :: IO Bool
testFunction2
  = shouldFailSyntaxError "error function with the last statement not returning"
    "int foo(int a) is a = 0 end"
    function
    (FunctionDoesNotReturn (Name "foo" (4, 7)))

testFunction3 :: IO Bool
testFunction3
  = shouldFailSyntaxError "error function that does not return for some paths"
    "int foo(int a) is if a == 0 then return a else a = 1 fi end"
    function
    (FunctionDoesNotReturn (Name "foo" (4, 7)))

testProgram1 :: IO Bool
testProgram1
  = shouldSucceed "normal program"
    "begin int foo(int a) is a = 0; return a end \n\n int x = call foo(10)  end"
    program
    (\case (Program ([], [Function (Int () (6, 9), Name "foo" (10, 13),
            [(Name "a" (18, 19), Int () (14, 17))],
            [Assign (Identifier "a" (24, 25), LiteralInt 0 (28, 29)) (24, 29),
            Return (Identifier "a" (38, 39)) (31, 39)]) (6, 43)],
            [Declare (Int () (47, 50), "x", FunctionCall ("foo",
            [LiteralInt 10 (64, 66)]) (55, 67)) (47, 67)]) (0, 72))
            -> True ; _ -> False)

testProgram2 :: IO Bool
testProgram2
  = shouldFailSyntaxError "error program with no statements after function defs"
    "begin int foo(int a) is a = 0; return a end end"
    program
    ExpectOneStatement

testProgram3 :: IO Bool
testProgram3
  = shouldFailSyntaxError "error program with codes after program end"
    "begin int foo(int a) is a = 0; return a end \n\n int x = 0 end x = 1"
    program
    UnexpectedCodeAfterProgramEnd

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
    testFunctionCall2,
    testArrayElement1,
    testArrayElement2,
    testUnaryOperation1,
    testUnaryOperation2,
    testBinaryOperation1,
    testBinaryOperation2,
    testexpressionWithBrackets1,
    testexpressionWithBrackets2,
    testLeftValue1,
    testLeftValue2,
    testRightValue1,
    testStrictExpression1,
    testStrictExpression2,
    testSkip,
    testRead1,
    testRead2,
    testFree1,
    testFree2,
    testExit,
    testPrint,
    testPrintLine,
    testReturn,
    testIf1,
    testIf2,
    testIf3,
    testWhile1,
    testWhile2,
    testScope1,
    testScope2,
    testType1,
    testType2,
    testType3,
    testType4,
    testType5,
    testDeclare1,
    testDeclare2,
    testAssign1,
    testAssign2,
    testStatements1,
    testStatements2,
    testFunction1,
    testFunction2,
    testFunction3,
    testProgram1,
    testProgram2,
    testProgram3
  ]
