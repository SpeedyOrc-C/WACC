module Test.WACC.Backend.IR where
import Data.Functor.Identity
import Control.Monad.Trans.State.Lazy
import Data.Map as M
import qualified WACC.Semantics.Structure as SM
import WACC.IR.Structure
import WACC.IR.FlattenExpression
import Text.AnsiEscape

{- Helper function for testing expressions -}
testExpression :: SM.Expression -> (Scalar, [SingleStatement])
                    -> FlattenerState -> IO Bool
testExpression expr (s, ss) fs =
    case runIdentity (evalStateT (expression expr) fs) of
        (s', ss') -> do
            if s == s' && ss == ss'
            then do 
                putStrLn $ green "    * " ++ show expr
                return True
            else do
                putStrLn $ red "    ! " ++ show expr
                return False

emptyState :: FlattenerState
emptyState = initialState M.empty

state1 :: FlattenerState
state1 = FlattenerState {
    mappingStack = [M.fromList [("x", Identifier "x" 1)]],
    variableCounter = M.size M.empty + 1,
    dataSegment = M.empty
}

{- Tests for expressions. -}
testLiteralBool :: IO Bool
testLiteralBool = testExpression (SM.LiteralBool True) (Immediate 1, [])
                    emptyState

testLiteralInt :: IO Bool
testLiteralInt  = testExpression (SM.LiteralInt 5) (Immediate 5, [])
                    emptyState

testLiteralChar :: IO Bool
testLiteralChar  = testExpression (SM.LiteralChar 'c') (Immediate 99, [])
                    emptyState

testLiteralPairNull :: IO Bool
testLiteralPairNull  = testExpression SM.LiteralPairNull (Immediate 0, [])
                    emptyState

testLiteralString :: IO Bool
testLiteralString = do
    let ds = M.fromList [("test_string", 123)]
    let s = initialState ds
    let expected = (String 123, [])
    testExpression (SM.LiteralString "test_string") expected s

testLiteralArray :: IO Bool
testLiteralArray = do
    let s = emptyState
    let expr = SM.LiteralArray SM.Int [SM.LiteralInt 1, SM.LiteralInt 2,
                                       SM.LiteralInt 3]
    let expected = (Variable (Temporary "var" 1), [Assign B8
                    (Temporary "var" 1) (NewArray B4 [Immediate 1, Immediate 2,
                    Immediate 3])])
    testExpression expr expected s

testLiteralPair :: IO Bool
testLiteralPair = do
    let s = emptyState
    let expr = SM.LiteralPair (SM.Int, SM.Char) (SM.LiteralInt 42,
                                                 SM.LiteralChar 'a')
    let expected = (Variable (Temporary "var" 1), [Assign B8
                    (Temporary "var" 1) (NewPair (B4, B1) (Immediate 42,
                    Immediate 97))])
    testExpression expr expected s

testUnaryNegate :: IO Bool
testUnaryNegate = do
    let s = emptyState
    let expr = SM.Negate (SM.LiteralInt 5)
    let expected = (Variable (Temporary "var" 1), [Assign B4
                    (Temporary "var" 1) (Subtract (Immediate 0) (Immediate 5))])
    testExpression expr expected s

testBinaryAdd :: IO Bool
testBinaryAdd = do
    let s = emptyState
    let expr = SM.Add (SM.LiteralInt 2) (SM.LiteralInt 3)
    let expected = (Variable (Temporary "var" 1), [Assign B4
                    (Temporary "var" 1) (Add (Immediate 2) (Immediate 3))])
    testExpression expr expected s

testFunctionCall :: IO Bool
testFunctionCall = do
    let s = emptyState
    let expr = SM.FunctionCall SM.Int "func" []
    let expected = (Variable (Temporary "var" 1), [Assign B4
                    (Temporary "var" 1) (Call B4 "fn_func" [])])
    testExpression expr expected s

testIndirectIdentifier :: IO Bool
testIndirectIdentifier = do
    let s = state1
    let expr = SM.Identifier SM.Int "x"
    let expected = (Variable (Identifier "x" 1), [])
    testExpression expr expected s

testArrayElementIdentifier :: IO Bool
testArrayElementIdentifier = do
    let s = state1
    let arrayExpr = SM.Identifier SM.Int "x"
    let indexExpr = SM.LiteralInt 2
    let expr = SM.ArrayElement SM.Int arrayExpr indexExpr
    let expected = (Variable (Temporary "var" 2),[Assign B8 (Temporary "var" 1)
                   (SeekArrayElement B4 (Variable (Identifier "x" 1)) 
                   (Immediate 2)),Assign B4 (Temporary "var" 2) 
                   (Dereference B4 (Variable (Temporary "var" 1)))])
    testExpression expr expected s

testPairFirstIdentifier :: IO Bool
testPairFirstIdentifier = do
    let s = state1
    let pairExpr = SM.Identifier (SM.Pair(SM.Int, SM.Char)) "x"
    let expr = SM.PairFirst SM.Int pairExpr
    let expected = (Variable (Temporary "var" 2),[Assign B8 (Temporary "var" 1)
                   (SeekPairFirst (Variable (Identifier "x" 1))),
                   Assign B4 (Temporary "var" 2) 
                   (Dereference B4 (Variable (Temporary "var" 1)))])
    testExpression expr expected s

testPairSecondIdentifier :: IO Bool
testPairSecondIdentifier = do
    let s = state1
    let pairExpr = SM.Identifier (SM.Pair(SM.Int, SM.Char)) "x"
    let expr = SM.PairSecond SM.Int pairExpr
    let expected = (Variable (Temporary "var" 2),[Assign B8 (Temporary "var" 1)
                   (SeekPairSecond (Variable (Identifier "x" 1))),
                   Assign B4 (Temporary "var" 2) 
                   (Dereference B4 (Variable (Temporary "var" 1)))])
    testExpression expr expected s

irUnitTests :: IO [Bool]
irUnitTests = sequence [
    testLiteralBool,
    testLiteralInt,
    testLiteralChar,
    testLiteralPairNull,
    testLiteralString,
    testLiteralArray,
    testLiteralPair,
    testUnaryNegate,
    testBinaryAdd,
    testFunctionCall,
    testIndirectIdentifier,
    testArrayElementIdentifier,
    testPairFirstIdentifier,
    testPairSecondIdentifier
  ]
