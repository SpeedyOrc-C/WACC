module Test.WACC.Backend.IR where
import WACC.IR.FlattenExpression (expression, initialState, FlattenerState (FlattenerState))
import Data.Functor.Identity
import Control.Monad.Trans.State.Lazy
import Data.Map as M
import qualified WACC.Semantics.Structure as SM
import WACC.IR.Structure

{- Helper function for testing expressions -}
testExpression :: SM.Expression -> (Scalar, [SingleStatement])
                    -> FlattenerState -> IO Bool
testExpression expr (s, ss) fs =
    case runIdentity (evalStateT (expression expr) fs) of
        (s', ss') -> do
            if s == s' && ss == ss'
            then return True
            else return False

emptyState :: FlattenerState
emptyState = initialState M.empty

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
    let expected = (Variable (Temporary "var" 1), [Assign B4 (Temporary "var" 1) (Subtract (Immediate 0) (Immediate 5))])
    testExpression expr expected s

testBinaryAdd :: IO Bool
testBinaryAdd = do
    let s = emptyState
    let expr = SM.Add (SM.LiteralInt 2) (SM.LiteralInt 3)
    let expected = (Variable (Temporary "var" 1), [Assign B4 (Temporary "var" 1) (Add (Immediate 2) (Immediate 3))])
    testExpression expr expected s

-- Test a function call expression
testFunctionCall :: IO Bool
testFunctionCall = do
    let s = emptyState
    let expr = SM.FunctionCall SM.Int "func" []
    let expected = (Variable (Temporary "var" 1), [Assign B4 (Temporary "var" 1) (Call B4 "fn_func" [])])
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
    testFunctionCall
  ]
