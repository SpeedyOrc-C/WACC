module Test.WACC.Backend.IR where
import WACC.IR.FlattenExpression (expression, initialState)
import Data.Functor.Identity
import Control.Monad.Trans.State.Lazy
import Data.Map as M
import qualified WACC.Semantics.Structure as SM
import WACC.IR.Structure

{- Helper function for testing expressions -}
testExpression :: SM.Expression -> (Scalar, [SingleStatement]) -> IO Bool
testExpression expr (s, ss) =
    case runIdentity (evalStateT (expression expr) (initialState M.empty)) of
        (s', ss') -> do
            if s == s' && ss == ss'
            then return True
            else return False

{- Tests for expressions. -}
testLiteralBool :: IO Bool
testLiteralBool = testExpression (SM.LiteralBool True) (Immediate 1, [])

testLiteralInt :: IO Bool
testLiteralInt  = testExpression (SM.LiteralInt 5) (Immediate 5, [])

testLiteralChar :: IO Bool
testLiteralChar  = testExpression (SM.LiteralChar 'c') (Immediate 99, [])

testLiteralPairNull :: IO Bool
testLiteralPairNull  = testExpression SM.LiteralPairNull (Immediate 0, [])


irUnitTests :: IO [Bool]
irUnitTests = sequence [
    testLiteralBool,
    testLiteralInt,
    testLiteralChar,
    testLiteralPairNull
  ]
