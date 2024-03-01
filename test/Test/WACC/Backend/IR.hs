module Test.WACC.Backend.IR where
import WACC.IR.FlattenExpression (expression, initialState)
import Data.Functor.Identity
import Control.Monad.Trans.State.Lazy
import Data.Map as M
import qualified WACC.Semantics.Structure as SM
import WACC.IR.Structure

testExpression :: SM.Expression -> (Scalar, [SingleStatement]) -> IO Bool
testExpression expr (s, ss) =
    case runIdentity (evalStateT (expression expr) (initialState M.empty)) of
        (s', ss') -> do
            if s == s' && ss == ss'
            then return True
            else return False

testBool :: IO Bool
testBool = testExpression (SM.LiteralBool True) (Immediate 1, [])

irUnitTests :: IO [Bool]
irUnitTests = sequence [
    testBool
  ]
