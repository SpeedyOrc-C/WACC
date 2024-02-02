module WACC.Semantics.Utils where

import Text.Parser (Range)
import WACC.Semantics.Error (WaccSemanticsErrorType)
import WACC.Semantics.Structure (Type (..))
import qualified WACC.Syntax.Structure as Syntax
import qualified Data.Map as M

data SemanticError = SemanticError Range WaccSemanticsErrorType deriving Show

-- | A special case of the built-in Either type,
--   where the Left errors are collected.
data CheckerResult a = Bad [SemanticError] | Ok a deriving Show

instance Functor CheckerResult where
    fmap :: (a -> b) -> CheckerResult a -> CheckerResult b
    fmap _ (Bad errors) = Bad errors
    fmap f (Ok result) = Ok $ f result

instance Applicative CheckerResult where
    pure :: a -> CheckerResult a
    pure = Ok

    (<*>) :: CheckerResult (a -> b) -> CheckerResult a -> CheckerResult b
    -- Here, the errors are collected.
    -- But the Either type only collects the first error.
    Bad errors <*> Bad errors' = Bad $ errors ++ errors'
    _ <*> Bad errors = Bad errors
    Bad errors <*> _ = Bad errors
    Ok f <*> Ok x = Ok $ f x

instance Monad CheckerResult where
    return :: a -> CheckerResult a
    return = pure

    (>>=) :: CheckerResult a -> (a -> CheckerResult b) -> CheckerResult b
    Bad errors >>= _ = Bad errors
    Ok result >>= f = f result

mergeMany :: Foldable t => t (CheckerResult a) -> CheckerResult [a]
mergeMany xs = reverse <$> foldl (\b a -> (:) <$> a <*> b) (Ok []) xs

data CheckerState = CheckerState {
    -- Is it inside a function? If so, what type does this function returns?
    functionContext :: Maybe Type,
    -- All functions' types of parameters and return value
    functionMapping :: String `M.Map` ([Type], Type),
    -- All variables' types
    mappingStack :: [String `M.Map` Type]
}

goDeeper :: CheckerState -> CheckerState
goDeeper state = state { mappingStack = M.empty : mappingStack state }

addIdentifier :: String -> Type -> CheckerState -> CheckerState
addIdentifier name t state = state {
    mappingStack =
        M.insert name t (head (mappingStack state)) :
        tail (mappingStack state)
}

lookUp :: CheckerState -> String -> Maybe Type
lookUp state name = case mappingStack state of
    [] -> Nothing
    mapping : rest -> case M.lookup name mapping of
        Just t -> Just t
        Nothing -> lookUp (state {mappingStack = rest}) name

lookUpInnermost :: CheckerState -> String -> Maybe Type
lookUpInnermost state =
    lookUp $ state { mappingStack = [head (mappingStack state)] }

-- | Can the right type take the place of the left type?
(<|) :: Type -> Type -> Bool
Any <| _ = True
_ <| Any = True
String <| Array Char = True
String <| Array Any = True
Array a <| Array b = a <| b
Pair (a, b) <| Pair (c, d) = a <| c && b <| d
a <| b = a == b

fromSyntaxType :: Syntax.Type -> Type
fromSyntaxType = \case
    Syntax.Int {} -> Int
    Syntax.Bool {} -> Bool
    Syntax.Char {} -> Char
    Syntax.String {} -> String
    Syntax.Array t _ -> Array (fromSyntaxType t)
    Syntax.Pair Nothing _ -> Pair (Any, Any)
    Syntax.Pair (Just (a, b)) _ -> Pair (fromSyntaxType a, fromSyntaxType b)

-- | Given that two types are compatible with each other,
--   find the common type between two types. This can find the type of an array.
common :: Type -> Type -> Type
common Any t = t
common t Any = t

common String (Array Char) = String
common (Array Char) String = String

common (Array Any) String = String
common String (Array Any) = String

common (Array a) (Array b) = Array (common a b)
common (Pair (a, b)) (Pair (c, d)) = Pair (common a c, common b d)

common a b = if a == b then a else undefined
