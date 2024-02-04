module WACC.Semantics.Utils where

import Text.Parser (Range)
import WACC.Semantics.Error (WaccSemanticsErrorType)
import WACC.Semantics.Structure (Type (..))
import qualified WACC.Syntax.Structure as Syntax
import qualified Data.Map as M

data SemanticError = SemanticError Range WaccSemanticsErrorType

instance Show SemanticError where
    show (SemanticError range error') = show range ++ " " ++ show error'

-- | A special case of the built-in Either type,
--   where the Left errors are collected.
data LogEither log a = Log [log] | Ok a deriving Show

instance Functor (LogEither log) where
    fmap :: (a -> b) -> LogEither log a -> LogEither log b
    fmap _ (Log errors) = Log errors
    fmap f (Ok result) = Ok $ f result

instance Applicative (LogEither log) where
    pure :: a -> LogEither log a
    pure = Ok

    (<*>) :: LogEither log (a -> b) -> LogEither log a -> LogEither log b
    -- Here, the errors are collected.
    -- But the Either type only collects the first error.
    Log errors <*> Log errors' = Log $ errors ++ errors'
    _ <*> Log errors = Log errors
    Log errors <*> _ = Log errors
    Ok f <*> Ok x = Ok $ f x

instance Monad (LogEither log) where
    return :: a -> LogEither log a
    return = pure

    (>>) :: LogEither log a -> LogEither log b -> LogEither log b
    (>>) = (*>)

    (>>=) :: LogEither log a -> (a -> LogEither log b) -> LogEither log b
    Log errors >>= _ = Log errors
    Ok result >>= f = f result

data CheckerState = CheckerState {
    -- Is it inside a function? If so, what type does this function returns?
    -- Nothing - in main program
    -- Just t  - in a function that returns type "t"
    functionContext :: Maybe Type,
    -- All functions' types of parameters and return value
    functionMapping :: String `M.Map` ([Type], Type),
    -- All variables' types
    mappingStack :: [String `M.Map` Type]
}

goDeeper :: CheckerState -> CheckerState
goDeeper state = state { mappingStack = M.empty : mappingStack state }

addMappingLayer :: [(String, Type)] -> CheckerState -> CheckerState
addMappingLayer mapping state =
    goDeeper state { mappingStack = M.fromList mapping : mappingStack state }

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

-- used for noncovariance type checking - array and pair
(<~) :: Type -> Type -> Bool
Pair(_, _) <~ NullType = True
Pair(a, b) <~ Pair(a', b') = a <~ a' && b <~ b'
Any <~ _ = True
_ <~ Any = True
a <~ b = a == b

-- | Can the right type take the place of the left type?
(<|) :: Type -> Type -> Bool
Any <| Any = False
Any <| _ = True
_ <| Any = True
String <| Array Char = True
String <| Array Any = True
Pair(_, _) <| NullType = True
Pair(a, b) <| Pair(a' , b') = a <~ a' && b <~ b'
Array a <| Array a' = a <~ a'
a <| b = a == b

isArray :: Type -> Bool
isArray (Array _) = True
isArray _ = False

isPair :: Type -> Bool
isPair (Pair(_, _)) = True
isPair _ = False

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
