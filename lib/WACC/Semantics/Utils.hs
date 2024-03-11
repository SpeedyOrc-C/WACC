module WACC.Semantics.Utils where

import qualified Data.Map as M

import qualified WACC.Syntax.Structure as Syntax
import Text.Parser (Range)
import WACC.Semantics.Error (WaccSemanticsErrorType (UndefinedStructure))
import WACC.Semantics.Structure (Type (..), Structure)

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
    -- All variables' types and where they are declared
    mappingStack :: [String `M.Map` (Range, Type)],
    structures :: String `M.Map` Structure
}

addScope :: CheckerState -> CheckerState
addScope state = state { mappingStack = M.empty : mappingStack state }

addIdentifier :: String -> (Range, Type) -> CheckerState -> CheckerState
addIdentifier name t state = state {
    mappingStack =
        M.insert name t (head (mappingStack state)) :
        tail (mappingStack state)
}

lookUp :: CheckerState -> String -> Maybe (Range, Type)
lookUp state name = case mappingStack state of
    [] -> Nothing
    mapping : rest -> case M.lookup name mapping of
        Just t -> Just t
        Nothing -> lookUp (state {mappingStack = rest}) name

lookUpInnermost :: CheckerState -> String -> Maybe (Range, Type)
lookUpInnermost state =
    lookUp $ state { mappingStack = [head (mappingStack state)] }

-- used for noncovariance type checking - array and pair
(<~) :: Type -> Type -> Bool
Pair(a, b) <~ Pair(a', b') = a <~ a' && b <~ b'
Any <~ _                   = True
_ <~ Any                   = True
a <~ b                     = a == b

-- | Can the right type take the place of the left type?
(<|) :: Type -> Type -> Bool
Any <| Any                  = False
Any <| _                    = True
_ <| Any                    = True
String <| Array Char        = True
String <| Array Any         = True
Pair(a, b) <| Pair(a' , b') = a <~ a' && b <~ b'
Array a <| Array a'         = a <~ a'
a <| b                      = a == b

(<?) :: Type -> Type -> Bool
Array a <? Array b         = a <| b
Pair(a, b) <? Pair(a', b') = a <| a' && b <| b'
_ <? _                     = False

isArray :: Type -> Bool
isArray (Array _) = True
isArray _         = False

isLiter :: Syntax.Expression -> Bool
isLiter (Syntax.LiteralArray _ _) = True
isLiter (Syntax.LiteralPair _ _)  = True
isLiter _                         = False

isPair :: Type -> Bool
isPair (Pair(_, _)) = True
isPair _            = False

fromSyntaxType :: CheckerState -> Syntax.Type -> LogEither SemanticError Type
fromSyntaxType state = \case
    Syntax.Int {}               -> Ok Int
    Syntax.Bool {}              -> Ok Bool
    Syntax.Char {}              -> Ok Char
    Syntax.String {}            -> Ok String
    Syntax.Array t _            -> do 
        t' <- fromSyntaxType state t
        Ok (Array t')
    Syntax.Pair Nothing _       -> Ok (Pair (Any, Any))
    Syntax.Pair (Just (a, b)) _ -> do
        a' <- fromSyntaxType state a
        b' <- fromSyntaxType state b
        Ok (Pair (a', b'))
    Syntax.Struct str range -> do
        let structures' = structures state
        case M.lookup str structures' of
            Nothing -> Log [SemanticError range (UndefinedStructure str)]
            _ -> return (Struct str)

-- | Given that two types are compatible with each other,
--   find the common type between two types. This can find the type of an array.
common :: Type -> Type -> Type
common Any t = t
common t Any = t

common String (Array Char) = String
common (Array Char) String = String

common (Array Any) String = String
common String (Array Any) = String

common (Array a) (Array b)         = Array (common a b)
common (Pair (a, b)) (Pair (c, d)) = Pair (common a c, common b d)

common a b = if a == b then a else undefined
