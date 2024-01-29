module WACC.Semantics where

import Text.Parser (Range)
import WACC (Type (..), Expression, Statement, Function, Program)

import qualified Data.Map as M

data WaccSemanticsErrorType
    = UndefinedVariable String
    | UndefinedFunction String
    | InvalidExpression Expression
    | IncompatibleAssignment (Type, Type)
    | InvalidExit Type
    | InvalidReturn Type
    | InvalidPrint Type
    | InvalidPrintLine Type
    | InvalidFree Type
    | InvalidRead Type
    | InvalidCondition Type

data SemanticError = SemanticError Range WaccSemanticsErrorType
data IdentifierType
newtype CheckerState = CheckerState [M.Map String Type]

getType :: CheckerState -> String -> Maybe Type
getType (CheckerState []) _ = Nothing
getType (CheckerState (m:ms)) name = case M.lookup name m of
    Nothing -> getType (CheckerState ms) name
    Just t -> Just t

class Compatible t where
    (<|) :: t -> t -> Bool 

instance Compatible Type where
    (<|) :: Type -> Type -> Bool
    (TypeInt {}) <| (TypeInt {}) = True
    (TypeBool {}) <| (TypeBool {}) = True
    (TypeChar {}) <| (TypeChar {}) = True
    (TypeString {}) <| (TypeString {}) = True
    (TypeString {}) <| (TypeArray (TypeChar {}) _) = True
    (TypeArray a _) <| (TypeArray b _) = a <| b
    (TypePair (Just (a, b)) _) <| (TypePair (Just (c, d)) _) = a <| c && b <| d
    (TypePair {}) <| (TypePair {}) = True
    _ <| _ = False

class CheckSemantics a where
    check :: CheckerState -> a -> (CheckerState, [SemanticError])

instance CheckSemantics Expression where


instance CheckSemantics Statement where


instance CheckSemantics [Statement] where


instance CheckSemantics Function where


instance CheckSemantics Program where


