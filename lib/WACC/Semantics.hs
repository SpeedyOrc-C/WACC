module WACC.Semantics where

import Text.Parser (Range)
import qualified WACC as W (Type(..))
import WACC (Expression(..), Statement, Function, Program)

import qualified Data.Map as M

data Type
    = TypeAny
    | TypeInt
    | TypeBool
    | TypeChar
    | TypeString
    | TypeArray Type
    | TypePair (Maybe (Type, Type))

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
newtype CheckerState = CheckerState [String `M.Map` Type]

class Compatible t where
    (<|) :: t -> t -> Bool

instance Compatible Type where
    (<|) :: Type -> Type -> Bool
    TypeAny <| _ = True
    _ <| TypeAny = True
    TypeInt <| TypeInt = True
    TypeBool <| TypeBool = True
    TypeChar <| TypeChar = True
    TypeString <| TypeString = True
    TypeString <| TypeArray TypeChar = True
    TypeArray a <| TypeArray b = a <| b
    TypePair (Just (a, b)) <| TypePair (Just (c, d)) = a <| c && b <| d
    (TypePair {}) <| (TypePair {}) = True
    _ <| _ = False

lookUpIdentifierType :: CheckerState -> String -> Maybe Type
lookUpIdentifierType (CheckerState []) _ = Nothing
lookUpIdentifierType (CheckerState (m:ms)) name = case M.lookup name m of
    Nothing -> lookUpIdentifierType (CheckerState ms) name
    Just t -> Just t

fromWaccType :: W.Type -> Type
fromWaccType = \case
    W.TypeInt {} -> TypeInt
    W.TypeBool {} -> TypeBool
    W.TypeChar {} -> TypeChar
    W.TypeString {} -> TypeString
    W.TypeArray t _ -> TypeArray (fromWaccType t)
    W.TypePair Nothing _ -> TypePair Nothing
    W.TypePair (Just (a, b)) _ -> TypePair (Just (fromWaccType a, fromWaccType b))

computeType :: CheckerState -> Expression -> Type
computeType state = \case
    LiteralInt {} -> TypeInt

class CheckSemantics a where
    check :: CheckerState -> a -> (CheckerState, [SemanticError])

instance CheckSemantics Expression where


instance CheckSemantics Statement where


instance CheckSemantics [Statement] where


instance CheckSemantics Function where


instance CheckSemantics Program where


