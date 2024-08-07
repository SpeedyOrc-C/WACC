{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module WACC.Semantics.Structure where

import Data.Set (Set)
import Data.List (intercalate)

{-
All constructors no longer have a range, as they're only needed in
syntactic and semantic analysis.
-}

data Program = Program [Structure] (Set Function) Block
    deriving Show

data Function = Function Type String [(String, Type)] Block
    deriving (Show, Eq)

data Structure = Structure String [(String, Type)]
    deriving (Show, Eq)

instance Ord Structure where
    Structure name1 _ `compare` Structure name2 _ =
        name1 `compare` name2

instance Ord Function where
    Function _ name1 _ _ `compare` Function _ name2 _ _ =
        name1 `compare` name2

newtype Block = Block [Statement] deriving (Show, Eq)

data Statement
    = Declare Type String Expression
    | Assign Type Expression Expression
    | Read Type Expression
    | Free Type Expression
    | Return Type Expression
    | Exit Expression
    | Print Type Expression
    | PrintLine Type Expression
    | If Expression Block Block
    | While Expression Block
    | Scope Block
    deriving (Show, Eq)

(?>) :: Type -> Type -> Type
x ?> Any = x
Any ?> x = x
Pair (t1, t2) ?> Pair (t1', t2') = Pair (t1 ?> t1', t2 ?> t2')
x ?> _ = x
(Struct a x) ?> (Struct _ y) = Struct a (zipWith (?>) x y)
t1 ?> t2
    | t1 == t2  = t1
    | otherwise = error ("Cannot unify different types." ++ show t1 ++ show t2)

alignAny :: Type -> Expression -> Expression
alignAny (Array t) (LiteralArray t' exps)
    = LiteralArray (t ?> t') exps
alignAny (Pair (t1', t2')) (LiteralPair (t1, t2) exp')
    = LiteralPair (t1 ?> t1', t2 ?> t2') exp'
alignAny t (PairFirst _ e) = PairFirst t e
alignAny t (PairSecond _ e) = PairSecond t e
alignAny _ y = y

data Type
    = Any
    | Int
    | Bool
    | Char
    | String
    | Array Type
    | Pair (Type, Type)
    | Struct String [Type]
    | RefType Type
    deriving Eq

instance Show Type where
    show = \case
        Any -> "unknown"
        Int -> "int"
        Bool -> "bool"
        Char -> "char"
        String -> "string"
        Array t -> show t ++ "[]"
        Pair (a, b) -> "pair(" ++ show a ++ ", " ++ show b ++ ")"
        Struct str ts -> str ++ "(" ++ intercalate ", " (show <$> ts) ++ ")"
        RefType t -> show t ++ "&"

data ComparisonType = CompareChar | CompareInt deriving (Show, Eq)

data Expression
    = Identifier Type String
    | LiteralInt Int
    | LiteralBool Bool
    | LiteralChar Char
    | LiteralString String
    | LiteralArray Type [Expression]
    | LiteralPair (Type, Type) (Expression, Expression)
    | LiteralPairNull

    | NewStruct [Expression]
    | Field (Type, String) Expression String
    | ArrayElement Type Expression Expression

    | Not Expression
    | Negate Expression
    | Length Expression
    | Order Expression
    | Character Expression
    | PairFirst Type Expression
    | PairSecond Type Expression
    | Multiply Expression Expression
    | Divide Expression Expression
    | Remainder Expression Expression
    | Add Expression Expression
    | Subtract Expression Expression

    | Greater ComparisonType Expression Expression
    | GreaterEqual ComparisonType Expression Expression
    | Less ComparisonType Expression Expression
    | LessEqual ComparisonType Expression Expression

    | Equal Type Expression Expression
    | NotEqual Type Expression Expression

    | And Expression Expression
    | Or Expression Expression

    | FunctionCall Type String [(Type, (Type, Expression))]
    deriving (Show, Eq)

canBeRefType :: Expression -> Bool
canBeRefType (Identifier _ _) = True
canBeRefType (ArrayElement {}) = True
canBeRefType (Field {}) = True
canBeRefType (PairFirst {}) = True
canBeRefType (PairSecond {}) = True
canBeRefType _ = False

isIdent :: Expression -> Bool
isIdent (Identifier _ _) = True
isIdent _ = False

identType :: Expression -> Type
identType (Identifier t _) = t
identType _ = error "should not use ident type when it is not an ident"

isRefType :: Type -> Bool
isRefType (RefType _) = True
isRefType _ = False

isStruct :: Type -> Bool
isStruct (Struct _ _) = True
isStruct _ = False