module WACC.Semantics.Structure where

import Data.Set (Set)

{-
All constructors no longer have a range, as they're only needed in
syntactic and semantic analysis.
-}

data Program = Program (Set Function) [Statement]
    deriving Show

data Function = Function Type String [(String, Type)] [Statement]
    deriving (Show, Eq)

instance Ord Function where
    Function _ name1 _ _ `compare` Function _ name2 _ _ =
        name1 `compare` name2

data Statement
    = Declare Type String Expression
    | Assign Expression Expression
    | Read Expression
    | Free Expression
    | Return Expression
    | Exit Expression
    | Print Type Expression
    | PrintLine Type Expression
    | If Expression [Statement] [Statement]
    | While Expression [Statement]
    | Scope [Statement]
    deriving (Show, Eq)

data Type
    = Any
    | Int
    | Bool
    | Char
    | String
    | Array Type
    | Pair (Type, Type)
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

data ComparisonType = CompareChar | CompareInt deriving (Show, Eq)

data Expression
    = Identifier String
    | LiteralInt Int
    | LiteralBool Bool
    | LiteralChar Char
    | LiteralString String
    | LiteralArray Type [Expression]
    | LiteralPair (Type, Type) (Expression, Expression)
    | LiteralPairNull

    | ArrayElement Expression Expression

    | Not Expression
    | Negate Expression
    | Length Expression
    | Order Expression
    | Character Expression
    | PairFirst Expression
    | PairSecond Expression
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
    
    | FunctionCall String [Expression]
    deriving (Show, Eq)
