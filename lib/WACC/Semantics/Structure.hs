module WACC.Semantics.Structure where

import Data.Map (Map)

{-
All constructors no longer have a range, as they're only needed in
syntactic and semantic analysis.
-}

data Program = Program (Map String Function) [Statement]
    deriving Show

data Function = Function Type String [(Type, String)] [Statement]
    deriving Show

data Statement
    = Declare Type String Expression
    | Assign Expression Expression
    | Read Expression
    | Free Expression
    | Return Expression
    | Exit Expression
    | Print Expression
    | PrintLine Expression
    | If Expression [Statement] [Statement]
    | While Expression [Statement]
    | Scope [Statement]
    deriving Show

data Type
    = Any
    | Int
    | Bool
    | Char
    | String
    | Array Type
    | Pair (Type, Type)
    | Callable [Type] Type
    deriving (Show, Eq)

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

    | GreaterInt Expression Expression
    | GreaterEqual Expression Expression
    | LessInt Expression Expression
    | LessEqual Expression Expression

    | GreaterChar Expression Expression
    | GreaterEqualChar Expression Expression
    | LessChar Expression Expression
    | LessEqualChar Expression Expression

    | Equal Type Expression Expression
    | NotEqual Type Expression Expression

    | And Expression Expression
    | Or Expression Expression
    
    | FunctionCall String [Expression]
    deriving Show
