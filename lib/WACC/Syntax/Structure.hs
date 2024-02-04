module WACC.Syntax.Structure where

import Text.Parser ( Range )

{- Defines Program to include a list of functions and a list of statements. -}
data Program = Program ([Function], [Statement]) Range
    deriving Show

{- Defines Function to include a return type, a function name, a list of 
  parameters, and a list of statements. -}
data Function = Function (Type, Name, [(Name, Type)], [Statement]) Range
    deriving Show

{- Defines Name to include a name and a range. -}
data Name = Name String Range deriving (Show, Eq)

{- Defines Statement to be one of the forms. -}
data Statement
    = Skip () Range
    | Declare (Type, String, Expression) Range
    | Assign (Expression, Expression) Range
    | Read Expression Range
    | Free Expression Range
    | Return Expression Range
    | Exit Expression Range
    | Print Expression Range
    | PrintLine Expression Range
    | If (Expression, [Statement], [Statement]) Range
    | While (Expression, [Statement]) Range
    | Scope [Statement] Range
    deriving Show

{- Defines Type to be one of the forms. -}
data Type
    = Int () Range
    | Bool () Range
    | Char () Range
    | String () Range
    | Array Type Range
    | Pair (Maybe (Type, Type)) Range
    deriving (Show, Eq)

{- Defines Unary to be a single expression. -}
type Unary = Expression

{- Defines Binary to be a tuple of two expressions. -}
type Binary = (Expression, Expression)

{- Defines Expression to be one of the forms. -}
data Expression
    = Identifier String Range
    | LiteralInt Int Range
    | LiteralBool Bool Range
    | LiteralChar Char Range
    | LiteralString String Range
    | LiteralArray [Expression] Range
    -- newpair(e1, e2)
    | LiteralPair (Expression, Expression) Range
    | LiteralPairNull () Range
    | ArrayElement (Expression, Expression) Range
    | Not Unary Range
    | Negate Unary Range
    | Length Unary Range
    | Order Unary Range
    | Character Unary Range
    | PairFirst Unary Range
    | PairSecond Unary Range
    | Multiply Binary Range
    | Divide Binary Range
    | Remainder Binary Range
    | Add Binary Range
    | Subtract Binary Range
    | Greater Binary Range
    | GreaterEqual Binary Range
    | Less Binary Range
    | LessEqual Binary Range
    | Equal Binary Range
    | NotEqual Binary Range
    | And Binary Range
    | Or Binary Range
    | FunctionCall (String, [Expression]) Range
    deriving Show
