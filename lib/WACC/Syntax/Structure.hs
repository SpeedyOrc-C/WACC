module WACC.Syntax.Structure where

import Text.Parser ( Range )

data Program = Program ([Function], [Statement]) Range
    deriving Show

data Function = Function (Type, String, [(Type, String)], [Statement]) Range
    deriving Show

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

data Type
    = Int () Range
    | Bool () Range
    | Char () Range
    | String () Range
    | Array Type Range
    | Pair (Maybe (Type, Type)) Range
    deriving Show

type Unary = Expression
type Binary = (Expression, Expression)

data Expression
    = Identifier String Range
    | LiteralInt Int Range
    | LiteralBool Bool Range
    | LiteralChar Char Range
    | LiteralString String Range
    | LiteralArray [Expression] Range
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
