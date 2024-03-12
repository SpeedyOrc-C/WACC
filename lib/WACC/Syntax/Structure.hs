module WACC.Syntax.Structure where

import Text.Parser ( Range )

data Program = Program ([Structure], [Function], [Statement]) Range
    deriving Show

{- Defines Function to include a return type, a function name, a list of
  parameters, and a list of statements. -}
data Function = Function (Type, Name, [(Name, Type)], [Statement]) Range
    deriving Show

data Structure = Structure (Name, [(Name, Type)]) Range
    deriving Show

data Name = Name String Range deriving (Show, Eq)

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
    | RefType Type Range
    | Struct String Range
    deriving (Show, Eq)

-- Unary operators have one operand.
type Unary = Expression

-- Binary operators have two operands.
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
    | Field (Expression, Name) Range
    | NewStruct [Expression] Range
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
