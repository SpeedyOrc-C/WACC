module Text.WACC where

import Text.Parser

data Program = Program ([Function], [Statement]) Range
    deriving Show

data Function = Function (Type, String, [Parameter], [Statement]) Range
    deriving Show

data Parameter = Parameter (Type, String) Range
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
    = TypeInt () Range
    | TypeBool () Range
    | TypeChar () Range
    | TypeString () Range
    | TypeArray Type Range
    | TypePair (Maybe (Type, Type)) Range
    deriving Show

type Unary = Expression
type Binary = (Expression, Expression)

data Expression
    = LiteralInt Int Range
    | LiteralBool Bool Range
    | LiteralChar Char Range
    | LiteralString String Range
    | LiteralArray [Expression] Range
    | LiteralPair (Expression, Expression) Range
    | LiteralPairNull () Range
    | Identifier String Range
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

expressionRange :: Expression -> Range
expressionRange expr = case expr of
    LiteralInt _ r -> r
    LiteralBool _ r -> r
    LiteralChar _ r -> r
    LiteralString _ r -> r
    LiteralArray _ r -> r
    LiteralPair _ r -> r
    LiteralPairNull _ r -> r
    Identifier _ r -> r
    ArrayElement _ r -> r
    Not _ r -> r
    Negate _ r -> r
    Length _ r -> r
    Order _ r -> r
    Character _ r -> r
    PairFirst _ r -> r
    PairSecond _ r -> r
    Multiply _ r -> r
    Divide _ r -> r
    Remainder _ r -> r
    Add _ r -> r
    Subtract _ r -> r
    Greater _ r -> r
    GreaterEqual _ r -> r
    Less _ r -> r
    LessEqual _ r -> r
    Equal _ r -> r
    NotEqual _ r -> r
    And _ r -> r
    Or _ r -> r
    FunctionCall _ r -> r

isLeftValue :: Expression -> Bool
isLeftValue = \case
    Identifier _ _ -> True
    ArrayElement (a, _) _ -> isArrayElement a
    PairFirst e _ -> isLeftValue e
    PairSecond e _ -> isLeftValue e
    _ -> False 

isArrayElement :: Expression -> Bool
isArrayElement = \case
    Identifier {} -> True
    ArrayElement (a, _) _ -> isArrayElement a
    _ -> False

isRightValue :: Expression -> Bool
isRightValue = undefined
