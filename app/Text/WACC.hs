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

type UnaryOperator = Expression -> Expression
type BinaryOperator = Expression -> Expression -> Expression

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
    | Not Expression Range
    | Negate Expression Range
    | Length Expression Range
    | Order Expression Range
    | Character Expression Range
    | PairFirst Expression Range
    | PairSecond Expression Range
    | Multiply (Expression, Expression) Range
    | Divide (Expression, Expression) Range
    | Remainder (Expression, Expression) Range
    | Add (Expression, Expression) Range
    | Subtract (Expression, Expression) Range
    | Greater (Expression, Expression) Range
    | GreaterEqual (Expression, Expression) Range
    | Less (Expression, Expression) Range
    | LessEqual (Expression, Expression) Range
    | Equal (Expression, Expression) Range
    | NotEqual (Expression, Expression) Range
    | And (Expression, Expression) Range
    | Or (Expression, Expression) Range
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
isLeftValue expr = case expr of
    Identifier _ _ -> True
    ArrayElement _ _ -> True
    PairFirst _ _ -> True
    PairSecond _ _ -> True
    _ -> False 
