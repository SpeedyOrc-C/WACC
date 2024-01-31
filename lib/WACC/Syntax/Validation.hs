module WACC.Syntax.Validation where

import Text.Parser ( Range )
import WACC.Syntax.Structure ( Expression(..), Type(..), Statement(..) )

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

statementRange :: Statement -> Range
statementRange stmt = case stmt of
    Skip _ r -> r
    Declare _ r -> r
    Assign _ r -> r
    Read _ r -> r
    Free _ r -> r
    Return _ r -> r
    Exit _ r -> r
    Print _ r -> r
    PrintLine _ r -> r
    If _ r -> r
    While _ r -> r
    Scope _ r -> r

isLeftValue :: Expression -> Bool
isLeftValue = \case
    Identifier _ _ -> True
    a@ArrayElement {} -> isArrayElement a
    PairFirst e _ -> isLeftValue e
    PairSecond e _ -> isLeftValue e
    _ -> False

isPairElement :: Expression -> Bool
isPairElement = \case
    PairFirst e _ -> isLeftValue e
    PairSecond e _ -> isLeftValue e
    _ -> False

isArrayElement :: Expression -> Bool
isArrayElement = \case
    Identifier {} -> True
    ArrayElement (a, i) _ -> isArrayElement a && isExpression i
    _ -> False

isExpression :: Expression -> Bool
isExpression = \case
    LiteralInt {} -> True
    LiteralBool {} -> True
    LiteralChar {} -> True
    LiteralString {} -> True
    LiteralPairNull {} -> True
    Identifier {} -> True
    a@ArrayElement {} -> isArrayElement a
    Not e _ -> isExpression e
    Negate e _ -> isExpression e
    Length e _ -> isExpression e
    Order e _ -> isExpression e
    Character e _ -> isExpression e
    Multiply (a, b) _ -> isExpression a && isExpression b
    Divide (a, b) _ -> isExpression a && isExpression b
    Remainder (a, b) _ -> isExpression a && isExpression b
    Add (a, b) _ -> isExpression a && isExpression b
    Subtract (a, b) _ -> isExpression a && isExpression b
    Greater (a, b) _ -> isExpression a && isExpression b
    GreaterEqual (a, b) _ -> isExpression a && isExpression b
    Less (a, b) _ -> isExpression a && isExpression b
    LessEqual (a, b) _ -> isExpression a && isExpression b
    Equal (a, b) _ -> isExpression a && isExpression b
    NotEqual (a, b) _ -> isExpression a && isExpression b
    And (a, b) _ -> isExpression a && isExpression b
    Or (a, b) _ -> isExpression a && isExpression b
    _ -> False

isLiteralArray :: Expression -> Bool
isLiteralArray = \case
    LiteralArray a _ -> all isExpression a
    _ -> False

isLiteralPair :: Expression -> Bool
isLiteralPair = \case
    LiteralPair (a, b) _ -> isExpression a && isExpression b
    _ -> False

isFunctionCall :: Expression -> Bool
isFunctionCall = \case
    FunctionCall (_, a) _ -> all isExpression a
    _ -> False

isRightValue :: Expression -> Bool
isRightValue e = or [
    isExpression e,
    isLiteralArray e,
    isLiteralPair e,
    isPairElement e,
    isFunctionCall e
    ]

isType :: Type -> Bool
isType t = isTypeBase t || isTypeArray t || isTypePair t

isTypeBase :: Type -> Bool
isTypeBase = \case
    Int {} -> True
    Bool {} -> True
    Char {} -> True
    String {} -> True
    _ -> False

isTypeArray :: Type -> Bool
isTypeArray = \case
    Array t _ -> isType t
    _ -> False

isTypePair :: Type -> Bool
isTypePair = \case
    Pair (Just (a, b)) _ -> isTypePairElement a && isTypePairElement b
    _ -> False

isTypePairElement :: Type -> Bool
isTypePairElement = \case
    Pair Nothing _ -> True
    t -> isTypeBase t || isTypeArray t

willReturn :: Statement -> Bool
willReturn (Return {}) = True
willReturn (Exit {}) = True
willReturn (If (_, t, e) _) = any willReturn t && any willReturn e
willReturn (Scope s _) = any willReturn s
willReturn _ = False

