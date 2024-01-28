module Text.Parser.WACC.SyntaxError where

data WaccSyntaxErrorType
    = ExpectRightOperand String
    | ExpectExpressionInBracket
    | UnmatchedBracket
    | ExpectIndexInBracket
    | UnmatchedSquareBracket
    | MissingEscapedChar
    | NonGraphicChar
    | UnmatchedSingleQuote
    | ExpectOneCharacter
    | UnmatchedDoubleQuote
    | ExpectConditionIf
    | ExpectThenClause
    | ExpectElseClause
    | ExpectConditionWhile
    | ExpectWhileBody
    | ExpectStatementAfterSemicolon
    | ExpectScopeBody
    | ExpectTypesInOutermostPairType
    | UnexpectTypesInInnerPairType
    | UnknownType
    | InvalidLeftValue
    | InvalidRightValue
    | ExpectOneStatement
    | ExpectOneExpression

instance Show WaccSyntaxErrorType where
    show :: WaccSyntaxErrorType -> String
    show (ExpectRightOperand operator) =
        "Expect right operand for operator “" ++ operator ++ "”"
    show ExpectExpressionInBracket =
        "Expect an expression in bracket"
    show UnmatchedBracket =
        "Unmatched bracket in expression"
    show ExpectIndexInBracket =
        "Expect an index in bracket"
    show UnmatchedSquareBracket =
        "Unmatched square bracket in index"
    show MissingEscapedChar =
        "Missing escaped charater"
    show NonGraphicChar =
        "Non graphic character is not allowed"
    show UnmatchedSingleQuote =
        "Unmatched single quote in literal character"
    show UnmatchedDoubleQuote =
        "Unmatched double quote in literal string"
    show ExpectOneCharacter =
        "Expect one character in literal character"
    show ExpectConditionIf =
        "Expect condition after “if” keyword"
    show ExpectThenClause =
        "Expect 'then' clause after condition"
    show ExpectElseClause =
        "Expect 'else' clause after “then” clause"
    show ExpectConditionWhile =
        "Expect condition after “while” keyword"
    show ExpectWhileBody =
        "Expect body after “while” condition"
    show ExpectStatementAfterSemicolon =
        "Expect a statement after semicolon"
    show ExpectScopeBody =
        "Expect body after “begin” keyword"
    show ExpectTypesInOutermostPairType =
        "Expect types in outermost pair type"
    show UnexpectTypesInInnerPairType =
        "Unexpected types in inner pair type"
    show UnknownType =
        "Unknown type"
    show InvalidLeftValue =
        "Invalid left value"
    show InvalidRightValue =
        "Invalid right value"
    show ExpectOneStatement =
        "Expect one statement"
    show ExpectOneExpression =
        "Expect one expression"
