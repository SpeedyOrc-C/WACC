module Text.Parser.WACC.SyntaxError where

data WaccSyntaxErrorType
    = ExpectRightOperand String
    | ExpectExpressionInBracket
    | UnmatchedBracket
    | ExpectIndexInBracket
    | UnmatchedSquareBracket
    | MissingEscapedChar
    | NonGraphicChar Char
    | NonAsciiChar Char
    | UnmatchedSingleQuote
    | ExpectOneCharacter
    | UnmatchedDoubleQuote
    | ExpectConditionIf
    | ExpectThen
    | ExpectThenClause
    | ExpectElse
    | ExpectElseClause
    | ExpectFi
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
    | IntegerOverflow Int
    | ExpectProgramBegin
    | ExpectProgramEnd
    | NoReturnInFunction

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
    show (NonGraphicChar c) =
        "Non graphic character " ++ show c ++ " is not allowed"
    show (NonAsciiChar c) =
        "Non ASCII character “" ++ [c] ++ "” is not allowed"
    show UnmatchedSingleQuote =
        "Unmatched single quote in literal character"
    show UnmatchedDoubleQuote =
        "Unmatched double quote in literal string"
    show ExpectOneCharacter =
        "Expect one character in literal character"
    show ExpectConditionIf =
        "Expect condition after “if” keyword"
    show ExpectThen =
        "Expect “then” keyword after condition"
    show ExpectThenClause =
        "Expect “then” clause after condition"
    show ExpectElse =
        "Expect “else” keyword after “then” clause"
    show ExpectElseClause =
        "Expect “else” clause after “then” clause"
    show ExpectFi =
        "Expect “fi” keyword after “then” clause"
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
    show (IntegerOverflow i) =
        "Integer literal “" ++ show i ++ "” is too " ++
        (if i < -(2::Int)^(31::Int) then "small" else "big") ++ ", " ++
        "and should be in range -2^31 and 2^31-1"
    show ExpectProgramBegin =
        "Expect “begin” keyword at the beginning of program"
    show ExpectProgramEnd =
        "Expect “end” keyword at the end of program"
    show NoReturnInFunction =
        "There is no return statement in function body"
