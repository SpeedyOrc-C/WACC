module WACC.Syntax.Error where

import WACC.Syntax.Structure (Name (..))


extendStatements :: String
extendStatements = "Or add a \";\" and extend the existing statements."

extendCondition :: String
extendCondition = "Or add other operators and extend the existing condition."

data WaccSyntaxErrorType
    = ExpectOperand
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
    | ExpectDo
    | ExpectWhileBody
    | ExpectDone
    | ExpectStatementAfterSemicolon
    | ExpectScopeBody
    | ExpectScopeEnd
    | PairTypeErased
    | PairTypeInPairTypeNotErased
    | UnknownType
    | ExpectIdentifierInDeclaration
    | InvalidLeftValue
    | InvalidRightValue
    | ExpectOneStatement
    | ExpectAssignEqualSign
    | ExpectDeclareEqualSign
    | ExpectOneExpression
    | IntegerOverflow Int
    | ExpectFunctionEnd
    | ExpectProgramBegin
    | ExpectProgramEnd
    | FunctionDoesNotReturn Name
    | UnexpectedCodeAfterProgramEnd
    deriving Eq

{- Makes WaccSyntaxErrorType an instance of type class Show to display helpful 
error messages. -}
instance Show WaccSyntaxErrorType where
    show :: WaccSyntaxErrorType -> String
    show ExpectOperand =
        "Expect an operand."
    show ExpectExpressionInBracket =
        "Expect an expression in bracket."
    show UnmatchedBracket =
        "Cannot find \")\"."
    show ExpectIndexInBracket =
        "Expect an index in bracket."
    show UnmatchedSquareBracket =
        "Unmatched square bracket in index."
    show MissingEscapedChar =
        "Missing escaped character."
    show (NonGraphicChar c) =
        "Non graphic character " ++ show c ++ " is not allowed."
    show (NonAsciiChar c)
        | isCJK = "除了 ASCII 字符以外的都不可以，所以汉字“" ++ [c] ++ "”也不行。"
        | isKana = "ASCII 文字以外はダメだから、仮名「" ++ [c] ++ "」もダメだよ。"
        | otherwise = "Non ASCII character “" ++ [c] ++ "” is not allowed."
        where
            isCJK = c >= '\x4E00' && c <= '\x9FFF'
            isKana = c >= '\x3040' && c <= '\x30FF'
    show UnmatchedSingleQuote =
        "Unmatched single quote in literal character."
    show UnmatchedDoubleQuote =
        "Unmatched double quote in literal string."
    show ExpectOneCharacter =
        "Expect one character in literal character."
    show ExpectConditionIf =
        "Expect condition after \"if\" keyword."
    show ExpectThen =
        "Expect \"then\" keyword.\n" ++ extendCondition
    show ExpectThenClause =
        "Expect \"then\" clause after condition."
    show ExpectElse =
        "Expect \"else\" keyword after \"then\" clause.\n" ++ extendStatements
    show ExpectElseClause =
        "Expect \"else\" clause after \"then\" clause."
    show ExpectFi =
        "Expect \"fi\" keyword after \"then\" clause.\n" ++ extendStatements
    show ExpectConditionWhile =
        "Expect condition after \"while\" keyword."
    show ExpectDo =
        "Expect \"do\" keyword.\n" ++ extendCondition
    show ExpectWhileBody =
        "Expect while's body."
    show ExpectDone =
        "Expect \"done\" keyword after while's body.\n" ++ extendStatements
    show ExpectStatementAfterSemicolon =
        "Expect a statement after semicolon."
    show ExpectScopeBody =
        "Expect scope's body."
    show ExpectScopeEnd =
        "Expect \"end\" keyword after scope's body.\n" ++ extendStatements
    show PairTypeErased =
        "Pair type here should have inner types."
    show PairTypeInPairTypeNotErased =
        "Pair type in pair type should be type-erased."
    show UnknownType =
        "Unknown type."
    show ExpectIdentifierInDeclaration =
        "Expect an identifier in declaration."
    show InvalidLeftValue =
        "Invalid left value."
    show InvalidRightValue =
        "Invalid right value."
    show ExpectOneStatement =
        "Expect a statement, declaration, assignment, \"if\", \"while\", or scope."
    show ExpectAssignEqualSign =
        "Expect \"=\" sign for assignment (or extend it with array indices)."
    show ExpectDeclareEqualSign =
        "Expect \"=\" sign for declaration."
    show ExpectOneExpression =
        "Expect one expression for assignment."
    show (IntegerOverflow i) =
        "Integer literal \"" ++ show i ++ "\" is too " ++
        (if i < intLowerBound then "small" else "big") ++ ". " ++
        "It should be between -2^31 and 2^31-1"
    show ExpectFunctionEnd =
        "Expect \"end\" keyword at the end of function.\n" ++ extendStatements
    show ExpectProgramBegin =
        "Expect \"begin\" keyword at the beginning of program."
    show ExpectProgramEnd =
        "Expect \"end\" keyword at the end of program.\n" ++ extendStatements
    show (FunctionDoesNotReturn (Name name _)) =
        "There is a path in function \"" ++ name ++ "\" that does not return"
    show UnexpectedCodeAfterProgramEnd =
        "There is unexpected code after \"end\" keyword."

{- The smallest allowed integer literal. -}
intLowerBound :: Int
intLowerBound = -(2::Int)^(31::Int)

{- The biggest allowed integer literal. -}
intUpperBound :: Int
intUpperBound = (2::Int)^(31::Int) - 1
