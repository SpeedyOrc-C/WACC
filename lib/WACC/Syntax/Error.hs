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
    | ExpectOneField String
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
    | ExpectIdentifierInStructInitialization
    | InvalidLeftValue
    | ExpectOneStatement
    | ExpectAssignEqualSign
    | ExpectDeclareEqualSign
    | ExpectOneExpression
    | IntegerOverflow Int
    | ExpectFunctionEnd
    | ExpectStructEnd
    | ExpectProgramBegin
    | ExpectProgramEnd
    | FunctionDoesNotReturn Name
    | UnexpectedCodeAfterProgramEnd
    | ExpectAWhite
    | FunctionCallNoCall
    | ConditionHasSideEffect
    | FunctionMissingType
    | FunctionMissingIs
    | MainTrailingFunctions
    | ExpectIdentifierInStruct
    | ExpectRightCurleyBracket
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
    show (ExpectOneField name) =
        "Expect a field in declare of structure " ++ name
    show UnmatchedSquareBracket =
        "Unmatched square bracket in index."
    show MissingEscapedChar =
        "expected end of escape sequence \nvalid escape sequences are \
        \\\0, \\n, \\t, \\b, \\f, \\r, \\\", \\' or \\\\"
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
        "expected end of character literal"
    show UnmatchedDoubleQuote =
        "Unmatched double quote in literal string."
    show ExpectOneCharacter =
        "Expect escape character or graphic character in literal character.\n\
        \\" and \' must be escaped in the string literal i.e. \\\" and \\\'"
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
    show ExpectIdentifierInStructInitialization =
        "Expect an identifier in struct initialization."
    show InvalidLeftValue =
        "Invalid left value."
    show ExpectOneStatement =
        "Expect a statement, declaration, assignment, \"if\", \"while\", or scope."
    show ExpectAssignEqualSign =
        "Expect \"=\" sign for assignment (or extend it with array indices)."
    show ExpectDeclareEqualSign =
        "Expect \"=\" sign for declaration."
    show ExpectOneExpression =
        "Expect one expression for assignment.\n\
        \Expressions may start with integer, string, character or \
        \boolean literals; identifiers; unary operators; null; or parentheses.\n\
        \In addition, expressions may contain array indexing operations; \
        \and comparison, logical, and arithmetic operators."
    show (IntegerOverflow i) =
        "Integer literal \"" ++ show i ++ "\" is too " ++
        (if i < intLowerBound then "small" else "big") ++ ". " ++
        "It should be between -2^31 and 2^31-1"
    show ExpectFunctionEnd =
        "Expect \"end\" keyword at the end of function.\n" ++ extendStatements
    show ExpectStructEnd =
        "Expect \"end\" keyword at the end of structure declare.\n"
    show ExpectProgramBegin =
        "Expect \"begin\" keyword at the beginning of program."
    show ExpectProgramEnd =
        "Expect \"end\" keyword at the end of program.\n" ++ extendStatements
    show (FunctionDoesNotReturn (Name name _)) =
        "There is a path in function \"" ++ name ++ "\" that does not return"
    show UnexpectedCodeAfterProgramEnd =
        "There is unexpected code after \"end\" keyword.\n\
        \All program body and \
        \function declarations must be within `begin` and `end`."
    show ExpectAWhite =
        "Expected a white character"
    show ConditionHasSideEffect =
        "Cannot call functions or create arrays in the condition."
    show FunctionMissingType =
        "Function does not specify its return type."
    show FunctionMissingIs =
        "Function needs \"is\" keyword before the body."
    show FunctionCallNoCall =
        "Expect `call` before using function.\n" ++
        "Function call may not appear in expressions."
    show MainTrailingFunctions =
        "Functions definitions should not be after the main program."
    show ExpectIdentifierInStruct =
        "Expect identifier after struct"
    show ExpectRightCurleyBracket =
        "Expect the right curley bracket at the end of the declaration of struct"

{- The smallest allowed integer literal. -}
intLowerBound :: Int
intLowerBound = -(2::Int)^(31::Int)

{- The biggest allowed integer literal. -}
intUpperBound :: Int
intUpperBound = (2::Int)^(31::Int) - 1
