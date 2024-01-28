module Text.Parser.WACC where

import Text.Parser
import Text.WACC

import Control.Monad ( void )
import Control.Applicative
    ( Alternative(many, empty, some, (<|>)), asum, optional )
import Data.Functor ( (<&>) )
import Data.Maybe (fromJust)

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
    show ExpectOneStatement =
        "Expect one statement"
    show ExpectOneExpression =
        "Expect one expression"


type WaccParser a = Parser WaccSyntaxErrorType a

addRange :: (info -> Range -> node) -> Parser error info -> Parser error node
addRange constructor parser = Parser $ \input -> do
    Parsed range result rest <- parse parser input
    Right (Parsed range (constructor result range) rest)

(~) :: (info -> Range -> node) -> WaccParser info -> WaccParser node
(~) = addRange

white :: Parser error ()
white = void $ asum $ char <$> [' ', '\t', '\r', '\n']

surroundManyWhites :: Parser error a -> Parser error a
surroundManyWhites = (`surroundedBy` many white)

linebreak :: Parser error ()
linebreak = void (char '\n') <|> void (str "\r\n")

digits :: [Char]
digits = ['0'..'9']

identifierTailChars :: [Char]
identifierTailChars = '_' : ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

identifierString :: Parser error String
identifierString = do
    c <- charThat (`elem` headChars)
    s <- many (charThat (`elem` identifierTailChars))
    let name = c:s
    if name `elem` ["len", "ord", "chr",
                    "begin", "end", "is",
                    "if", "then", "else", "fi",
                    "while", "do", "done",
                    "skip", "read", "free", "return", "exit", "print", "println"]
        then empty else return name
    where
    headChars = '_' : ['A'..'Z'] ++ ['a'..'z']

charEscaped :: WaccParser Char
charEscaped = (
        ('\'' <$ char '\'')
    <|> ('\"' <$ char '\"')
    <|> ('\\' <$ char '\\')
    <|> ('\r' <$ char 'r')
    <|> ('\n' <$ char 'n')
    <|> ('\t' <$ char 't')
    <|> ('\b' <$ char 'b')
    <|> ('\f' <$ char 'f')
    <|> ('\0' <$ char '0')
    ) `syntaxError` MissingEscapedChar

graphicChar :: WaccParser Char
graphicChar = charThat $ \c ->
    lower <= c && c <= upper
    where
    (lower, upper) = (' ', '~')

charInner :: WaccParser Char
charInner = do
    c <- graphicChar `syntaxError` NonGraphicChar
    case c of
        '\'' -> empty
        '\"' -> empty
        '\\' -> charEscaped
        _ -> return c

-- Expression

expressionIdentifier :: WaccParser Expression
expressionIdentifier = Identifier ~ identifierString

expressionLiteralBool :: WaccParser Expression
expressionLiteralBool = LiteralBool ~
    ((True <$ str "true") <|> (False <$ str "false"))

expressionLiteralInt :: WaccParser Expression
expressionLiteralInt = LiteralInt ~ (read <$> some (charThat (`elem` digits)))

expressionLiteralChar :: WaccParser Expression
expressionLiteralChar = LiteralChar ~ do
    _ <- char '\''
    c <- charInner `syntaxError` ExpectOneCharacter
    _ <- char '\'' `syntaxError` UnmatchedSingleQuote
    return c

expressionLiteralString :: WaccParser Expression
expressionLiteralString = LiteralString ~ do
    _ <- char '"'
    s <- many charInner
    _ <- char '"' `syntaxError` UnmatchedDoubleQuote
    return s

expressionLiteralArray :: WaccParser Expression
expressionLiteralArray = LiteralArray ~ do
    _ <- char '['
    _ <- many white
    es <- optional $ expression `separatedBy` surroundManyWhites (char ',')
    _ <- many white
    _ <- char ']'
    return $ if null es then [] else fromJust es

expressionLiteralPair :: WaccParser Expression
expressionLiteralPair = LiteralPair ~ do
    _ <- str "newpair"
    _ <- surroundManyWhites $ char '('
    a <- expression
    _ <- surroundManyWhites (char ',')
    b <- expression
    _ <- many white
    _ <- char ')'
    return (a, b)

expressionLiteralPairNull :: WaccParser Expression
expressionLiteralPairNull = LiteralPairNull ~ void (str "null")

expressionFunctionCall :: WaccParser Expression
expressionFunctionCall = FunctionCall ~ do
    _        <- str "call"
    name     <- surroundManyWhites identifierString
    _        <- char '('
    paraList <- optional $ surroundManyWhites $
                expression `separatedBy` surroundManyWhites (char ',')
    _        <- char ')'
    return (name, if null paraList then [] else fromJust paraList)


expressionBase :: WaccParser Expression
expressionBase = asum [
    expressionWithBrackets,
    expressionIdentifier,
    expressionLiteralBool,
    expressionLiteralInt,
    expressionLiteralChar,
    expressionLiteralString,
    expressionLiteralPairNull
    -- expressionLiteralArray
    ]

expressionPairElement :: WaccParser Expression
expressionPairElement = asum
    [PairFirst  ~ do
        _ <- str "fst"
        _ <- many white
        leftValue
    ,PairSecond ~ do
        _ <- str "snd"
        _ <- many white
        leftValue
    ]

indexOperator :: WaccParser Expression -> WaccParser Expression
indexOperator higherParser = do
    ((from, _), array) <- getRangeA higherParser
    indices <- many $ do
        _ <- many white
        _ <- char '['
        index <- getRangeA $ surroundManyWhites expression `syntaxError` ExpectIndexInBracket
        _ <- char ']' `syntaxError` UnmatchedSquareBracket
        return index

    let mergeArrayElement x ((_, to), y) = ArrayElement (x, y) (from, to)

    return $ foldl mergeArrayElement array indices

expressionArrayElement :: WaccParser Expression
expressionArrayElement = indexOperator expressionBase

unaryOperator
    :: Parser error Expression
    -> ([(String, Expression -> Range -> Expression)],
        [(String, Expression -> Range -> Expression)])
    -> Parser error Expression
unaryOperator higherParser (wordOperators, symbolOperators) = do
        operators <- many . asum . map getRangeA $

            let wordOperatorsParsers = wordOperators <&>
                    \(operator, constructor) -> constructor <$
                        str operator
                            <* follows (`notElem` identifierTailChars)
                            <* many white

                symbolOperatorParsers = symbolOperators <&>
                    \(operator, constructor) -> constructor <$
                        str operator
                            <* many white

            in wordOperatorsParsers ++ symbolOperatorParsers

        ((_, to), e) <- getRangeA higherParser

        let mergeUnaryExpression x (range, constructor) =
                constructor x (fst range, to)

        return $ foldl mergeUnaryExpression e operators

expressionUnaryOperation :: WaccParser Expression
expressionUnaryOperation = unaryOperator expressionArrayElement ([
        ("len", Length),
        ("ord", Order),
        ("chr", Character)
        -- ("fst", PairFirst),
        -- ("snd", PairSecond)
    ],[
        ("!", Not),
        ("-", Negate)
    ])

binaryOperator ::
    WaccParser Expression
    -> [(String, (Expression, Expression) -> Range -> Expression)]
    -> WaccParser Expression
binaryOperator higherParser operators = do
    ((from, _), e) <- getRangeA higherParser

    es <- many . asum . map getRangeA $ operators <&>
        \(symbol, constructor) -> do
            _ <- surroundManyWhites $ str symbol
            right <- higherParser `syntaxError` ExpectRightOperand symbol
            return (constructor, right)

    let mergeBinaryExpression x ((_, to), (constructor, y)) =
            constructor (x, y) (from, to)

    return $ foldl mergeBinaryExpression e es

expressionBinaryOperation :: WaccParser Expression
expressionBinaryOperation = foldl binaryOperator expressionUnaryOperation [
    [("*", Multiply) ,("/", Divide) ,("%", Remainder)],
    [("+", Add) ,("-", Subtract)],
    [("<=", LessEqual) ,("<", Less) ,(">=", GreaterEqual) ,(">", Greater)],
    [("==", Equal) ,("!=", NotEqual)],
    [("&&", And)],
    [("||", Or)]
    ]

expressionWithBrackets :: WaccParser Expression
expressionWithBrackets = do
    _ <- char '('
    e <- surroundManyWhites expressionBinaryOperation
        `syntaxError` ExpectExpressionInBracket
    _ <- char ')' `syntaxError` UnmatchedBracket

    return e

expression :: WaccParser Expression
expression = expressionBinaryOperation

rightValue :: WaccParser Expression
rightValue = asum [
    expression,
    expressionLiteralArray,
    expressionLiteralPair,
    expressionFunctionCall,
    expressionPairElement
    ]

-- Statement

statementSkip :: WaccParser Statement
statementSkip = Skip ~ void (str "skip")

commandLikeStatement
    :: (Expression -> Range -> node)
    -> String -> WaccParser node
commandLikeStatement constructor command =
    constructor ~
        (str command *> follows (`notElem` identifierTailChars) *> many white
            *> expression)

statementRead :: WaccParser Statement
statementRead = commandLikeStatement Read "read"

statementFree :: WaccParser Statement
statementFree = commandLikeStatement Free "free"

statementExit :: WaccParser Statement
statementExit = commandLikeStatement Exit "exit"

statementPrint :: WaccParser Statement
statementPrint = commandLikeStatement Print "print"

statementPrintLine :: WaccParser Statement
statementPrintLine = commandLikeStatement PrintLine "println"

statementIf :: WaccParser Statement
statementIf = If ~ do
    _ <- str "if"
    condition <- surroundManyWhites expression `syntaxError` ExpectConditionIf
    _ <- str "then"
    thenClause <- surroundManyWhites statements `syntaxError` ExpectThenClause
    _ <- str "else"
    elseClause <- surroundManyWhites statements `syntaxError` ExpectElseClause
    _ <- str "fi"
    return (condition, thenClause, elseClause)

statementWhile :: WaccParser Statement
statementWhile = While ~ do
    _ <- str "while"
    condition <- surroundManyWhites expression `syntaxError` ExpectConditionWhile
    _ <- str "do"
    body <- surroundManyWhites statements `syntaxError` ExpectWhileBody
    _ <- str "done"
    return (condition, body)

statementScope :: WaccParser Statement
statementScope = Scope ~ do
    _ <- str "begin"
    body <- surroundManyWhites statements `syntaxError` ExpectScopeBody
    _ <- str "end"
    return body

typeInt :: WaccParser Type
typeInt = TypeInt ~ void (str "int")

typeBool :: WaccParser Type
typeBool = TypeBool ~ void (str "bool")

typeChar :: WaccParser Type
typeChar = TypeChar ~ void (str "char")

typeString :: WaccParser Type
typeString = TypeString ~ void (str "string")

pairElementType :: WaccParser Type
pairElementType = asum [
    typeInt,
    typeBool,
    typeChar,
    typeString,
    typePairNothing `followsParser` (
        (many white *> (char ',' <|> char ')'))
            `syntaxError` UnexpectTypesInInnerPairType)
    ] `syntaxError` UnknownType

typePairNothing :: WaccParser Type
typePairNothing = TypePair ~ (Nothing <$ str "pair")

typePair :: WaccParser Type
typePair = TypePair ~ do
    _ <- str "pair"
    _ <- many white
    _ <- char '(' `syntaxError` ExpectTypesInOutermostPairType
    a <- pairElementType
    _ <- surroundManyWhites (char ',')
    b <- pairElementType
    _ <- many white
    _ <- char ')'
    return $ Just (a, b)

baseType :: WaccParser Type
baseType = asum [
    typeInt,
    typeBool,
    typeChar,
    typeString,
    typePair
    ]

typeArray :: WaccParser Type
typeArray = do
    ((from, _), t) <- getRangeA baseType
    brackets <- many $ getRangeA $ void $
        surroundManyWhites (char '[') >> char ']'

    let mergeArray x ((_, to), _) = TypeArray x (from, to)

    return $ foldl mergeArray t brackets

type' :: WaccParser Type
type' = typeArray

leftValue :: WaccParser Expression
leftValue = expression `that` isLeftValue

statementDeclare :: WaccParser Statement
statementDeclare = Declare ~ do
    t <- type'
    _ <- some white
    name <- identifierString
    _ <- surroundManyWhites $ char '='
    value <- rightValue `syntaxError` ExpectOneExpression
    return (t, name, value)

statementAssign :: WaccParser Statement
statementAssign = Assign ~ do
    left <- leftValue
    _ <- surroundManyWhites $ char '='
    right <- rightValue `syntaxError` ExpectOneExpression
    return (left, right)

statement :: Parser WaccSyntaxErrorType Statement
statement = asum [
    statementSkip,
    statementRead,
    statementFree,
    statementExit,
    statementPrint,
    statementPrintLine,
    statementIf,
    statementWhile,
    statementScope,
    statementDeclare,
    statementAssign
    ] `syntaxError` ExpectOneStatement

statements :: Parser WaccSyntaxErrorType [Statement]
statements = (:) <$> statement <*> many (do
    _ <- surroundManyWhites (char ';')
    statement `syntaxError` ExpectStatementAfterSemicolon
    )
