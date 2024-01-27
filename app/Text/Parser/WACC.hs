module Text.Parser.WACC where

import Text.Parser
import Text.WACC

import Control.Monad ( void )
import Control.Applicative
    ( Alternative(many, empty, some, (<|>)), asum )
import Data.Functor ( (<&>) )

data WaccSyntaxErrorType
    = ExpectRightOperand String
    | ExpectExpressionInBracket
    | UnmatchedBracket
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

instance Show WaccSyntaxErrorType where
    show :: WaccSyntaxErrorType -> String
    show (ExpectRightOperand operator) =
        "Expect right operand for operator “" ++ operator ++ "”"
    show ExpectExpressionInBracket =
        "Expect expression in bracket"
    show UnmatchedBracket =
        "Unmatched bracket in expression"
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

expressionBase :: WaccParser Expression
expressionBase = asum [
    expressionWithBrackets,
    expressionIdentifier,
    expressionLiteralBool,
    expressionLiteralInt,
    expressionLiteralChar,
    expressionLiteralString
    ]

unaryOperator
    :: Parser error Expression
    -> ([(String, Expression -> Range -> Expression)],
        [(String, Expression -> Range -> Expression)])
    -> Parser error Expression
unaryOperator higherParser (wordOperators, symbolOperators) = do
        operators <- many $ asum $ map getRangeA $

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

        e <- higherParser

        let mergeUnaryExpression
                :: Expression
                -> (Range, Expression -> Range -> Expression)
                -> Expression
            mergeUnaryExpression x (range, constructor) =
                constructor x (fst range, snd $ expressionRange x)

        return $ foldl mergeUnaryExpression e operators

expressionUnaryOperation :: WaccParser Expression
expressionUnaryOperation = unaryOperator expressionBase ([
        ("len", Length),
        ("ord", Order),
        ("chr", Character)
    ],[
        ("!", Not),
        ("-", Negate)
    ])

binaryOperator ::
    WaccParser Expression
    -> [(String, (Expression, Expression) -> Range -> Expression)]
    -> WaccParser Expression
binaryOperator higherParser operators = Parser $ \input -> do
    first@(Parsed (from, _) leftExpression rest) <- parse higherParser input

    let nextParser = asum $ operators <&> \(symbol, constructor) -> do
            _ <- surroundManyWhites $ str symbol
            right <- higherParser `syntaxError` ExpectRightOperand symbol
            return (constructor, right)

    case parse (many nextParser) rest of
        Left x -> Left x
        Right (Parsed _ [] _) -> return first
        Right (Parsed (_, to) rightExpressions rest') -> do
            Right $ Parsed (from, to) mergedExpression rest'
            where
                mergedExpression =
                    foldl mergeBinaryExpression leftExpression rightExpressions

                mergeBinaryExpression x (constructor, y) = constructor (x, y)
                    (fst $ expressionRange x, snd $ expressionRange y)

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
    _ <- many white
    condition <- expression `syntaxError` ExpectConditionIf
    _ <- many white
    _ <- str "then"
    _ <- many white
    thenClause <- statements `syntaxError` ExpectThenClause
    _ <- many white
    _ <- str "else"
    _ <- many white
    elseClause <- statements `syntaxError` ExpectElseClause
    _ <- many white
    _ <- str "fi"
    return (condition, thenClause, elseClause)

statementWhile :: WaccParser Statement
statementWhile = While ~ do
    _ <- str "while"
    _ <- many white
    condition <- expression `syntaxError` ExpectConditionWhile
    _ <- many white
    _ <- str "do"
    _ <- many white
    body <- statements `syntaxError` ExpectWhileBody
    _ <- many white
    _ <- str "done"
    return (condition, body)

statement :: Parser WaccSyntaxErrorType Statement
statement = asum [
    statementSkip,
    statementRead,
    statementFree,
    statementExit,
    statementPrint,
    statementPrintLine,
    statementIf,
    statementWhile
    ]

statements :: Parser WaccSyntaxErrorType [Statement]
statements = (:) <$> statement <*> many (do
    _ <- surroundManyWhites (char ';')
    statement `syntaxError` ExpectStatementAfterSemicolon
    )
