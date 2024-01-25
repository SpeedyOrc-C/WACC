module Text.Parser.WACC where

import Text.Parser
import Text.WACC

import Control.Monad ( void )
import Control.Applicative
    ( Alternative(many, empty, some, (<|>)), asum )
import Data.Functor ( (<&>) )
import Data.Char ( ord )

data WaccSyntaxErrorType
    = MissingRightOperand String
    | MissingExpressionInBracket
    | UnmatchedBracket
    | MissingEscapedChar
    | NonAsciiChar
    | UnmatchedSingleQuote
    | ExpectOneCharacter
    | UnmatchedDoubleQuote

instance Show WaccSyntaxErrorType where
    show (MissingRightOperand operator) =
        "Missing right operand for operator “" ++ operator ++ "”"
    show MissingExpressionInBracket =
        "Missing expression in bracket"
    show UnmatchedBracket =
        "Unmatched bracket in expression"
    show MissingEscapedChar =
        "Missing escaped charater"
    show NonAsciiChar =
        "Non ASCII character is not allowed"
    show UnmatchedSingleQuote =
        "Unmatched single quote in literal character"
    show UnmatchedDoubleQuote =
        "Unmatched double quote in literal string"
    show ExpectOneCharacter =
        "Expect one character in literal character"


type WaccParser a = Parser WaccSyntaxErrorType a

addRange :: (info -> Range -> node) -> Parser error info -> Parser error node
addRange constructor parser = Parser $ \input -> do
    Parsed range result rest <- parse parser input
    Right (Parsed range (constructor result range) rest)

(~) :: (info -> Range -> node) -> Parser error info -> Parser error node
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
    if name `elem` ["len", "ord", "chr"]
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

charAscii :: WaccParser Char
charAscii = charThat (\c -> 0 <= ord c && ord c <= 127)

charInner :: WaccParser Char
charInner = do
    c <- charAscii `syntaxError` NonAsciiChar
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

expressionBase :: WaccParser Expression
expressionBase =
        expressionWithBrackets
    <|> expressionIdentifier
    <|> expressionLiteralBool
    <|> expressionLiteralInt
    <|> expressionLiteralChar

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
            right <- higherParser `syntaxError` MissingRightOperand symbol
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
        `syntaxError` MissingExpressionInBracket
    _ <- char ')' `syntaxError` UnmatchedBracket

    return e

-- Statement

statementSkip :: Parser WaccSyntaxErrorType Statement
statementSkip = Skip ~ void (str "skip")

statement :: Parser WaccSyntaxErrorType Statement
statement = statementSkip

statements :: Parser WaccSyntaxErrorType [Statement]
statements = statement `separatedBy` surroundManyWhites (char ';')
