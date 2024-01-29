module Text.Parser.WACC where

import Text.WACC
import Text.Parser
import Text.Parser.WACC.SyntaxError

import Control.Monad
import Control.Applicative
import Data.Functor
import Data.Maybe

type WaccParser a = Parser WaccSyntaxErrorType a

addRange :: (info -> Range -> node) -> Parser error info -> Parser error node
addRange constructor parser = Parser $ \input -> do
    Parsed range result rest <- parse parser input
    Right (Parsed range (constructor result range) rest)

(~) :: (info -> Range -> node) -> WaccParser info -> WaccParser node
(~) = addRange

nonLineBreak :: Parser error ()
nonLineBreak = void $ charThat (`notElem` ['\r', '\n'])

white :: Parser error ()
white = void (asum $ char <$> [' ', '\t', '\r', '\n']) <|> void comment

comment :: Parser error ()
comment = void $ do
    _ <- char '#'
    many nonLineBreak

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
    if name `elem` ["len", "ord", "chr", "call",
                    "begin", "end", "is",
                    "if", "then", "else", "fi",
                    "while", "do", "done",
                    "skip", "read", "free", "return", "exit", "print", "println",
                    "int", "string", "pair", "char", "null", "newpair"]
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

charInner :: WaccParser Char
charInner = do
    c <- (one `syntaxErrorWhen` (NonAsciiChar, (> '~')))
            `syntaxErrorWhen` (NonGraphicChar, (< ' '))
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

expressionNoSignLiteralInt :: WaccParser Expression
expressionNoSignLiteralInt = LiteralInt ~ do
    read <$> some (charThat (`elem` digits))

expressionLiteralInt :: WaccParser Expression
expressionLiteralInt = LiteralInt ~ do
    sign <- optional (char '-' <|> char '+')
    let negator (Just '-') x = -x
        negator _ x = x
    (negator sign . read <$> some (charThat (`elem` digits)))
        `syntaxErrorWhen`
            (IntegerOverflow, \x -> x < -twoToThe31 || twoToThe31 - 1 < x)
    where
    twoToThe31 = (2::Int) ^ (31::Int)

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
    expressionLiteralPairNull,
    expressionLiteralPair,
    expressionLiteralArray,
    expressionFunctionCall
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
                    \(operator, constructor) ->
                        constructor <$
                            case operator of
                                "!" -> str operator <* many white
                                "-" -> (str operator <* many white)
                                    `notFollowedBy` expressionNoSignLiteralInt
                                _ -> error "unreachable"

            in wordOperatorsParsers ++ symbolOperatorParsers

        ((_, to), e) <- getRangeA higherParser

        let mergeUnaryExpression x ((from, _), constructor) =
                constructor x (from, to)

        return $ foldl mergeUnaryExpression e (reverse operators)

expressionUnaryOperation :: WaccParser Expression
expressionUnaryOperation =
    unaryOperator expressionArrayElement ([
        ("len", Length),
        ("ord", Order),
        ("chr", Character),
        ("fst", PairFirst),
        ("snd", PairSecond)
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

leftValue :: WaccParser Expression
leftValue = (expression `that` isLeftValue) `syntaxError` InvalidLeftValue

rightValue :: WaccParser Expression
rightValue = (expression `that` isRightValue) `syntaxError` InvalidRightValue

strictExpression :: WaccParser Expression
strictExpression = expression `that` isExpression

-- Statement

statementSkip :: WaccParser Statement
statementSkip = Skip ~ void (str "skip")

command :: String -> WaccParser node -> WaccParser node
command keyword value = do
    _ <- str keyword
    _ <- follows (`notElem` identifierTailChars)
    _ <- many white
    value

statementRead :: WaccParser Statement
statementRead = Read ~ command "read" leftValue

statementFree :: WaccParser Statement
statementFree = Free ~ command "free" strictExpression

statementExit :: WaccParser Statement
statementExit = Exit ~ command "exit" strictExpression

statementPrint :: WaccParser Statement
statementPrint = Print ~ command "print" strictExpression

statementPrintLine :: WaccParser Statement
statementPrintLine = PrintLine ~ command "println" strictExpression

statementReturn :: WaccParser Statement
statementReturn = Return ~ command "return" strictExpression

statementIf :: WaccParser Statement
statementIf = If ~ do
    _ <- str "if"
    condition <- surroundManyWhites expression `syntaxError` ExpectConditionIf
    _ <- str "then" `syntaxError` ExpectThen
    thenClause <- surroundManyWhites statements `syntaxError` ExpectThenClause
    _ <- str "else" `syntaxError` ExpectElse
    elseClause <- surroundManyWhites statements `syntaxError` ExpectElseClause
    _ <- str "fi" `syntaxError` ExpectFi
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

-- `syntaxError` UnexpectTypesInInnerPairType
-- `syntaxError` UnknownType
-- `syntaxError` ExpectTypesInOutermostPairType

typePair :: WaccParser Type
typePair = TypePair ~ do
    _ <- str "pair"
    optional $ do
        _ <- many white
        _ <- char '(' 
        a <- typeArray
        _ <- surroundManyWhites (char ',')
        b <- typeArray
        _ <- many white
        _ <- char ')'
        return (a, b)

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
type' = typeArray `that` isType

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

statement :: WaccParser Statement
statement = asum [
    statementSkip,
    statementRead,
    statementFree,
    statementExit,
    statementPrint,
    statementPrintLine,
    statementReturn,
    statementIf,
    statementWhile,
    statementScope,
    statementDeclare,
    statementAssign
    ] `syntaxError` ExpectOneStatement

statements :: WaccParser [Statement]
statements = (:) <$> statement <*> many (do
    _ <- surroundManyWhites (char ';')
    statement `syntaxError` ExpectStatementAfterSemicolon
    )

parameter :: WaccParser Parameter
parameter = Parameter ~ do
    t <- type'
    _ <- some white
    name <- identifierString
    return (t, name)

function :: WaccParser Function
function = Function ~ do
    t <- type'
    _ <- some white
    name <- identifierString
    _ <- many white
    _ <- char '('
    parameters <- optional $ surroundManyWhites $
        parameter `separatedBy` surroundManyWhites (char ',')
    _ <- char ')'
    _ <- surroundManyWhites $ str "is"
    body <- statements
        `syntaxErrorWhen` (const NoReturnInFunction, not . willReturn . last)
    _ <- many white
    _ <- str "end"
    return (t, name, if null parameters then [] else fromJust parameters, body)

program :: WaccParser Program
program = strict $ Program ~ do
    _ <- surroundManyWhites (str "begin" `syntaxError` ExpectProgramBegin)
    functions <- optional $ function `separatedBy` many white
    _ <- many white
    body <- statements
    _ <- surroundManyWhites (str "end" `syntaxError` ExpectProgramEnd)
    return (if null functions then [] else fromJust functions, body)
