module WACC.Syntax.Parser where

import GHC.Generics (Associativity(..))

import Control.Monad
import Control.Applicative
import Data.Functor
import Data.Maybe

import Text.Parser
import WACC.Syntax.Structure
import WACC.Syntax.Error
import WACC.Syntax.Validation

{- Define a SyntaxParser which can identify a SyntaxError 
   within the range of our `WaccSyntaxErrorType`. -}
type WaccParser a = Parser WaccSyntaxErrorType a

{- It takes a constructor function, a parser, and returns a new parser. 
   This parser modifies the result of parsing by applying the constructor
   to include a range information. -}
addRange :: (info -> Range -> node) -> Parser error info -> Parser error node
addRange constructor parser = Parser $ \input -> do
    Parsed range result rest <- parse parser input
    Right (Parsed range (constructor result range) rest)

{- An abbreviated symbol for addRange. -}
(~) :: (info -> Range -> node) -> WaccParser info -> WaccParser node
(~) = addRange

{- It consumes a single character as long as 
   it is not a newline or a carriage return. -}
nonLineBreak :: Parser error ()
nonLineBreak = void $ charThat (`notElem` ['\r', '\n'])

{- The white parser tries to parse either the whitespace characters 
   (space, tab, carriage return, or newline) or the comment parser below. -}
white :: Parser error ()
white = void (asum $ char <$> [' ', '\t', '\r', '\n']) <|> comment

{- The comment parser firstly parse `#` and ignore the result, 
   then it uses `nonLineBreak` parser to handle the rest of this line. -}
comment :: Parser error ()
comment = void $ char '#' *> many nonLineBreak

{- This function takes a parser as parameter and generates a new parser by
   using `surroundedBy`, which parses an object surrounded by two surrounders
   (the surrounders here is many white). It can handle a statement surrounded
   by arbitary number of `white`s. -}
surroundManyWhites :: Parser error a -> Parser error a
surroundManyWhites = (`surroundedBy` many white)

{- This is a parser which handles two different linebreaks. -}
linebreak :: Parser error ()
linebreak = void (char '\n') <|> void (str "\r\n")

{- It is a list of all digit chars. -}
digits :: [Char]
digits = ['0'..'9']

{- It is a list of all valid chars for the `tail` of an identifier. -}
identifierTailChars :: [Char]
identifierTailChars = '_' : ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

{- This parser checks if the identifier is valid and not a reserved keyword.
   If it is invalid or it is a keyword, then it fails using empty. 
   Otherwise, it returns the parsed identifier. -}
identifierString :: Parser error String
identifierString = do
    s <- (:)
        <$> charThat (`elem` headChars)
        <*> many (charThat (`elem` identifierTailChars))
    
    if s `elem` keywords then empty else return s

    where
    headChars = '_' : ['A'..'Z'] ++ ['a'..'z']
    keywords = ["len", "ord", "chr", "call", "true", "false",
                "begin", "end", "is",
                "if", "then", "else", "fi",
                "while", "do", "done",
                "skip", "read", "free", "return", "exit", "print", "println",
                "int", "string", "pair", "char", "null", "newpair"]

{- It is a parser that handles escaped characters in character literals 
   and returns the corresponding Char. -}
charEscaped :: WaccParser Char
charEscaped = (
        ('\'' <$ char '\'')
    <|> ('\"' <$ char '\"')
    <|> ('\\' <$ char '\\')
    <|> ('\r' <$ char 'r' )
    <|> ('\n' <$ char 'n' )
    <|> ('\t' <$ char 't' )
    <|> ('\b' <$ char 'b' )
    <|> ('\f' <$ char 'f' )
    <|> ('\0' <$ char '0' )
    ) `syntaxError` MissingEscapedChar

{- It is a parser that parses the first char of input stream.
   It checks if the parsed character is greater than '~' or less than ' ',
   if so, it will detect this syntax error and fail. Otherwise, it will handle
   escaped chars or return this char. -}
charInner :: WaccParser Char
charInner = do
    c <- (one
        `syntaxErrorWhen`
            ((> '~'), \((from, _), x) -> SyntaxError from (NonAsciiChar x))
        ) `syntaxErrorWhen`
            ((< ' '), \((from, _), x) -> SyntaxError from (NonGraphicChar x))
    case c of
        '\'' -> empty
        '\"' -> empty
        '\\' -> charEscaped
        _ -> return c

-- Expression

{- This parser parses an identifier according to its range. -}
expressionIdentifier :: WaccParser Expression
expressionIdentifier = Identifier ~ identifierString

{- This parser parses a bool literal according to its range. -}
expressionLiteralBool :: WaccParser Expression
expressionLiteralBool = LiteralBool ~
    ((True <$ str "true") <|> (False <$ str "false"))

{- This parser parses an int literal without sign according to its range. -}
expressionNoSignLiteralInt :: WaccParser Expression
expressionNoSignLiteralInt = LiteralInt ~ do
    read <$> some (charThat (`elem` digits))

{- This parser parses an int literal with sign according to its range. -}
expressionLiteralInt :: WaccParser Expression
expressionLiteralInt = LiteralInt ~ do
    sign <- optional (char '-' <|> char '+')
    let negator (Just '-') x = -x
        negator _ x = x
    (negator sign . read <$> some (charThat (`elem` digits)))
        `syntaxErrorWhen` (
            \x -> x < intLowerBound || intUpperBound < x,
            \((from, _), x) -> SyntaxError from (IntegerOverflow x)
            )

{- This parser parses a char literal according to its range. -}
expressionLiteralChar :: WaccParser Expression
expressionLiteralChar = LiteralChar ~ do
    _ <- char '\''
    c <- charInner `syntaxError` ExpectOneCharacter
    _ <- char '\'' `syntaxError` UnmatchedSingleQuote
    return c

{- This parser parses a string literal according to its range. -}
expressionLiteralString :: WaccParser Expression
expressionLiteralString = LiteralString ~ do
    _ <- char '"'
    s <- many charInner
    _ <- char '"' `syntaxError` UnmatchedDoubleQuote
    return s

{- This parser parses an array literal according to its range. -}
expressionLiteralArray :: WaccParser Expression
expressionLiteralArray = LiteralArray ~ do
    _  <- char '['
    _  <- many white
    es <- optional $ expression `separatedBy` surroundManyWhites (char ',')
    _  <- many white
    _  <- char ']'
    return $ if null es then [] else fromJust es

{- This parser parses a pair literal according to its range. -}
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

{- This parser is used for a null pair. -}
expressionLiteralPairNull :: WaccParser Expression
expressionLiteralPairNull = LiteralPairNull ~ void (str "null")

{- This parser parses a function call, it returns the function name
   and a list of parameters. -}
expressionFunctionCall :: WaccParser Expression
expressionFunctionCall = FunctionCall ~ do
    _    <- str "call"
    f    <- surroundManyWhites identifierString
    _    <- char '('
    args <- optional $ surroundManyWhites $
                expression `separatedBy` surroundManyWhites (char ',')
    _    <- char ')'
    return (f, if null args then [] else fromJust args)

{- It is a parser that parses expressions. It tries each one type in order
   until one of them succeeds. If none of them succeeds, it fails. -}
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
        _     <- many white
        _     <- char '['
        index <- getRangeA $ surroundManyWhites expression `syntaxError`
                    ExpectIndexInBracket
        _     <- char ']' `syntaxError` UnmatchedSquareBracket
        return index

    let mergeArrayElement x ((_, to), y) = ArrayElement (x, y) (from, to)

    return $ foldl mergeArrayElement array indices

expressionArrayElement :: WaccParser Expression
expressionArrayElement = indexOperator expressionBase

unaryOperator
    :: WaccParser Expression
    -> ([(String, Expression -> Range -> Expression)],
        [(String, Expression -> Range -> Expression)])
    -> WaccParser Expression
unaryOperator higherParser (wordOperators, symbolOperators) = do
    let wordOperatorsParsers = wordOperators <&>
            \(operator, constructor) -> (operator, constructor) <$
                str operator
                    <* follows (`notElem` identifierTailChars)
                    <* many white

        symbolOperatorParsers = symbolOperators <&>
            \(operator, constructor) -> (operator, constructor) <$
                    case operator of
                        "!" -> str operator <* many white
                        "-" -> (str operator <* many white)
                                `notFollowedBy` expressionNoSignLiteralInt
                        _ -> error "unreachable"

        constructorParsers = wordOperatorsParsers ++ symbolOperatorParsers

    constructors <- many . asum . map getRangeA $ constructorParsers
    let operatorStrings = fst . snd <$> constructors

    ((_, to), e) <- if null operatorStrings
        then getRangeA higherParser
        else getRangeA higherParser `syntaxError` ExpectOperand

    let mergeUnaryExpression x ((from, _), (_, constructor)) =
            constructor x (from, to)

    return $ foldl mergeUnaryExpression e (reverse constructors)

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
    -> (Associativity, [(String, (Expression, Expression) -> Range -> Expression)])
    -> WaccParser Expression
binaryOperator higherParser (associativity, operators) = do
    firstExpression <- getRangeA higherParser

    rightExpressionsAndConstructors <-
        many . asum $ operators <&> \(symbol, constructor) -> do
            _ <- surroundManyWhites $ getRangeA $ str symbol
            (rightRange, right) <- getRangeA $ higherParser
                                    `syntaxError` ExpectOperand
            return (constructor, (rightRange, right))

    case associativity of
        LeftAssociative -> do
            let merge ((from, _), x) (constructor, ((_, to), y)) =
                    ((from, to), constructor (x, y) (from, to))

            return $ snd $
                foldl merge firstExpression rightExpressionsAndConstructors

        RightAssociative -> do
            let (constructors, rights) = unzip rightExpressionsAndConstructors
            let expressions = firstExpression : rights
            let leftExpressionsAndConstructors = zip expressions constructors
            let lastExpression = last expressions

            let merge (((from, _), x), constructor) ((_, to), y) =
                    ((from, to), constructor (x, y) (from, to))

            return $ snd $
                foldr merge lastExpression leftExpressionsAndConstructors

        NotAssociative -> error "WACC does not have this kind of operator!"

expressionBinaryOperation :: WaccParser Expression
expressionBinaryOperation = foldl binaryOperator expressionUnaryOperation [
    (LeftAssociative,
        [("*", Multiply), ("/", Divide), ("%", Remainder)]),
    (LeftAssociative,
        [("+", Add), ("-", Subtract)]),
    (LeftAssociative,
        [("<=", LessEqual), ("<", Less), (">=", GreaterEqual), (">", Greater)]),
    (LeftAssociative,
        [("==", Equal), ("!=", NotEqual)]),
    (RightAssociative,
        [("&&", And)]),
    (RightAssociative,
        [("||", Or)])
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
    _         <- str "while"
    _         <- some white
    condition <- expression `syntaxError` ExpectConditionWhile
    _         <- some white
    _         <- str "do"
    _         <- some white
    body      <- statements `syntaxError` ExpectWhileBody
    _         <- some white
    _         <- str "done"
    return (condition, body)

statementScope :: WaccParser Statement
statementScope = Scope ~ do
    _    <- str "begin"
    body <- surroundManyWhites statements `syntaxError` ExpectScopeBody
    _    <- str "end"
    return body

typeInt :: WaccParser Type
typeInt = Int ~ void (str "int")

typeBool :: WaccParser Type
typeBool = Bool ~ void (str "bool")

typeChar :: WaccParser Type
typeChar = Char ~ void (str "char")

typeString :: WaccParser Type
typeString = String ~ void (str "string")

-- `syntaxError` UnexpectTypesInInnerPairType
-- `syntaxError` UnknownType
-- `syntaxError` ExpectTypesInOutermostPairType

typePair :: WaccParser Type
typePair = Pair ~ do
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

    let mergeArray x ((_, to), _) = Array x (from, to)

    return $ foldl mergeArray t brackets

type' :: WaccParser Type
type' = typeArray `syntaxErrorWhen` (not . isType, \(_, t) -> findError t)
    where
    findError = \case
        Array (Pair Nothing (from, _)) _ ->
            SyntaxError from PairTypeErased
        Pair (Just (Pair (Just {}) (from, _), _)) _ ->
            SyntaxError from PairTypeInPairTypeNotErased
        Pair (Just (_, Pair (Just {}) (from, _))) _ ->
            SyntaxError from PairTypeInPairTypeNotErased
        _ -> error "unreachable"

statementDeclare :: WaccParser Statement
statementDeclare = Declare ~ do
    t     <- type'
    _     <- some white
    n     <- identifierString `syntaxError` ExpectIdentifierInDeclaration
    _     <- surroundManyWhites $ char '=' `syntaxError` ExpectDeclareEqualSign
    value <- rightValue `syntaxError` ExpectOneExpression
    return (t, n, value)

statementAssign :: WaccParser Statement
statementAssign = Assign ~ do
    left  <- leftValue
    _     <- surroundManyWhites $ char '=' `syntaxError` ExpectAssignEqualSign
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

statementSep :: Parser error ()
statementSep = void $ surroundManyWhites (char ';')

statements :: WaccParser [Statement]
statements = statement `separatedBy` statementSep

name :: WaccParser Name
name = Name ~ identifierString

parameter :: WaccParser (Name, Type)
parameter = do
    t <- type'
    _ <- some white
    p <- name
    return (p, t)

statementMustNotReturn :: WaccParser Statement
statementMustNotReturn = statement `that` (not . willReturn)

statementMustReturn :: WaccParser Statement
statementMustReturn = statement `that` willReturn

function :: WaccParser Function
function = Function ~ do
    t          <- type'
    _          <- some white
    n          <- name
    _          <- many white
    _          <- char '('
    parameters <- optional $ surroundManyWhites $
                    parameter `separatedBy` surroundManyWhites (char ',')
    _          <- char ')'
    _          <- surroundManyWhites $ str "is"
    body       <- statements `syntaxErrorWhen` (
                    not . willReturn . last,
                    \(_, body) ->
                        SyntaxError (fst (statementRange (last body)))
                        (FunctionDoesNotReturn n)
                    )
    _          <- many white
    _          <- str "end"
    return (t, n, if null parameters then [] else fromJust parameters, body)

program' :: WaccParser ([Function], [Statement])
program' = do
    _         <- surroundManyWhites (str "begin" `syntaxError` ExpectProgramBegin)
    functions <- optional $ function `separatedBy` many white
    _         <- many white
    body      <- statements
    _         <- surroundManyWhites (str "end" `syntaxError` ExpectProgramEnd)

    return (if null functions then [] else fromJust functions, body)

program :: WaccParser Program
program = Program ~ Parser (\input -> do
    case parse program' input of
        (Right (Parsed (_, to) _ (_, _:_))) ->
            Left $ Just (SyntaxError to UnexpectedCodeAfterProgramEnd)
        x -> x
    )