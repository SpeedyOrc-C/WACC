module WACC.Syntax.Parser where

import Prelude hiding (error)
import qualified Prelude as P
import GHC.Generics (Associativity(..))

import Control.Monad
import Control.Monad.State.Lazy
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
addRange constructor parser = do
    (range, result) <- getRangeA parser
    return $ constructor result range

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

{- It takes a parser as parameter to parse the array expression.
   Then, it iteratively parses indices inside square brackets. -}
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

{- It is a parser which parse an array element. -}
expressionArrayElement :: WaccParser Expression
expressionArrayElement = indexOperator expressionBase

{- It is a function which takes a parser and two lists of operators.
   The operators must be unary operator. It then form a new parser
   which can parse expressions with unary operators. -}
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
                        _ -> P.error "unreachable"

        constructorParsers = wordOperatorsParsers ++ symbolOperatorParsers

    constructors <- many . asum . map getRangeA $ constructorParsers
    let operatorStrings = fst . snd <$> constructors

    ((_, to), e) <- if null operatorStrings
        then getRangeA higherParser
        else getRangeA higherParser `syntaxError` ExpectOperand

    let mergeUnaryExpression x ((from, _), (_, constructor)) =
            constructor x (from, to)

    return $ foldl mergeUnaryExpression e (reverse constructors)

{- It is a parser which parses expressions with unary operators. -}
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
        ("-", Negate),
        ("*", Dereference),
        ("&", Address)
    ]) `identifierWithBracket` FunctionCallNoCall

{- It is a function which takes a parser and two lists of operators.
   The operators must be binary operator. It then form a new parser
   which can parse expressions with binary operators. -}
binaryOperator ::
    WaccParser Expression
    -> (Associativity, [(String, (Expression, Expression)
    -> Range -> Expression)])
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

        NotAssociative -> P.error "WACC does not have this kind of operator!"

{- It is a parser which parses expressions with binary operators. -}
expressionBinaryOperation :: WaccParser Expression
expressionBinaryOperation = foldl binaryOperator expressionUnaryOperation [
    (LeftAssociative,
        [("*", Multiply), ("/", Divide), ("%", Remainder)]),
    (LeftAssociative,
        [("+", Add), ("-", Subtract)]),
    (LeftAssociative,
        [("<=", LessEqual), ("<", Less), (">=", GreaterEqual),
         (">", Greater)]),
    (LeftAssociative,
        [("==", Equal), ("!=", NotEqual)]),
    (RightAssociative,
        [("&&", And)]),
    (RightAssociative,
        [("||", Or)])
    ]

{- It is a parser which parses expressions with brackets. -}
expressionWithBrackets :: WaccParser Expression
expressionWithBrackets = do
    _ <- char '('
    e <- surroundManyWhites expressionBinaryOperation
        `syntaxError` ExpectExpressionInBracket
    _ <- char ')' `syntaxError` UnmatchedBracket

    return e
 {- It is a parser which parses expressions with binary operations. -}
expression :: WaccParser Expression
expression = expressionBinaryOperation

{- It is a function which can detect if an identifier is a function call
   or not. -}
identifierWithBracket :: WaccParser Expression
    -> WaccSyntaxErrorType
    -> WaccParser Expression
identifierWithBracket parser error = Parser $ \input ->
    case parse parser input of
        result@(Right (Parsed _ (Identifier _ _) rest)) ->
            case parse (many white *> char '(') rest of
                (Right _) -> Left $ Just $
                    SyntaxError (inputPosition input) error
                _ -> result
        x -> x

{- It is a parser which parses a left value. -}
leftValue :: WaccParser Expression
leftValue = expression `that` isLeftValue

{- It is a parser which parses a right value and
   raises a "ExpectOneExpression" if there is no expression. -}
rightValue :: WaccParser Expression
rightValue = expression `that` isRightValue `syntaxError` ExpectOneExpression
            `identifierWithBracket` FunctionCallNoCall


{- It is a parser which strictly parses the expression, which means the
   expression must be parsed completely. -}
strictExpression :: WaccParser Expression
strictExpression = expression `that` isExpression `syntaxError` ExpectOneExpression

-- Statement

{- It is a parser which parses skip statements. -}
statementSkip :: WaccParser Statement
statementSkip = Skip ~ void (str "skip")

{- It is a parser which parses a keyword and parses remaining content
   by the parser passed as an argument. -}
command :: String -> WaccParser node -> WaccParser node
command keyword value = do
    _ <- str keyword
    _ <- follows (`notElem` identifierTailChars)
    _ <- many white
    value

{- It is a parser which parses read statements. -}
statementRead :: WaccParser Statement
statementRead = Read ~ command "read"
                (leftValue `syntaxError` InvalidLeftValue)

{- It is a parser which parses free statements. -}
statementFree :: WaccParser Statement
statementFree = Free ~ command "free" strictExpression

{- It is a parser which parses exit statements. -}
statementExit :: WaccParser Statement
statementExit = Exit ~ command "exit" strictExpression

{- It is a parser which parses print statements. -}
statementPrint :: WaccParser Statement
statementPrint = Print ~ command "print" strictExpression

{- It is a parser which parses println statements. -}
statementPrintLine :: WaccParser Statement
statementPrintLine = PrintLine ~ command "println" strictExpression

{- It is a parser which parses return statements. -}
statementReturn :: WaccParser Statement
statementReturn = Return ~ command "return" strictExpression

{- It is a parser which parses conditional statements. -}
expressionCondition :: WaccParser Expression
expressionCondition = expression `syntaxErrorWhen` (not . isExpression,
    \((from, _), _) -> SyntaxError from ConditionHasSideEffect
    )

{- It is a parser which parses a if statement and return the condition,
   contents in 'then' clause and contents in 'else' clause. -}
statementIf :: WaccParser Statement
statementIf = If ~ do
    _ <- str "if"
    _ <- some white
    condition <- expressionCondition `syntaxError` ExpectConditionIf
    _ <- many white
    _ <- str "then" `syntaxError` ExpectThen
    _ <- some white
    thenClause <- statements `syntaxError` ExpectThenClause
    _ <- many white
    _ <- str "else" `syntaxError` ExpectElse
    _ <- some white
    elseClause <- statements `syntaxError` ExpectElseClause
    _ <- many white
    _ <- str "fi" `syntaxError` ExpectFi
    return (condition, thenClause, elseClause)

{- It is a parser which parses a while statement and return the condition
   and the body of the while loop. -}
statementWhile :: WaccParser Statement
statementWhile = While ~ do
    _         <- str "while"
    _         <- some white
    condition <- expressionCondition `syntaxError` ExpectConditionWhile
    _         <- many white
    _         <- str "do" `syntaxError` ExpectDo
    _         <- some white
    body      <- statements `syntaxError` ExpectWhileBody
    _         <- many white
    _         <- str "done" `syntaxError` ExpectDone
    return (condition, body)

{- It is a parser which parses statements started by 'begin' and
   return the content between the 'begin' statement and the 'end' statement. -}
statementScope :: WaccParser Statement
statementScope = Scope ~ do
    _    <- str "begin"
    _    <- some white
    body <- statements `syntaxError` ExpectScopeBody
    _    <- many white
    _    <- str "end" `syntaxError` ExpectScopeEnd
    return body

{- This is a parser which parses an int type. -}
typeInt :: WaccParser Type
typeInt = Int ~ void (str "int")

{- This is a parser which parses a bool type. -}
typeBool :: WaccParser Type
typeBool = Bool ~ void (str "bool")

{- This is a parser which parses a char type. -}
typeChar :: WaccParser Type
typeChar = Char ~ void (str "char")

{- This is a parser which parses a string type. -}
typeString :: WaccParser Type
typeString = String ~ void (str "string")

{- It is a parser which parses a pair type. -}
typePair :: WaccParser Type
typePair = Pair ~ do
    _ <- str "pair"
    optional $ do
        _ <- many white
        _ <- char '('
        a <- typeArrayOrPointer
        _ <- surroundManyWhites (char ',')
        b <- typeArrayOrPointer
        _ <- many white
        _ <- char ')'
        return (a, b)

{- It is a parser which parses type. It will try each type in the list. -}
baseType :: WaccParser Type
baseType = asum [
    typeInt,
    typeBool,
    typeChar,
    typeString,
    typePair
    ]



{- It is a parser which parses an array type. -}
typeIdent :: (WaccParser a, Type -> Range -> Type) ->  WaccParser Type
typeIdent (parser, ident) = do
    ((from, _), t) <- getRangeA baseType
    brackets <- many $ getRangeA $ void $ parser *> many white

    let mergeIdent x ((_, to), _) = ident x (from, to)
    return $ foldl mergeIdent t brackets

typeArrayOrPointer :: WaccParser Type
typeArrayOrPointer =
    asum (map typeIdent 
    [(surroundManyWhites (char '[') >> char ']', Array),
     (surroundManyWhites (char '*')            , Pointer)])


{- It is a parser which parses an array type or a pair type and
   reports a syntax error if needed. -}
type' :: WaccParser Type
type' = typeArrayOrPointer `syntaxErrorWhen` (not . isType, \(_, t) -> findError t)
    where
    findError = \case
        Array (Pair Nothing (from, _)) _ ->
            SyntaxError from PairTypeErased
        Pair (Just (Pair (Just {}) (from, _), _)) _ ->
            SyntaxError from PairTypeInPairTypeNotErased
        Pair (Just (_, Pair (Just {}) (from, _))) _ ->
            SyntaxError from PairTypeInPairTypeNotErased
        _ -> P.error "unreachable"

{- It is a parser which parses different kinds of declaration statements. -}
statementDeclare :: WaccParser Statement
statementDeclare = Declare ~ do
    t     <- type'
    _     <- some white
    n     <- identifierString `syntaxError` ExpectIdentifierInDeclaration
    _     <- surroundManyWhites $ char '=' `syntaxError` ExpectDeclareEqualSign
    value <- rightValue
    return (t, n, value)

{- It is a parser which parses different kinds of assignment statements. -}
statementAssign :: WaccParser Statement
statementAssign = Assign ~ do
    left  <- leftValue
    _     <- surroundManyWhites $ char '=' `syntaxError` ExpectAssignEqualSign
    right <- rightValue
    return (left, right)

{- It is a parser which parses different kinds of statements,
   it will try each statement parser in the list. -}
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

{- It is a parser which parses whitespaces and colons. -}
statementSep :: Parser error ()
statementSep = void $ surroundManyWhites (char ';')

{- It is a parser which parses statements separated by whitespaces
   and colons. -}
statements :: WaccParser [Statement]
statements = statement `separatedBy` statementSep

{- It is a parser which parses a 'name', that is an identifier. -}
name :: WaccParser Name
name = Name ~ identifierString

{- It is a parser which parses parameters in function headers. -}
parameter :: WaccParser (Name, Type)
parameter = do
    t <- type'
    _ <- some white
    p <- name
    return (p, t)

{- It is a parser which parses a statement which must not return anything. -}
statementMustNotReturn :: WaccParser Statement
statementMustNotReturn = statement `that` (not . willReturn)

{- It is a parser which parses a statement which must return something. -}
statementMustReturn :: WaccParser Statement
statementMustReturn = statement `that` willReturn

{- It is a parser which parses a funcion. -}
function :: WaccParser Function
function = Function ~ do
    typePosition <- get
    maybeType  <- optional (type' <* some white)
    n          <- name
    _          <- many white
    _          <- char '('
    parameters <- optional $ surroundManyWhites $
                    parameter `separatedBy` surroundManyWhites (char ',')
    _          <- char ')' `syntaxError` UnmatchedBracket
    _          <- many white
    isPosition <- get
    maybeIs    <- optional (str "is" <* some white)
    body       <- statements `syntaxErrorWhen` (
                    not . willReturn . last,
                    \(_, body) ->
                        SyntaxError (fst (statementRange (last body)))
                        (FunctionDoesNotReturn n)
                    )
    _          <- many white
    _          <- str "end" `syntaxError` ExpectFunctionEnd
    case maybeType of
        Nothing -> do
            put typePosition
            failWith $ const $ SyntaxError (inputPosition typePosition)
                FunctionMissingType
        Just t -> do
            case maybeIs of
                Nothing -> do
                    put isPosition
                    failWith $ const $ SyntaxError (inputPosition isPosition)
                        FunctionMissingIs
                Just {} ->
                    return (t, n, if null parameters then [] else fromJust parameters, body)

{- It is a parser which parses a program with a 'begin' statement
   at the beginning and an 'end' statement at the end. -}
program' :: WaccParser ([Function], [Statement])
program' = do
    _         <- surroundManyWhites (str "begin" `syntaxError`
                                     ExpectProgramBegin)
    functions <- optional $ function `separatedBy` many white
    _         <- many white
    body      <- statements
    _         <- many white

    trailingFunctionPosition <- get
    maybeTrailingFunctions <-
        optional $ void $ function `separatedBy` many white

    _         <- surroundManyWhites (str "end" `syntaxError` ExpectProgramEnd)

    case maybeTrailingFunctions of
        Just {} -> do
            put trailingFunctionPosition
            failWith (const $ SyntaxError
                (inputPosition trailingFunctionPosition) MainTrailingFunctions)
        Nothing ->
            return (if null functions then [] else fromJust functions, body)

{- It is a parser which parses a program and
   detects any code after the 'end' statement. -}
program :: WaccParser Program
program = Program ~ Parser (\input -> do
    case parse program' input of
        (Right (Parsed (_, to) _ (_, _:_))) ->
            Left $ Just (SyntaxError to UnexpectedCodeAfterProgramEnd)
        x -> x
    )