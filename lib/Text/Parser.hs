module Text.Parser where

import Prelude hiding (error)

import Control.Applicative

{- (fromIndex, toIndex) -}
type Range = (Int, Int)

{- (length, [(index, character)]) -}
type InputStream = (Int, [(Int, Char)])

{- Represents the result of parsing. The result can be a `Left` value, 
indicating failure, or a `Right` value, indicating success. If parsing fails, 
`Left Nothing` indicates a mismatch of the pattern but not necessarily an 
error, while `Left Just SyntaxError` indicates that a syntax error has 
occurred. If successfully parsed, the matched result is included. -}
type ParserResult error object
  = Either (Maybe (SyntaxError error)) (Parsed object)

{- A syntax error has a location and an error type. -}
data SyntaxError error = SyntaxError Int error
    deriving Show

{- A successfully parsed result includes the range of the input stream 
consumed, the parsed object, and the remaining input stream after parsing. -}
data Parsed object = Parsed Range object InputStream
    deriving Show

{- A parser that takes an input stream and returns the result of parsing. -}
newtype Parser error object = Parser {
    parse :: InputStream -> ParserResult error object
}

{- Returns the index of the current character in the input stream. -}
inputPosition :: InputStream -> Int
inputPosition (len, []) = len
inputPosition (_, (position, _):_) = position

{- Turns the string into an input stream and parse it using the given parser to 
get the result. -}
parseString :: Parser error object -> String -> ParserResult error object
parseString parser string = parse parser (length string, zip [0..] string)

{- Attaches a syntax error to the parser. If the parser returns `Left Nothing`, 
make it return the given syntax error instead. -}
syntaxError :: Parser error object -> error -> Parser error object
syntaxError parser error = Parser $ \input -> case parse parser input of
    Left Nothing -> Left $ Just $ SyntaxError (inputPosition input) error
    x -> x

{- Makes the parser return a syntax error generated from the parsed range and 
parsed result instead of a `Right` result if the parser succeeds and the 
condition is true for the parsed result. -}
syntaxErrorWhen
    :: Parser error object
    -> (object -> Bool, (Range, object) -> SyntaxError error)
    -> Parser error object
syntaxErrorWhen parser (condition, error) = Parser $ \input
  -> case parse parser input of
    Right (Parsed range result rest) ->
        if condition result
            then Left $ Just $ error (range, result)
            else Right $ Parsed range result rest
    x -> x

{- Makes Parser an instance of the Functor type class. -}
instance Functor (Parser error) where

    {- If the parser succeeds, make it return another result generated using 
    the given function and the original result. -}
    fmap :: (a -> b) -> Parser error a -> Parser error b
    mapper `fmap` parser = Parser $ \input -> do
        Parsed range result rest <- parse parser input
        Right $ Parsed range (mapper result) rest

{- Makes Parser an instance of the Applicative type class. -}
instance Applicative (Parser error) where

    {- Returns a parser that always succeeds with the result being the given 
    value and does not consume any input stream. -}
    pure :: a -> Parser error a
    pure constant = Parser $ \input -> return $
        Parsed (inputPosition input, inputPosition input) constant input

    {- Sequence the two parsers. If both parsers succeed, return the combined 
    range of two parsings and the second result mapped by the function from the 
    first result. -}
    (<*>) :: Parser error (a -> b) -> Parser error a -> Parser error b
    parser1 <*> parser2 = Parser $ \input -> do
        Parsed (from, _) mapper rest <- parse parser1 input
        case parse parser2 rest of
            Right (Parsed (_, to) item rest') ->
                Right $ Parsed (from, to) (mapper item) rest'
            Left x -> Left x

    {- Sequence the two parsers. If both parsers succeed, return the range and 
    result of the second parser. -}
    (*>) :: Parser error a -> Parser error b -> Parser error b
    parser1 *> parser2 = Parser $ \input -> do
        Parsed _ _ rest <- parse parser1 input
        parse parser2 rest

    {- Sequence the two parsers. If both parsers succeed, return the range and 
    result of the first parser. -}
    (<*) :: Parser error a -> Parser error b -> Parser error a
    parser1 <* parser2 = Parser $ \input -> do
        Parsed range result rest <- parse parser1 input
        case parse parser2 rest of
            Right (Parsed _ _ rest') -> Right $ Parsed range result rest'
            Left x -> Left x

{- Makes Parser an instance of the Alternative type class. -}
instance Alternative (Parser error) where

    {- Generates a parser that always fails with `Left Nothing`. -}
    empty :: Parser error a
    empty = Parser $ const $ Left Nothing

    {- Parse using the two parsers alternatively. If the first parser succeeds, 
    return the successful result. If the first parser returns `Left Nothing`, 
    parse the same input stream using the second parser. If the first parser 
    returns a syntax error, return this error. -}
    (<|>) :: Parser error a -> Parser error a -> Parser error a
    parser1 <|> parser2 = Parser $ \input ->
        case parse parser1 input of
            Right result -> Right result
            Left Nothing -> parse parser2 input
            x -> x

{- Makes Parser an instance of the Monad type class. -}
instance Monad (Parser error) where

    {- Returns a parser that always succeeds with the result being the given 
    value and does not consume any input stream. -}
    return :: a -> Parser error a
    return = pure

    {- First parse using the given parser. If it succeeds, generate another 
    parser using the parsed result and the generator function, and parse the 
    remaining input stream using this generated parser. Return the combined 
    range and the second parsed result if succeeds. -}
    (>>=) :: Parser error a -> (a -> Parser error b) -> Parser error b
    parser >>= newParserGenerator = Parser $ \input -> do
        Parsed (from, _) item rest <- parse parser input
        case parse (newParserGenerator item) rest of
            Right (Parsed (_, to) item' rest') ->
                Right $ Parsed (from, to) item' rest'
            x -> x

    {- Sequence the two parsers. If both parsers succeed, return the range and 
    result of the second parser. -}
    (>>) :: Parser error a -> Parser error b -> Parser error b
    (>>) = (*>)

{- Generates a parser that parses whether the current character is the given 
character. Returns the current position of the input stream and the character 
if succeeds. Otherwise, return `Left Nothing`. -}
char :: Char -> Parser error Char
char character = Parser $ \(len, stream) -> case stream of
    [] -> Left Nothing
    (position, inputCharacter) : inputRest ->
        if character == inputCharacter then
            Right $ Parsed (position, position + 1) character (len, inputRest)
        else
            Left Nothing

{- Generates a parser that parses whether the current character satisfies the 
given condition. Returns the current position of the input stream and the 
character if succeeds. Otherwise, return `Left Nothing`. -}
charThat :: (Char -> Bool) -> Parser error Char
charThat condition = Parser $ \(len, stream) -> case stream of
    [] -> Left Nothing
    (position, inputCharacter) : inputRest ->
        if condition inputCharacter then
            Right $ Parsed
                (position, position + 1)
                inputCharacter
                (len, inputRest)
        else
            Left Nothing

{- Modifies the behaviour of the given parser to return a new parser that 
checks whether the parsed result of the given parser satisfies the given 
condition. If true, return the same result. If false, return `Left Nothing`. -}
that :: Parser error object -> (object -> Bool) -> Parser error object
that parser condition = Parser $ \input -> do
    Parsed range result rest <- parse parser input
    if condition result
        then Right $ Parsed range result rest
        else Left Nothing

{- Generates a parser that parses whether the current character satisfies the 
given condition. Returns the previous position of the input stream and the 
character if succeeds. Otherwise, return `Left Nothing`. -}
follows :: (Char -> Bool) -> Parser error Char
follows condition = Parser $ \(len, stream) -> case stream of
    [] -> Left Nothing
    (position, inputCharacter) : inputRest ->
        if condition inputCharacter then
            Right $ Parsed
                (position - 1, position)
                inputCharacter
                (len, (position, inputCharacter) : inputRest)
        else
            Left Nothing

{- Generates a new parser using the given two parsers that parses the input 
stream using both. If both succeeds, return the parsed range and result of the 
first parser. -}
followedBy :: Parser error a -> Parser error b -> Parser error a
followedBy parser1 parser2 = Parser $ \input -> do
    Parsed range result rest <- parse parser1 input
    case parse parser2 rest of
        Right (Parsed {}) -> Right $ Parsed range result rest
        Left x -> Left x

{- Generates a new parser using the given two parsers that parses the input 
stream using both. If the first succeeds and the second fails, then return 
successfully the parsed range and result of the first parser. If the first 
succeeds and the second also succeeds, return `Left Nothing`. -}
notFollowedBy :: Parser error a -> Parser error' b -> Parser error a
notFollowedBy parser1 parser2 = Parser $ \input -> do
    Parsed range result rest <- parse parser1 input
    case parse parser2 rest of
        Right {} -> Left Nothing
        _ -> Right $ Parsed range result rest

{- Given a string, generates a parser that parses whether the input stream 
begins with this string. If succeeds, return the whole range and the matched 
string. -}
str :: String -> Parser error String
str = traverse char

{- Given a parser for an object and a parser for a separator, generates a new 
parser that parses a sequence of the objects separated by the separator. The 
parsed range will be the whole sequence and the parsed result will be a list of 
the objects. It will fail if the input stream does not begin with an object. -}
separatedBy :: Alternative f => f a1 -> f a2 -> f [a1]
parser `separatedBy` it = (:) <$> parser <*> many (it *> parser)

{- Given a parser for an object and a parser for a surrounder, generates a new 
parser that parses an object surrounded by two surrounders. The parsed range 
and result will be that of the object. It will fail if the input stream does 
not begin with a surrounder. -}
surroundedBy :: Applicative f => f a -> f b -> f a
parser `surroundedBy` it = it *> parser <* it

{- Generates a parser that always succeeds and returns the first character in 
the input stream as the parsed result and the current position as the parsed 
range. It consumes that character from the input stream. -}
one :: Parser error Char
one = charThat (const True)

{- Modifies the given parser so that when it succeeds, it also requires that 
all the input stream is consumed. Otherwise, return `Left Nothing`. -}
strict :: Parser error object -> Parser error object
strict parser = Parser $ \input -> case parse parser input of
    Right (Parsed range result (len, []))
      -> Right $ Parsed range result (len, [])
    Right {} -> Left Nothing
    x -> x

{- Modifies the given parser so that when it succeeds, the parsed result will 
be the tuple containing the parsed range and the parsed result. -}
getRangeA :: Parser error object -> Parser error (Range, object)
getRangeA parser = Parser $ \input -> do
    case parse parser input of
        Right (Parsed range result rest)
          -> Right $ Parsed range (range, result) rest
        Left x -> Left x
