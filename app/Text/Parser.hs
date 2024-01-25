module Text.Parser where

import Prelude hiding (error)

import Control.Applicative

type Range = (Int, Int)
type InputStream = (Int, [(Int, Char)])
type ParserResult error object = Either (Maybe (SyntaxError error)) (Parsed object)

data SyntaxError error = SyntaxError Int error
    deriving Show

data Parsed object = Parsed Range object InputStream
    deriving Show

newtype Parser error object = Parser {
    parse :: InputStream -> ParserResult error object
}

inputPosition :: InputStream -> Int
inputPosition (len, []) = len
inputPosition (_, (position, _):_) = position

parseString :: Parser error object -> String -> ParserResult error object
parseString parser string = parse parser (length string, zip [0..] string)

syntaxError :: Parser error object -> error -> Parser error object
syntaxError parser error = Parser $ \input -> case parse parser input of
    Left Nothing -> Left $ Just $ SyntaxError (inputPosition input) error
    x -> x

instance Functor (Parser error) where
    fmap :: (a -> b) -> Parser error a -> Parser error b
    mapper `fmap` parser = Parser $ \input -> do
        Parsed range result rest <- parse parser input
        Right $ Parsed range (mapper result) rest

instance Applicative (Parser error) where
    pure :: a -> Parser error a
    pure constant = Parser $ \input -> return $
        Parsed (inputPosition input, inputPosition input) constant input

    (<*>) :: Parser error (a -> b) -> Parser error a -> Parser error b
    parser1 <*> parser2 = Parser $ \input -> do
        Parsed (from, _) mapper rest <- parse parser1 input
        case parse parser2 rest of
            Right (Parsed (_, to) item rest') ->
                Right $ Parsed (from, to) (mapper item) rest'
            Left x -> Left x

    (*>) :: Parser error a -> Parser error b -> Parser error b
    parser1 *> parser2 = Parser $ \input -> do
        Parsed _ _ rest <- parse parser1 input
        parse parser2 rest

    (<*) :: Parser error a -> Parser error b -> Parser error a
    parser1 <* parser2 = Parser $ \input -> do
        Parsed range result rest <- parse parser1 input
        case parse parser2 rest of
            Right (Parsed _ _ rest') -> Right $ Parsed range result rest'
            Left x -> Left x

instance Alternative (Parser error) where
    empty :: Parser error a
    empty = Parser $ const $ Left Nothing

    (<|>) :: Parser error a -> Parser error a -> Parser error a
    parser1 <|> parser2 = Parser $ \input ->
        case parse parser1 input of
            Right result -> Right result
            Left Nothing -> parse parser2 input
            x -> x

instance Monad (Parser error) where
    return :: a -> Parser error a
    return = pure

    (>>=) :: Parser error a -> (a -> Parser error b) -> Parser error b
    parser >>= newParserGenerator = Parser $ \input -> do
        Parsed (from, _) item rest <- parse parser input
        case parse (newParserGenerator item) rest of
            Right (Parsed (_, to) item' rest') ->
                Right $ Parsed (from, to) item' rest'
            x -> x

    (>>) :: Parser error a -> Parser error b -> Parser error b
    (>>) = (*>)

char :: Char -> Parser error Char
char character = Parser $ \(len, stream) -> case stream of
    [] -> Left Nothing
    (position, inputCharacter) : inputRest ->
        if character == inputCharacter then
            Right $ Parsed (position, position + 1) character (len, inputRest)
        else
            Left Nothing

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

str :: String -> Parser error String
str = traverse char

separatedBy :: Alternative f => f a1 -> f a2 -> f [a1]
parser `separatedBy` it = (:) <$> parser <*> many (it *> parser)

surroundedBy :: Applicative f => f a -> f b -> f a
parser `surroundedBy` it = it *> parser <* it

one :: Parser error Char
one = charThat (const True)

getRangeA :: Parser error object -> Parser error (Range, object)
getRangeA parser = Parser $ \input -> do
    case parse parser input of
        Right (Parsed range result rest) -> Right $ Parsed range (range, result) rest
        Left x -> Left x
