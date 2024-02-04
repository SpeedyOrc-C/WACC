module Text.SourceCode where
    
import Data.Function ( (&) )

{- Checks whether the character is a Chinese, Japanese, or Korean unified 
Ideogram. -}
isCJK :: Char -> Bool
isCJK c = c >= '\x4E00' && c <= '\x9FFF'

{- Checks whether the character is a Kana letter. -}
isKana :: Char -> Bool
isKana c = c >= '\x3040' && c <= '\x30FF'

{- Checks whether the character is a punctuation mark in Chinese, Japanese, or 
Korean. -}
isPunctuationCJK :: Char -> Bool
isPunctuationCJK c = c >= '\x3000' && c <= '\x303F'

{- Checks whether the character is a full-width punctuation mark. -}
isFullWidthPunctuation :: Char -> Bool
isFullWidthPunctuation c = c >= '\xff01' && c <= '\xff5e'

{- Checks whether the character is a common full-width character. -}
isCommonFullWidth :: Char -> Bool
isCommonFullWidth c = any (c &)
  [isCJK, isKana, isPunctuationCJK, isFullWidthPunctuation]

{- Returns the width of the string in half-width. -}
width :: String -> Int
width input = length input + length (filter isCommonFullWidth input)

{- Returns the width of the first n characters in half-width. -}
widthBasedIndex :: Int -> String -> Int
widthBasedIndex index input = width $ take index input

{- Returns the position (row, col) of the character with the given index. -}
textPosition :: String -> Int -> (Int, Int)
textPosition text i = f text i (0, 0) where
    f :: String -> Int -> (Int, Int) ->(Int, Int)
    f _ 0 pos = pos
    f [] _ _ = (-1, -1)
    f ('\n':text') n (row, _) = f text' (n-1) (row+1, 0)
    f ('\r':'\n':text') n (row, _) = f text' (n-1) (row+1, 0)
    f (_:text') n (row, col) = f text' (n-1) (row, col+1)

{- Returns a list of two strings with the first one being the given string and 
the second one being the underline of it from index to the end using a 
repetition of the given character, and applying the given text style to the 
underline. -}
underlineFrom :: Int -> (Char, String -> String) -> String -> [String]
underlineFrom index (lineStyle, modifier) line = [line,
    replicate (min index' (width line)) ' ' ++
    modifier (replicate (width line - index') lineStyle)]
    where index' = widthBasedIndex index line

{- Returns a list of two strings with the first one being the given string and 
the second one being the underline of it from `from` index (inclusive) to `to` 
index (exclusive), using a repetition of the given character, and applying the 
given text style to the underline. -}
underlineLineSection :: Int -> Int -> (Char, String -> String) -> String
  -> [String]
underlineLineSection from to (lineStyle, modifier) line = [line,
    replicate (min from' (width line)) ' ' ++
    modifier (replicate (min (to' - from') (width line - from')) lineStyle)]
    where
    from' = widthBasedIndex from line
    to' = widthBasedIndex to line

{- Returns a list of strings that when concatenated giving the underlined 
version of the input text, with the underline from `fromIndex` (inclusive) to 
`toIndex` (exclusive), using a repetition of the given character, and applying 
the given text style to the underline. The underlined section is surrounded by 
`padding` number of original lines above and below it. -}
underlineTextSection
    :: Int -> Int
    -> (Int, Char, String -> String)
    -> String
    -> [String]
underlineTextSection fromIndex toIndex (padding, lineStyle, modifier) input =
    f from to (lines input)
    where

    from = textPosition input fromIndex
    to = textPosition input toIndex

    f :: (Int, Int) -> (Int, Int) -> [String] -> [String]
    f _ _ [] = []

    {- Ensures that there are `padding` number of original lines below the underlined section. -}
    f (0, fromCol) (0, toCol) (line:rest) =
        underlineLineSection fromCol toCol (lineStyle, modifier) line ++
        take padding rest

    f (0, fromCol) (toRow, toCol) (line:rest) =
        underlineFrom fromCol (lineStyle, modifier) line ++
        f (0, 0) (toRow - 1, toCol) rest

    {- Ensures that there are `padding` number of original lines above the underlined section. -}
    f (fromRow, fromCol) (toRow, toCol) ls =
        unmodifiedLines ++
        f (0, fromCol) (toRow - fromRow, toCol) rest
        where
        droppedLinesCount = max (fromRow - padding) 0
        unmodifiedLinesCount = min padding fromRow
        unmodifiedLines = take unmodifiedLinesCount $ drop droppedLinesCount ls
        rest = drop fromRow ls

{- Replaces all tabs in the input string with four spaces. -}
removeTabs :: String -> String
removeTabs s = s >>= \case
    '\t' -> "    "
    c -> [c]
