module Text.SourceCode where
    
import Data.Function ( (&) )

-- | 这是一个中日韩统一表意字符吗？
--   Is this a Chinese, Japanese, Korean unified Ideogram?
isCJK :: Char -> Bool
isCJK c = c >= '\x4E00' && c <= '\x9FFF'

-- | この文字は仮名ですか？
--   Is this a Kana letter?
isKana :: Char -> Bool
isKana c = c >= '\x3040' && c <= '\x30FF'

isPunctuationCJK :: Char -> Bool
isPunctuationCJK c = c >= '\x3000' && c <= '\x303F'

isFullWidthPunctuation :: Char -> Bool
isFullWidthPunctuation c = c >= '\xff01' && c <= '\xff5e'

isCommonFullWidth :: Char -> Bool
isCommonFullWidth c = any (c &) [isCJK, isKana, isPunctuationCJK, isFullWidthPunctuation]

width :: String -> Int
width input = length input + length (filter isCommonFullWidth input)

widthBasedIndex :: Int -> String -> Int
widthBasedIndex index input = width $ take index input

textPosition :: String -> Int -> (Int, Int)
textPosition text i = f text i (0, 0) where
    f :: String -> Int -> (Int, Int) ->(Int, Int)
    f _ 0 pos = pos
    f [] _ _ = (-1, -1)
    f ('\n':text') n (row, _) = f text' (n-1) (row+1, 0)
    f ('\r':'\n':text') n (row, _) = f text' (n-1) (row+1, 0)
    f (_:text') n (row, col) = f text' (n-1) (row, col+1)

underlineFrom :: Int -> (Char, String -> String) -> String -> [String]
underlineFrom index (lineStyle, modifier) line = [line,
    replicate (min index' (width line)) ' ' ++
    modifier (replicate (width line - index') lineStyle)]
    where index' = widthBasedIndex index line

underlineLineSection :: Int -> Int -> (Char, String -> String) -> String -> [String]
underlineLineSection from to (lineStyle, modifier) line = [line,
    replicate (min from' (width line)) ' ' ++
    modifier (replicate (min (to' - from') (width line - from')) lineStyle)]
    where
    from' = widthBasedIndex from line
    to' = widthBasedIndex to line

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

    f (0, fromCol) (0, toCol) (line:rest) =
        underlineLineSection fromCol toCol (lineStyle, modifier) line ++
        take padding rest

    f (0, fromCol) (toRow, toCol) (line:rest) =
        underlineFrom fromCol (lineStyle, modifier) line ++
        f (0, 0) (toRow - 1, toCol) rest

    f (fromRow, fromCol) (toRow, toCol) ls =
        unmodifiedLines ++
        f (0, fromCol) (toRow - fromRow, toCol) rest
        where
        droppedLinesCount = max (fromRow - padding) 0
        unmodifiedLinesCount = min padding fromRow
        unmodifiedLines = take unmodifiedLinesCount $ drop droppedLinesCount ls
        rest = drop fromRow ls
