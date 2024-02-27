module WACC.IR.LiteralString where

import qualified Data.Set as S
import qualified Data.Map as M
import           Control.Arrow

import WACC.Semantics.Structure

{- Define a type class for types that contain literal strings. -}
class HasLiteralStrings a where
    getLiteralStrings :: a -> S.Set String

{- For lists of items that contain literal strings. -}
instance HasLiteralStrings a => HasLiteralStrings [a] where
    getLiteralStrings :: HasLiteralStrings a => [a] -> S.Set String
    getLiteralStrings = S.unions . map getLiteralStrings

{- For the Program type, which contains blocks of statements. -}
instance HasLiteralStrings Program where
    getLiteralStrings :: Program -> S.Set String
    getLiteralStrings (Program fs (Block ss)) =
        getLiteralStrings (S.toList fs) `S.union` getLiteralStrings ss

{- For the Function type, which contains blocks of statements. -}
instance HasLiteralStrings Function where
    getLiteralStrings :: Function -> S.Set String
    getLiteralStrings (Function _ _ _ (Block ss)) = getLiteralStrings ss

{- For the Expression type, which may contain literal strings. -}
instance HasLiteralStrings Expression where
    getLiteralStrings :: Expression -> S.Set String
    getLiteralStrings = \case
        LiteralString s -> S.singleton s
        LiteralArray _ ss -> S.unions $ getLiteralStrings <$> ss
        LiteralPair _ (a, b) -> getLiteralStrings a `S.union` 
                                getLiteralStrings b
        _ -> S.empty

{- For the Statement type, which may contain literal strings. -}
instance HasLiteralStrings Statement where
    getLiteralStrings :: Statement -> S.Set String
    getLiteralStrings = \case
        Declare _ _ e -> getLiteralStrings e
        Assign _ _ e -> getLiteralStrings e
        Return e -> getLiteralStrings e
        Print String e -> getLiteralStrings e
        PrintLine String e -> getLiteralStrings e
        Scope (Block ss') -> getLiteralStrings ss'
        If _ (Block ss') (Block ss'') ->
            getLiteralStrings ss' `S.union` getLiteralStrings ss''
        While _ (Block ss') -> getLiteralStrings ss'

        _ -> S.empty

{- Define a newtype for data segments, which maps string literals to unique 
   identifiers. -}
newtype DataSegments = DataSegments (String `M.Map` Int)

{- Function to create data segments from a program. -}
createDataSegments :: Program -> M.Map String Int
createDataSegments =
        getLiteralStrings
    >>> S.toList
    >>> (`zip` [1..])
    >>> M.fromList
