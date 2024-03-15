module WACC.IR.LiteralString where

import qualified Data.Set as S
import qualified Data.Map as M
import           Control.Arrow

import WACC.Semantics.Structure

class HasLiteralStrings a where
    getLiteralStrings :: a -> S.Set String

instance HasLiteralStrings a => HasLiteralStrings [a] where
    getLiteralStrings :: HasLiteralStrings a => [a] -> S.Set String
    getLiteralStrings = S.unions . map getLiteralStrings

instance HasLiteralStrings Program where
    getLiteralStrings :: Program -> S.Set String
    getLiteralStrings (Program _ fs (Block ss)) =
        getLiteralStrings (S.toList fs) `S.union` getLiteralStrings ss

instance HasLiteralStrings Function where
    getLiteralStrings :: Function -> S.Set String
    getLiteralStrings (Function _ _ _ (Block ss)) = getLiteralStrings ss

instance HasLiteralStrings Expression where
    getLiteralStrings :: Expression -> S.Set String
    getLiteralStrings = \case
        LiteralString s -> S.singleton s
        LiteralArray _ ss -> S.unions $ getLiteralStrings <$> ss
        LiteralPair _ (a, b) -> getLiteralStrings a `S.union`
                                getLiteralStrings b
        FunctionCall _ _ params -> getLiteralStrings (map (snd.snd) params)
        NewStruct exps -> getLiteralStrings exps
        _ -> S.empty

instance HasLiteralStrings Statement where
    getLiteralStrings :: Statement -> S.Set String
    getLiteralStrings = \case
        Declare _ _ e -> getLiteralStrings e
        Assign _ _ e -> getLiteralStrings e
        Return _ e -> getLiteralStrings e
        Print String e -> getLiteralStrings e
        PrintLine String e -> getLiteralStrings e
        Scope (Block ss') -> getLiteralStrings ss'
        If _ (Block ss') (Block ss'') ->
            getLiteralStrings ss' `S.union` getLiteralStrings ss''
        While _ (Block ss') -> getLiteralStrings ss'
        _ -> S.empty

newtype DataSegments = DataSegments (String `M.Map` Int)

internalStrings :: S.Set String
internalStrings = S.fromList ["{ ", ", ", " }"]

{- | Find all literal strings in the program, remove all the duplicated,
     and give each a unique number. -}
createDataSegments :: Program -> M.Map String Int
createDataSegments =
        getLiteralStrings
    >>> S.union internalStrings
    >>> S.toList
    >>> (`zip` [1..])
    >>> M.fromList
