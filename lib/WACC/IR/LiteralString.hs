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
    getLiteralStrings (Program fs (Block ss)) =
        getLiteralStrings (S.toList fs) `S.union` getLiteralStrings ss

instance HasLiteralStrings Function where
    getLiteralStrings :: Function -> S.Set String
    getLiteralStrings (Function _ _ _ (Block ss)) = getLiteralStrings ss

instance HasLiteralStrings Statement where
    getLiteralStrings :: Statement -> S.Set String
    getLiteralStrings = \case
        Declare _ _ (LiteralString s) -> S.singleton s
        Assign _ _ (LiteralString s) -> S.singleton s
        Return (LiteralString s) -> S.singleton s
        Print String (LiteralString s) -> S.singleton s
        PrintLine String (LiteralString s) -> S.singleton s
        Scope (Block ss') -> getLiteralStrings ss'
        If _ (Block ss') (Block ss'') ->
            getLiteralStrings ss' `S.union` getLiteralStrings ss''
        While _ (Block ss') -> getLiteralStrings ss'

        _ -> S.empty

newtype DataSegments = DataSegments (String `M.Map` Int)

createDataSegments :: Program -> M.Map String Int
createDataSegments =
        getLiteralStrings
    >>> S.toList
    >>> (`zip` [1..])
    >>> M.fromList
