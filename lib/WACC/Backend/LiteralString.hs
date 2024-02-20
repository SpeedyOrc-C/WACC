module WACC.Backend.LiteralString where

import qualified Data.Set as S
import qualified Data.Map as M

import WACC.Semantics.Structure

class HasLiteralStrings a where
    getLiteralStrings :: a -> [String]

instance HasLiteralStrings a => HasLiteralStrings [a] where
    getLiteralStrings :: HasLiteralStrings a => [a] -> [String]
    getLiteralStrings = concatMap getLiteralStrings

instance HasLiteralStrings Program where
    getLiteralStrings :: Program -> [String]
    getLiteralStrings (Program fs (Block ss)) =
        getLiteralStrings (S.toList fs) ++ getLiteralStrings ss

instance HasLiteralStrings Function where
    getLiteralStrings :: Function -> [String]
    getLiteralStrings (Function _ _ _ (Block ss)) =
        getLiteralStrings ss

instance HasLiteralStrings Statement where
    getLiteralStrings :: Statement -> [String]
    getLiteralStrings = \case
        Declare String _ (LiteralString s) -> return s
        Assign _ _ (LiteralString s) -> return s
        Return (LiteralString s) -> return s
        Print String (LiteralString s) -> return s
        PrintLine String (LiteralString s) -> return s
        Scope (Block ss') -> getLiteralStrings ss'
        If _ (Block ss') (Block ss'') -> getLiteralStrings ss' ++ getLiteralStrings ss''
        While _ (Block ss') -> getLiteralStrings ss'
        _ -> []

newtype DataSegments = DataSegments (String `M.Map` Int)

createDataSegments :: [String] -> M.Map String Int
createDataSegments = M.fromList . (`zip` [1..]) . S.toList . S.fromList
