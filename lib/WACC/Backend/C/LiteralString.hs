module WACC.Backend.C.LiteralString where

import qualified Data.Map as M

dump :: (String, Int) -> String
dump (string, number) =
    "WACC_Array STRING_" ++ show number ++ "__" ++ " = { " ++
    show (length string + 1) ++ ", 1, " ++ show string ++ " };"

dumpDataSegments :: M.Map String Int -> [String]
dumpDataSegments = map dump . M.toList
