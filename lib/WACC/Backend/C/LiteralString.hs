module WACC.Backend.C.LiteralString where

import qualified Data.Map as M

constantNameFromStringNo :: Int -> [Char]
constantNameFromStringNo n = "STR_" ++ show n ++ "__"

dumpLiteralString :: (String, Int) -> String
dumpLiteralString (string, number) =
    "WACC_Array " ++ constantNameFromStringNo number ++ " = { " ++
    show (length string + 1) ++ ", 1, " ++ show string ++ " };"

dumpDataSegments :: M.Map String Int -> [String]
dumpDataSegments = map dumpLiteralString . M.toList
