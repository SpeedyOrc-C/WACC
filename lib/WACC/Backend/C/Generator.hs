module WACC.Backend.C.Generator where

import qualified Data.Map as M

import WACC.Semantics.Structure
import WACC.Backend.LiteralString
import WACC.Backend.C.LiteralString

import Text.Parser
import WACC.Syntax.Parser
import WACC.Semantics.Checker
import WACC.Semantics.Utils
import Data.Function
import Data.List (intercalate)

p s =
    let
    Right (Parsed _ ast _) = parseString program s
    Ok ast' = check (CheckerState Nothing M.empty [M.empty]) ast
    in
    ast'

d :: IO String
d = generateCode . p <$> readFile "./draft.wacc"

data GeneratorState = GeneratorState {
    depth :: Int,
    literalStrings :: M.Map String Int
}

class Generate a b where
    generate :: GeneratorState -> a -> b

data TypeSize = I8 | I32 | Ptr deriving Show

fromType :: Type -> String
fromType = \case
    Bool -> "int8_t"
    Char -> "int8_t"
    Int -> "int32_t"
    String -> "WACC_Array"
    Array {} -> "WACC_Array*"
    Pair {} -> "WACC_Pair*"
    Any -> "void*"

generateLeftValue :: GeneratorState -> Expression -> String
generateLeftValue state = \case
    PairFirst _ e -> generate state e ++ "->fst"
    PairSecond _ e -> generate state e ++ "->snd"
    e -> generate state e

instance Generate Expression String where
    generate :: GeneratorState -> Expression -> String
    generate state = \case
        Identifier i -> i
        LiteralChar c -> show c
        LiteralInt i -> show i
        LiteralBool True -> "true"
        LiteralBool False -> "false"
        LiteralString s -> constantNameFromStringNo (literalStrings state M.! s)
        LiteralPairNull -> "0"

        LiteralPair _ (a, b) ->
            "new_WACC_Pair" ++ "(" ++
            generate state a ++ ", " ++ generate state b ++
            ")"
        PairFirst t e -> "WACC_Pair_First(" ++ generate state e ++ ")"
        PairSecond t e -> "WACC_Pair_Second(" ++ generate state e ++ ")"

        LiteralArray t a ->
            "new_WACC_Array(" ++ show (length a) ++
            ", sizeof(" ++ fromType t ++ "), (" ++ fromType t ++ "[]){" ++
            intercalate ", " (generate state <$> a) ++ "})"
        ArrayElement _ a i ->
            "WACC_Array_Index(" ++
            generate state a ++ ", " ++ generate state i ++ ")"

instance Generate Statement String where
    generate :: GeneratorState -> Statement -> String
    generate state =
        let
        ident = replicate (depth state * 4) ' '
        in \case
        PrintLine Char e -> ident ++
            "WACC_println_Char(" ++ generate state e ++ ");"
        PrintLine Int e -> ident ++
            "WACC_println_Int(" ++ generate state e ++ ");"
        PrintLine Bool e -> ident ++
            "WACC_println_Bool(" ++ generate state e ++ ");"
        PrintLine String e -> ident ++
            "WACC_println_String(&" ++ generate state e ++ ");"
        PrintLine Pair {} e -> ident ++
            "WACC_println_Address(" ++ generate state e ++ ");"
        PrintLine Array {} e -> ident ++
            "WACC_println_Address(" ++ generate state e ++ ");"

        Declare t name e -> ident ++
            fromType t ++ " " ++ name ++ " = " ++ generate state e ++ ";"

        Assign _ (Identifier l) r@(LiteralInt {}) -> ident ++
            l ++ " = " ++ generate state r ++ ";"
        Assign _ (Identifier l) r@(LiteralBool {}) -> ident ++
            l ++ " = " ++ generate state r ++ ";"
        Assign _ (Identifier l) r@(LiteralChar {}) -> ident ++
            l ++ " = " ++ generate state r ++ ";"
        Assign _ (Identifier l) r@(LiteralPairNull {}) -> ident ++
            l ++ " = " ++ generate state r ++ ";"
        
        Assign t l r@(LiteralInt {}) -> ident ++
            "memset(&" ++ generate state l ++
            ", " ++ generate state r ++ ", sizeof(" ++ fromType t  ++ "));"
        Assign t l r@(LiteralBool {}) -> ident ++
            "memset(&" ++ generate state l ++
            ", " ++ generate state r ++ ", sizeof(" ++ fromType t  ++ "));"
        Assign t l r@(LiteralChar {}) -> ident ++
            "memset(&" ++ generate state l ++
            ", " ++ generate state r ++ ", sizeof(" ++ fromType t  ++ "));"
        Assign t l r@(LiteralPairNull {}) -> ident ++
            "memset(&" ++ generate state l ++
            ", " ++ generate state r ++ ", sizeof(" ++ fromType t  ++ "));"

        Assign t l r -> ident ++
            "memcpy(&" ++ generate state l ++
            ", &" ++ generate state r ++ ", sizeof(" ++ fromType t  ++ "));"
            -- generateLeftValue state l ++ " = " ++ generate state r ++ ";"

instance Generate [Statement] [String] where
    generate state = map (generate state)

instance Generate Program [String] where
    generate state p@(Program functions body) =
        ["#include <stdlib.h>"
        ,"#include <memory.h>"
        ,"#include \"WACC.h\""] ++
        strings ++
        [""] ++
        ["int main(void) {"] ++
            generate (state' {depth = depth state' + 1}) body ++
        ["}"]
        where
        strings = dumpDataSegments . createDataSegments . getLiteralStrings $ p
        state' = state {
            literalStrings = createDataSegments . getLiteralStrings $ p
        }

generateCode :: Program -> String
generateCode = unlines . generate (GeneratorState 0 M.empty)
