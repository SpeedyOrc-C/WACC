module WACC.Semantics.Error where

import WACC.Semantics.Structure

data OperandDirection = OperandLeft | OperandRight deriving Show

{- Define different semantic error types. -}
data WaccSemanticsErrorType
    = UndefinedIdentifier String
    | UndefinedFunction String
    | UndefinedStructure String
    | UndefinedField String String
    | ShouldBeStruct
    | OnlyAcceptIdentifier
    | RedefinedIdentifier String Int
    | RedefinedFunction String
    | ArgumentNumberMismatch String Int Int
    | StructureNumberMismatch String Int Int
    | InconsistentTypesInArray Type Type
    | IncompatibleAssignment Type Type
    | IncompatibleArgument Type Type
    | RedefinedParameter String
    | RedefinedField String String
    | RedefinedStruct String
    | InvalidRead Type
    | InvalidFree Type
    | InvalidReturn Type Type
    | ReturnInMain
    | InvalidCondition Type
    | InvalidArray Type
    | InvalidIndex Type
    | InvalidPair Type
    | InvalidNot Type
    | InvalidNegate Type
    | InvalidLength Type
    | InvalidOrder Type
    | InvalidCharacter Type
    | InvalidArithmetic Type
    | InvalidComparisonLeft Type
    | InvalidComparisonRight ComparisonType Type
    | InvalidEquality Type Type
    | InvalidLogical Type
    | BothSideAnyAssignment
    | InvalidExit Type

{- Define 'Show' for all kinds of semantic error types. -}
instance Show WaccSemanticsErrorType where
    show :: WaccSemanticsErrorType -> String
    show OnlyAcceptIdentifier =
        "Only accept identifier, array element, field, pair first and pair se\
        \cond for passing in a reference"
    show ShouldBeStruct =
        "Should be a struct type to allow getting field"
    show (UndefinedIdentifier identifier) =
        "Variable \"" ++ identifier ++ "\" is not declared."
    show (UndefinedFunction function) =
        "Function \"" ++ function ++ "\" is not declared."
    show (UndefinedStructure struct) =
        "Struct \"" ++ struct ++ "\" is not declared."

    show (UndefinedField struct field) =
        "Field \"" ++ field ++ "\" is not declared in the Struct \"" ++ struct ++ "\"."
    show (RedefinedIdentifier identifier _) =
        "Variable \"" ++ identifier ++ "\" is declared again."
    show (RedefinedFunction function) =
        "Function \"" ++ function ++ "\" is defined again."
    show (RedefinedStruct struct) =
        "Struct \"" ++ struct ++ "\" is defined again."
    show (RedefinedField structure field) =
        "Field \"" ++ field ++ "\" is defined again at struct " ++ structure ++ "."
    show (ArgumentNumberMismatch name expect actual) =
        "Function \"" ++ name ++ "\" takes " ++ show expect ++
        " argument" ++ (if expect > 1 then "s" else "") ++ ". " ++
        "Actual: " ++ show actual
    show (StructureNumberMismatch name expect actual) =
        "Struct \"" ++ name ++ "\" has " ++ show expect ++
        " field" ++ (if expect > 1 then "s" else "") ++ ". " ++
        "Actual: " ++ show actual
    show (InconsistentTypesInArray typeBefore typeAfter) =
        "Inconsistent types: type before is " ++ show typeBefore
        ++ ", type after is " ++ show typeAfter ++ "."
    show (IncompatibleAssignment expectType actualType) =
        "Cannot assign " ++ show actualType ++ " to " ++ show expectType ++ "."
    show (IncompatibleArgument expectType actualType) =
        "IncompatibleArgument: expected type " ++ show expectType
        ++ " unexpected " ++ show actualType ++ "."
    show (RedefinedParameter identifier) =
        "Parameter \"" ++ identifier ++ "\" is declared again."
    show (InvalidRead invalidType) =
        "Can only read char or int. Actual: " ++ show invalidType
    show (InvalidFree invalidType) =
        "Can only free pair or array. Actual: " ++ show invalidType
    show ReturnInMain =
        "Unexpected return in main program."
    show (InvalidCondition invalidType) =
        "Condition should be bool. Actual: " ++ show invalidType
    show (InvalidArray invalidType) =
        "Only array can be indexed. Actual: " ++ show invalidType
    show (InvalidIndex invalidType) =
        "Array's index should be int. Actual: " ++ show invalidType
    show (InvalidPair invalidType) =
        "\"fst\" and \"snd\" are only for pair. Actual: " ++ show invalidType
    show (InvalidNot invalidType) =
        "\"!\" is only for bool. Actual: " ++ show invalidType
    show (InvalidNegate invalidType) =
        "\"-\" is only for int. Actual: " ++ show invalidType
    show (InvalidLength invalidType) =
        "\"len\" is only for array. Actual: " ++ show invalidType
    show (InvalidOrder invalidType) =
        "\"ord\" is only for char. Actual: " ++ show invalidType
    show (InvalidCharacter invalidType) =
        "\"chr\" is only for int. Actual: " ++ show invalidType
    show (InvalidArithmetic invalidType) =
        "Arithmetic operators are only for int. Actual: " ++ show invalidType
    show (InvalidComparisonLeft invalidType) =
        "Can only compare int or char. Actual: " ++ show invalidType
    show (InvalidComparisonRight expect actual) =
        "Can only compare the same type as " ++ show expect ++ ". " ++
        "Actual: " ++ show actual
    show (InvalidEquality expect actual) =
        "Can only check the equality of the same type as " ++ show expect
        ++ ". " ++ "Actual: " ++ show actual
    show (InvalidLogical invalidType) =
        "Logical operators are only for bool. Actual: " ++ show invalidType
    show BothSideAnyAssignment =
        "Types at both sides of assignment are unknown."
    show (InvalidExit invalidType) =
        "Exit code should be int. Actual: " ++ show invalidType
    show (InvalidReturn validType invalidType) =
        "This function should return " ++ show validType ++ ". " ++
        "Actual: " ++ show invalidType
