module WACC.Semantics.Error where

import WACC.Semantics.Structure

data OperandDirection = OperandLeft | OperandRight deriving Show

data WaccSemanticsErrorType
    = UndefinedIdentifier String
    | UndefinedFunction String
    | RedefinedIdentifier String
    | TooManyArguments String Int Int
    | TooFewArguments String Int Int
    | InconsistentTypesInArray Type Type
    | IncompatibleAssignment Type Type
    | IncompatibleArgument Type Type
    | RedefinedFunction String
    | RedefinedParameter String
    | InvalidRead Type
    | InvalidFree Type
    | InvalidReturn Type Type
    | ReturnInMain
    | InvalidIfCondition Type
    | InvalidWhileCondition Type
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

instance Show WaccSemanticsErrorType where
    show :: WaccSemanticsErrorType -> String
    show (UndefinedIdentifier identifier) = 
        "Variable " ++ identifier ++ " has not been declared in this scope."
    show (UndefinedFunction function) = 
        "Function " ++ function ++ " has not been declared in this scope."
    show (RedefinedIdentifier identifier) = 
        "Illegal redeclaration of variable " ++ identifier ++ "."
    show (TooManyArguments name expect actual) = 
        "Wrong number of arguments provided to function " ++ name
        ++ " unexpected " ++ show actual ++ " arguments,"
        ++ " expected " ++ show expect ++ " arguments."
    show (TooFewArguments name expect actual) = 
        "Wrong number of arguments provided to function " ++ name
        ++ " unexpected " ++ show actual ++ " arguments,"
        ++ " expected " ++ show expect ++ " arguments."
    show (InconsistentTypesInArray typeBefore typeAfter) = 
        "Inconsistent types: type before is " ++ show typeBefore 
        ++ ", type after is " ++ show typeAfter ++ "."
    show (IncompatibleAssignment expectType actualType) = 
        "IncompatibleAssignment: expected type " ++ show expectType
        ++ " unexpected " ++ show actualType ++ "."
    show (IncompatibleArgument expectType actualType) = 
        "IncompatibleArgument: expected type " ++ show expectType
        ++ " unexpected " ++ show actualType ++ "."
    show (RedefinedFunction function) = 
        "Illegal redeclaration of function " ++ function ++ "."
    show (RedefinedParameter identifier) =
        "Illegal redeclaration of parameter " ++ identifier ++ "."
        ++ " Please rename this parameter " ++ identifier ++ "."
    show (InvalidRead invalidType) = 
        "Unexpected read type " ++ show invalidType 
        ++ ", expected Char or Int."
    show (InvalidFree invalidType) =
        "Unexpected free type " ++ show invalidType 
        ++ ", expected Pair or Array."
    show ReturnInMain =
        "Unexpected return instruction in Main program."
    show (InvalidIfCondition invalidType) = 
        "Invalid if condition type: unexcepted "
        ++ show invalidType ++ ", expected Bool."
    show (InvalidWhileCondition invalidType) = 
        "Invalid while condition type: unexcepted "
        ++ show invalidType ++ ", expected Bool."
    show (InvalidArray invalidType) = 
        "Invalid type: unexpected " ++ show invalidType ++ ", expected Array."
    show (InvalidIndex invalidType) = 
        "Invalid index type: unexpected " ++ show invalidType
        ++ ", expected Int."
    show (InvalidPair invalidType) = 
        "Invalid type: unexpected " ++ show invalidType ++ ", expected Pair."
    show (InvalidNot invalidType) = 
        "Invalid type for '!': unexpected " ++ show invalidType 
        ++ ", expected Boolean."
    show (InvalidNegate invalidType) = 
        "Invalid type for '-': unexpected " ++ show invalidType
        ++ ", expected Int."
    show (InvalidLength invalidType) = 
        "Invalid type for 'len': unexpected " ++ show invalidType
        ++ ", expected Array."
    show (InvalidOrder invalidType) = 
        "Invalid type for 'ord': unexpected " ++ show invalidType 
        ++ ", expected Char."
    show (InvalidCharacter invalidType) = 
        "Invalid type for 'chr': unexpected " ++ show invalidType
        ++ ", expected Int."
    show (InvalidArithmetic invalidType) =
        "Invalid type for arithmetic operator: unexpected " 
        ++ show invalidType ++ ", expected Int."
    show (InvalidComparisonLeft invalidType) = 
        "Invalid type for comparison operator: unexpected " 
        ++ show invalidType ++ ", expected Int or Char."
    show (InvalidComparisonRight expect actual) =
        "Invalid type for comparison operator: unexpected " 
        ++ show actual ++ ", expected " ++ show expect ++ "."
    show (InvalidEquality expect actual) =
        "Invalid type for equality operator: unexpected " 
        ++ show actual ++ ", expected " ++ show expect ++ "."
    show (InvalidLogical invalidType) = 
        "Invalid type for logical operator: unexpected " 
        ++ show invalidType ++ ", expected bool."
    show BothSideAnyAssignment =
        "..."
    show (InvalidExit invalidType) =
        "Invalid type for exit: unexpected " 
        ++ show invalidType ++ ", expected Int."
    show (InvalidReturn validType invalidType) =
        "Invalid return type for return: unexpected "
        ++ show invalidType ++ ", expected " ++ show validType ++ "." 
