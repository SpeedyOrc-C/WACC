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
    | InvalidReturn Type
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
    | InvalidArithmetic OperandDirection Type
    | InvalidComparisonLeft Type
    | InvalidComparisonRight ComparisonType Type
    | InvalidEquality Type Type
    | InvalidLogical OperandDirection Type

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
        ++ ", expected char or int."
    show (InvalidFree invalidType) =
        "Unexpected free type " ++ show invalidType 
        ++ ", expected pair or array."
    show ReturnInMain =
        "Unexpected return instruction in Main function."
    show (InvalidIfCondition invalidType) = 
        "Invalid if condition type: unexcepted "
        ++ show invalidType ++ ", expected bool."
    show (InvalidWhileCondition invalidType) = 
        "Invalid while condition type: unexcepted "
        ++ show invalidType ++ ", expected bool."
    show (InvalidArray invalidType) = 
        "Invalid type: unexpected " ++ show invalidType ++ ", expected array."
    show (InvalidIndex invalidType) = 
        "Invalid index type: unexpected " ++ show invalidType
        ++ ", expected int."
    show (InvalidPair invalidType) = 
        "Invalid type: unexpected " ++ show invalidType ++ ", expected pair."
    show (InvalidNot invalidType) = 
        "Invalid type for '!': unexpected " ++ show invalidType 
        ++ ", expected boolean."
    show (InvalidNegate invalidType) = 
        "Invalid type for '-': unexpected " ++ show invalidType
        ++ ", expected int."
    show (InvalidLength invalidType) = 
        "Invalid type for 'len': unexpected " ++ show invalidType
        ++ ", expected array."
    show (InvalidOrder invalidType) = 
        "Invalid type for 'ord': unexpected " ++ show invalidType 
        ++ ", expected char."
    show (InvalidCharacter invalidType) = 
        "Invalid type for 'chr': unexpected " ++ show invalidType
        ++ ", expected int."
    show (InvalidArithmetic _ invalidType) =
        "Invalid type for arithmetic operator: unexpected " 
        ++ show invalidType ++ ", expected int."
    show (InvalidComparisonLeft invalidType) = 
        "Invalid type for comparison operator: unexpected " 
        ++ show invalidType ++ ", expected int or char."
    show (InvalidComparisonRight expect actual) =
        "Invalid type for comparison operator: unexpected " 
        ++ show actual ++ ", expected " ++ show expect ++ "."
    show (InvalidEquality expect actual) =
        "Invalid type for equality operator: unexpected " 
        ++ show actual ++ ", expected " ++ show expect ++ "."
    show (InvalidLogical direction invalidType) = 
        "Invalid type for logical operator: unexpected " 
        ++ show invalidType ++ ", expected bool."
