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
    deriving Show

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
        "IncompatibleAssignment: expected type " ++ show expectedType
        ++ " unexpected " ++ show actualType ++ "."
    show (IncompatibleArgument expectType actualType) = 
        "IncompatibleArgument: expected type " ++ show expectedType
        ++ " unexpected " ++ show actualType ++ "."
    show (RedefinedFunction function) = 
        "Illegal redeclaration of function " ++ function ++ "."
    show (RedefinedParameter identifier) =
        "Illegal redeclaration of parameter " ++ identifier ++ "."
        ++ " Please rename this parameter " ++ identifier ++ "."
    show (InvalidRead type) = 
        "Unexpected read type " ++ show type ++ ", expected char or int."
    show (InvalidFree type) =
        "Unexpected free type " ++ show type ++ ", expected pair or array."
    show ReturnInMain =
        "Unexpected return instruction in Main function."
    show (InvalidIfCondition type) = 
        "Invalid if condition type: unexcepted "
        ++ show type ++ ", expected bool."
    show (InvalidWhileCondition type) = 
        "Invalid while condition type: unexcepted "
        ++ show type ++ ", expected bool."
    show (InvalidArray type) = 
        "Invalid type: unexpected " ++ show type ++ ", expected array."
    show (InvalidIndex type) = 
        "Invalid index type: unexpected " ++ show type ++ ", expected int."
    show (InvalidPair type) = 
        "Invalid type: unexpected " ++ show type ++ ", expected pair."
    show (InvalidNot type) = 
        "Invalid type for '!': unexpected " ++ show type 
        ++ ", expected boolean."
    show (InvalidNegate type) = 
        "Invalid type for '-': unexpected " ++ show type 
        ++ ", expected int."
    show (InvalidLength type) = 
        "Invalid type for 'len': unexpected " ++ show type 
        ++ ", expected array."
    show (InvalidOrder type) = 
        "Invalid type for 'ord': unexpected " ++ show type 
        ++ ", expected char."
    show (InvalidCharacter type) = 
        "Invalid type for 'chr': unexpected " ++ show type 
        ++ ", expected int."
    show (InvalidArithmetic _ type) =
        "Invalid type for arithmetic operator: unexpected " 
        ++ show type ++ ", expected int."
    show (InvalidComparisonLeft type) = 
        "Invalid type for comparison operator: unexpected " 
        ++ show type ++ ", expected int or char."
    show (InvalidComparisonRight expect actual) =
        "Invalid type for comparison operator: unexpected " 
        ++ show actual ++ ", expected " ++ show expect ++ "."
    show (InvalidEquality expect actual) =
        "Invalid type for equality operator: unexpected " 
        ++ show actual ++ ", expected " ++ show expect ++ "."
    show (InvalidLogical direction type) = 
        "Invalid type for logical operator: unexpected " 
        ++ show type ++ ", expected bool."
