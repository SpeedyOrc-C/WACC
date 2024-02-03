module WACC.Semantics.Error where

import WACC.Semantics.Structure

data OperandDirection = OperandLeft | OperandRight deriving Show

data WaccSemanticsErrorType
    = UndefinedIdentifier String
    | UndefinedFunction String
    | RedefinedIdentifier String
    | TooManyArguments Int Int
    | TooFewArguments Int Int
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
