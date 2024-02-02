module WACC.Semantics.Error where

import WACC.Semantics.Structure

data OperandDirection = OperandLeft | OperandRight deriving Show

data WaccSemanticsErrorType
    = UndefinedIdentifier String
    | UndefinedFunction String
    | RedefinedIdentifier String
    | NotCallable String Type
    | TooManyArguments Int Int
    | TooFewArguments Int Int
    | InconsistentTypesInArray Type Type
    | IncompatibleAssignment Type Type

    | InvalidArray Type
    | InvalidIndex Type
    | InvalidPair Type

    | InvalidNot Type
    | InvalidNegate Type
    | InvalidLength Type
    | InvalidOrder Type
    | InvalidCharacter Type

    | InvalidMultiply OperandDirection Type
    | InvalidDivide OperandDirection Type
    | InvalidRemainder OperandDirection Type
    | InvalidAdd OperandDirection Type
    | InvalidSubtract OperandDirection Type

    | InvalidComparisonLeft Type
    | InvalidComparisonRight ComparisonType Type
    | InvalidEquality Type Type
    
    | InvalidAnd OperandDirection Type
    | InvalidOr OperandDirection Type
    deriving Show
