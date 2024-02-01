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

    | InvalidGreaterChar OperandDirection Type
    | InvalidGreaterEqualChar OperandDirection Type
    | InvalidLessChar OperandDirection Type
    | InvalidLessEqualChar OperandDirection Type

    | InvalidGreaterInt OperandDirection Type
    | InvalidGreaterEqualInt OperandDirection Type
    | InvalidLessInt OperandDirection Type
    | InvalidLessEqualInt OperandDirection Type
    
    | InvalidEqual Type Type
    | InvalidNotEqual Type Type
    | InvalidAnd OperandDirection Type
    | InvalidOr OperandDirection Type
    deriving Show
