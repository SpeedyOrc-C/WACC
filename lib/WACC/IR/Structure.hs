module WACC.IR.Structure where

import qualified WACC.Semantics.Structure as SM

data Identifier
    = Identifier String Int
    | Temporary String Int
    deriving (Show)

instance Eq Identifier where
    Identifier _ a == Identifier _ b = a == b
    Temporary _ a == Temporary _ b = a == b
    _ == _ = False

instance Ord Identifier where
    Temporary _ a `compare` Temporary _ b = compare a b
    Identifier _ a `compare` Identifier _ b = compare a b
    Identifier _ a `compare` Temporary _ b = compare a b
    Temporary _ a `compare` Identifier _ b = compare a b

data Size = B1 | B2 | B4 | B8 deriving (Show, Eq, Ord)

class HasSize a where getSize :: a -> Size

instance HasSize SM.Type where
    getSize = \case
        SM.Bool -> B1
        SM.Char -> B1
        SM.Int -> B4
        SM.String -> B8
        SM.Array {} -> B8
        SM.Pair {} -> B8
        SM.Any -> error "Size is unknown for type Any."

sizeToInt :: Size -> Int
sizeToInt = \case B1 -> 1; B2 -> 2; B4 -> 4; B8 -> 8

data Statement
    = Label Identifier
    | Goto Identifier
    | Assign Size Identifier Expression
    | AssignIndirect Size Identifier Expression
    | Print Expression
    | Free Identifier
    deriving Show

data Scalar = Immediate Int | Variable Identifier deriving Show

data Expression
    = Scalar Scalar
    | Add Scalar Scalar
    deriving Show
