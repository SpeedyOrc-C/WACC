module WACC.IR.Structure where

import Data.Set (Set)
import Data.Map (Map)

import qualified WACC.Semantics.Structure as SM

data Program s = Program (Map String Int) [Function s]

data Function s = Function String [(Int, Size)] [s] deriving Show

data NoExpressionStatement
    = NE SingleStatement
    | If Scalar [NoExpressionStatement] [NoExpressionStatement]

    -- Cannot free the "free variables" inside the while loop
    -- as they might be referenced again.
    -- References might only be freed after the loop.
    | While Scalar [NoExpressionStatement] (Set Identifier)
    deriving Show

data NoControlFlowStatement
    = NCF SingleStatement
    | Label String
    | Goto String
    | GotoIfNot Scalar String

    ---- These directives are for compiler only and not compiled.
    | FreeVariable Identifier
    | WhileReference (Set Identifier)
    deriving Show

data SingleStatement
    = Assign Size Identifier Expression
    | AssignIndirect Size Identifier Expression
    | Return Scalar
    deriving Show

data Size = B1 | B2 | B4 | B8 deriving (Show, Eq, Ord)

data Expression
    = Scalar Scalar
    | Add Scalar Scalar

    | NewArray Size [Scalar]
    | SeekArrayElement Scalar Scalar

    | NewPair Scalar Scalar
    | SeekPairFirst Scalar
    | SeekPairSecond Scalar

    | Dereference Scalar

    | Call String [Scalar]
    deriving Show

data Scalar
    = Immediate Int
    | Variable Identifier
    | String Int
    deriving Show

data Identifier
    = Identifier String Int
    | Temporary String Int
    deriving Show

data IdentifierType

instance Eq Identifier where
    Identifier _ a == Identifier _ b = a == b
    Temporary _ a == Temporary _ b = a == b
    _ == _ = False

instance Ord Identifier where
    Temporary _ a `compare` Temporary _ b = compare a b
    Identifier _ a `compare` Identifier _ b = compare a b
    Identifier _ a `compare` Temporary _ b = compare a b
    Temporary _ a `compare` Identifier _ b = compare a b

class HasSize a where
    getSize :: a -> Size

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
