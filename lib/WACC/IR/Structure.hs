module WACC.IR.Structure where

import Data.Set (Set)
import Data.Map (Map)

import qualified WACC.Semantics.Structure as SM

data Program s = Program (Map String Int) [Function s] deriving Show

data Function s = Function String [(Identifier, Size)] [s] deriving Show

data NoExpressionStatement
    = NE SingleStatement
    | If Scalar [NoExpressionStatement] [NoExpressionStatement]

    -- Cannot free the "free variables" inside the while loop
    -- as they might be referenced again.
    -- References might only be freed after the loop.
    | While (Scalar, [SingleStatement]) [NoExpressionStatement] (Set Identifier)
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
    | Exit Scalar
    | Free Scalar

    | PrintBool Scalar
    | PrintInt Scalar
    | PrintChar Scalar
    | PrintString Scalar
    | PrintAddress Scalar
    | PrintLineBreak
    deriving Show

data Size = B1 | B2 | B4 | B8 deriving (Show, Eq, Ord)

data Expression
    = Scalar Scalar

    | Not Scalar
    | Negate Scalar
    | Length Scalar
    | Order Scalar
    | Character Scalar

    | Multiply Scalar Scalar
    | Divide Scalar Scalar
    | Remainder Scalar Scalar
    | Add Scalar Scalar
    | Subtract Scalar Scalar

    | Greater Size Scalar Scalar
    | GreaterEqual Size Scalar Scalar
    | Less Size Scalar Scalar
    | LessEqual Size Scalar Scalar

    | Equal Size Scalar Scalar
    | NotEqual Size Scalar Scalar

    | And Scalar Scalar
    | Or Scalar Scalar

    | NewArray Size [Scalar]
    | SeekArrayElement Size Scalar Scalar

    | NewPair (Size, Size) (Scalar, Scalar)
    | SeekPairFirst Scalar
    | SeekPairSecond Scalar

    | Dereference Scalar

    | Call Size String [(Size, Scalar)]
    | ReadInt
    | ReadChar
    deriving Show

data Scalar
    = Immediate Int
    | Variable Identifier
    | String Int
    deriving Show

data Identifier
    = Identifier String Int
    | Parameter String Int
    | Temporary String Int
    deriving Show

identifierNo :: Identifier -> Int
identifierNo = \case
    Identifier _ n -> n
    Parameter _ n -> n
    Temporary _ n -> n

data IdentifierType

instance Eq Identifier where
    (==) :: Identifier -> Identifier -> Bool
    a == b = identifierNo a == identifierNo b

instance Ord Identifier where
    compare :: Identifier -> Identifier -> Ordering
    compare a b = compare (identifierNo a) (identifierNo b)

class HasSize a where
    getSize :: a -> Size

instance HasSize SM.Type where
    getSize :: SM.Type -> Size
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
