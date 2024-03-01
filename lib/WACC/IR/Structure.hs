module WACC.IR.Structure where

import Data.Set (Set)
import Data.Map (Map)

import qualified WACC.Semantics.Structure as SM

{- Define the IR program structure. -}
data Program s = Program (Map String Int) [Function s] deriving Show

{- Define the structure of a function in the IR. -}
data Function s = Function String [(Identifier, Size)] [s] deriving Show

{- Define statements that do not contain expressions directly. -}
data NoExpressionStatement
    = NE SingleStatement
    | If Scalar [NoExpressionStatement] [NoExpressionStatement]

    {- Cannot free the "free variables" inside the while loop
     as they might be referenced again.
     References might only be freed after the loop. -}
    | While (Scalar, [SingleStatement]) [NoExpressionStatement]
            (Set Identifier)
    deriving Show

{- Define statements without control flow constructs. -}
data NoControlFlowStatement
    = NCF SingleStatement
    | Label String
    | Goto String
    | GotoIf Scalar String
    | GotoIfNot Scalar String

    {- These directives are for compiler only and not compiled. -}
    | FreeVariable Identifier
    | WhileReference (Set Identifier)
    deriving Show

{- Define single statements. -}
data SingleStatement
    = Assign Size Identifier Expression
    | AssignIndirect Size Identifier Expression
    | Return Size Scalar
    | Exit Scalar
    | Free Scalar
    -- Stupid array has a length before the elements,
    -- but the address of the array is the first element.
    | FreeArray Scalar

    | PrintBool Scalar
    | PrintInt Scalar
    | PrintChar Scalar
    | PrintString Scalar
    | PrintAddress Scalar
    | PrintLineBreak
    deriving (Show, Eq)

{- Define different sizes. -}
data Size = B1 | B2 | B4 | B8 deriving (Show, Eq, Ord)

{- Define all kinds of expressions. -}
data Expression
    = Scalar Scalar

    | Not Scalar
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

    | Dereference Size Scalar

    | Call Size String [(Size, Scalar)]
    | ReadInt
    | ReadChar Scalar
    deriving (Show, Eq)

{- A scalar is an immediate value, a variable or a string. -}
data Scalar
    = Immediate Int
    | Variable Identifier
    | String Int
    deriving (Show, Eq)

{- Identifiers can be normal variable names, parameter names,
   or temporary names within the program. -}
data Identifier
    = Identifier String Int
    | Parameter String Int
    | Temporary String Int
    deriving Show

{- This is a function to extract the number part of an identifier,
   which can be used for comparison and ordering. -}
identifierNo :: Identifier -> Int
identifierNo = \case
    Identifier _ n -> n
    Parameter _ n -> n
    Temporary _ n -> n

data IdentifierType

{- Compare two identifiers according to their identifierNo. -}
instance Eq Identifier where
    (==) :: Identifier -> Identifier -> Bool
    a == b = identifierNo a == identifierNo b

{- Order identifiers according to their identifierNo. -}
instance Ord Identifier where
    compare :: Identifier -> Identifier -> Ordering
    compare a b = compare (identifierNo a) (identifierNo b)

class HasSize a where
    getSize :: a -> Size

{- Retrieve the size of different data types. -}
instance HasSize SM.Type where
    getSize :: SM.Type -> Size
    getSize = \case
        SM.Bool -> B1
        SM.Char -> B1
        SM.Int -> B4
        SM.String -> B8
        SM.Array {} -> B8
        SM.Pair {} -> B8
        -- will happen when left hand side is type of Any 
        -- and right hand side being []
        SM.Any -> B1

{- Convert the size to a corresponding number. -}
sizeToInt :: Size -> Int
sizeToInt = \case B1 -> 1; B2 -> 2; B4 -> 4; B8 -> 8
