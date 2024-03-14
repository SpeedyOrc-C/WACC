module WACC.IR.Structure where

import Data.Set (Set)
import qualified Data.Map as M

import qualified WACC.Semantics.Structure as SM
import           Control.Monad.Trans.State.Lazy


turnStructures :: String `M.Map` Size -> SM.Structure -> Structure
turnStructures preStruct (SM.Structure name fields) =
    Structure name size fields'
    where
        (size, fields') = turnStructures' 0 B1 fields
        roundSizeStruct :: Int -> Int -> Int
        roundSizeStruct y x
            = if x `mod` y == 0
                then y
                else roundSizeStruct (y `div` 2) x
        floorUpSize :: Int -> Size -> Int
        floorUpSize offset s
            = case r of
                0 -> n * size'
                _ -> (n + 1) * size'
            where
                size'  = case s of
                    (B k) -> roundSizeStruct 8 k
                    _     -> sizeToInt s
                (n, r) = offset `divMod` size'

        turnStructures' ::
            Int
            -> Size
            -> [(String, SM.Type)]
            -> (Int, [(String,(Int, Size))])
        turnStructures' offset s []
            = (totS, [])
                where
                    totS = floorUpSize offset s
        turnStructures' offset maximumSize ((f, SM.Struct name' _):fs) =
            case ft of
                (Just ft') -> (offset'', (f,(offset', ft')):xs)
                    where
                        offset' = floorUpSize offset ft'
                        maximumSize'
                            = if sizeToInt maximumSize > sizeToInt ft'
                                then maximumSize
                                else ft'
                        (offset'', xs)
                            = turnStructures' (offset' + sizeToInt ft')
                            maximumSize' fs
                Nothing  -> error $ "should never occur (struct field with type \
                    \ struct must be defined in previous struct)" ++ show (length preStruct)
            where
                ft  = M.lookup name' preStruct

        turnStructures' offset maximumSize ((f, ft):fs)
            = (offset'', (f,(offset', ft')):xs)
                where
                    ft' = getSize ft
                    offset' = floorUpSize offset ft'
                    maximumSize'
                        = if sizeToInt maximumSize > sizeToInt ft'
                            then maximumSize
                            else ft'
                    (offset'', xs)
                        = turnStructures' (offset' + sizeToInt ft')
                            maximumSize' fs

initialStructures :: String `M.Map` Size -> [SM.Structure] -> [(String,Structure)]
initialStructures _ [] = []
initialStructures mmap (x:xs)
    = (name, x') :initialStructures map' xs
        where
            x'@(Structure name size' _) = turnStructures mmap x
            map' = M.insert name (B size') mmap

data FlattenerState = FlattenerState {
    mappingStack :: [String `M.Map` Identifier],
    variableCounter :: Int,
    dataSegment :: String `M.Map` Int,
    structures :: String `M.Map` Structure
} deriving Show

initialState :: String `M.Map` Int -> [SM.Structure] -> FlattenerState
initialState ds structs = FlattenerState {
    mappingStack = [],
    variableCounter = M.size ds + 1,
    dataSegment = ds,
    structures = M.fromList $ initialStructures M.empty structs
}

data Program s = Program (M.Map String Int) [Function s] deriving Show

data Function s = Function String [(Identifier, Size)] [s] deriving Show

data Structure = Structure String Int [(String,(Int, Size))] deriving Show

data WhileInfo = WhileInfo {
    possibleFreeVars :: Set Identifier,
    referencedFreeVariables :: Set Identifier
} deriving Show

data NoExpressionStatement
    = NE SingleStatement
    | If Scalar [NoExpressionStatement] [NoExpressionStatement]

    -- Cannot free the "free variables" inside the while loop
    -- as they might be referenced again.
    -- References might only be freed after the loop.
    | While (Scalar, [SingleStatement]) [NoExpressionStatement]
            WhileInfo
    deriving Show

data NoControlFlowStatement
    = NCF SingleStatement
    | Label String
    | Goto String
    | GotoIf Scalar String
    | GotoIfNot Scalar String

    -- These directives are for compiler only and not compiled.
    | FreeVariable Identifier
    | WhileReference (Set Identifier)
    deriving Show

data SingleStatement
    = Assign Size Identifier Expression
    | FunctionReturnStruct Size Identifier String [(Size, Scalar)]
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

data Size = B1 | B2 | B4 | B8 | B Int deriving (Show, Eq, Ord)

data Expression
    = Scalar Scalar

    | NewStruct
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
    | GetField Int Scalar

    | And Scalar Scalar
    | Or Scalar Scalar

    | NewArray Size [Scalar]
    | SeekArrayElement Size Scalar Scalar

    | NewPair (Size, Size) (Scalar, Scalar)
    | SeekPairFirst Scalar
    | SeekPairSecond Scalar

    | Reference Scalar
    | Dereference Size Scalar

    | Call Size String [(Size, Scalar)]
    | ReadInt
    | ReadChar Scalar
    deriving (Show, Eq)

data Scalar
    = Immediate Int
    | Variable Identifier
    | String Int
    deriving (Show, Eq)

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

getSize' :: SM.Type -> State FlattenerState Size
getSize' = \case
    SM.Struct name _ -> do
        structures <- gets structures
        case structures M.!? name of
            Just (Structure _ size _) -> return $ B size
            _ -> error "(should never happen) cannot find structure"

    t -> return $ getSize t

instance HasSize SM.Type where
    getSize :: SM.Type -> Size
    getSize = \case
        SM.Bool -> B1
        SM.Char -> B1
        SM.Int ->  B4
        SM.String -> B8
        SM.Array {} -> B8
        SM.Pair {} -> B8
        -- will happen when left hand side is type of Any
        -- and right hand side being []
        SM.Any -> B1
        SM.Struct {} -> error "should not use getSize' to get the size of the struct"
        SM.RefType {} -> B8


sizeToInt :: Size -> Int
sizeToInt = \case B1 -> 1; B2 -> 2; B4 -> 4; B8 -> 8; (B i) -> i
