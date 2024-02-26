module WACC.Backend.StackPool where

import Control.Arrow

type StackPool a = [StackSegment a]

data StackSegment a = Seg Int a | Pit Int
    deriving (Show)

allocateStack :: Int -> a -> StackPool a -> (Int, StackPool a)
allocateStack = allocateStack' 0

allocateStack' :: Int -> Int -> a -> StackPool a -> (Int, StackPool a)
allocateStack' offset size name [] = (offset, [Seg size name])
allocateStack' offset size name (seg@(Seg size' _) : rest) =
    second (seg :)
        (allocateStack' (offset + size') size name rest)
allocateStack' offset size name (Pit p1 : Pit p2 : rest) =
    allocateStack' offset size name (Pit (p1 + p2) : rest)
allocateStack' offset size name [Pit {}] =
    allocateStack' offset size name []
allocateStack' offset size name (Pit pitSize : rest) =
    case compare size pitSize of
        LT -> (offset, Seg size name : Pit (pitSize - size) : rest)
        EQ -> (offset, Seg size name : rest)
        GT -> second (Pit pitSize :)
                (allocateStack' (offset + pitSize) size name rest)

freeStack :: Int -> StackPool a -> StackPool a
freeStack 0 (Seg size _ : rest) = Pit size : rest
freeStack 0 _ = error "Segmentation fault"
freeStack n (Pit size : rest) = Pit size : freeStack (n - size) rest
freeStack n (seg@(Seg size _) : rest) = seg : freeStack (n - size) rest
freeStack _ _ = error "Segmentation fault"
