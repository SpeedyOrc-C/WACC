{-# LANGUAGE RecordWildCards #-}
module WACC.Backend.X64.Unix.Generate where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Sequence
import Data.Sequence((|>), (<|), (><), Seq((:<|)))

import qualified WACC.IR.Structure as IR
import           WACC.IR.Structure (Size(..), Identifier)
import           WACC.Backend.X64.Structure

data MemoryLocation
    = AtRegister PhysicalRegister
    | AtStack Int Size
    | AtParameterStack Int Size

data StackSegment
    = StackSegment Size Identifier
    | Pit Int

type StackSegments = Sequence.Seq StackSegment

data MemoryPool = MemoryPool {
    registers :: S.Set PhysicalRegister,
    stackPool :: StackSegments,
    memoryMap :: M.Map Identifier MemoryLocation
}

allocate :: MemoryPool -> Size -> Identifier -> (MemoryPool, MemoryLocation)
allocate mp@MemoryPool{..} size ident
    | S.size registers == 0 = (mp{stackPool = stackPool', 
        memoryMap = M.insert ident location memoryMap}, location)
    | otherwise = (mp{registers = registers',
        memoryMap = M.insert ident (AtRegister reg) memoryMap}, AtRegister reg)
    where
        reg = S.elemAt 0 registers
        registers' = S.deleteAt 0 registers
        (stackPool', location) = addNewStack stackPool Sequence.empty 0
        addNewStack :: StackSegments -> StackSegments -> 
            Int -> (StackSegments, MemoryLocation)
        addNewStack xs ys offset
            | null xs   = (ys Sequence.|> StackSegment size ident, AtStack offset size)
            | otherwise =
                case x' of
                    (StackSegment n _) -> addNewStack xs' (ys |> x') (offset + IR.sizeToInt n)
                    (Pit _)            -> stackNextAddress xs ys (IR.sizeToInt size)
            where
                (x' :<| xs') = xs
                stackNextAddress :: StackSegments -> StackSegments -> Int ->
                    (StackSegments, MemoryLocation)
                stackNextAddress xs ys 0
                    = (ys >< (StackSegment size ident <| xs), AtStack offset size)
                stackNextAddress xs ys k
                    |null xs = addNewStack xs ys (offset + IR.sizeToInt size - k)
                    |otherwise = case x'' of
                        StackSegment n' _ -> addNewStack xs' (ys |> x'') (offset + IR.sizeToInt size - k + IR.sizeToInt n')
                        Pit n' -> 
                            if k <  n' then
                            (ys >< (StackSegment size ident <| (Pit (n' - k) <|
                                xs')), AtStack offset size)
                            else
                                stackNextAddress xs' (ys |> x'') (k - n')
                        where 
                            (x'' :<| xs') = xs



free :: MemoryPool -> Identifier -> MemoryPool
free mp@MemoryPool{..} ident 
    = case location of
        AtRegister reg -> mp{registers = S.insert reg registers, memoryMap = memoryMap'}
        AtStack offset _ -> mp{memoryMap = memoryMap', stackPool = deleteStack stackPool}
        _ -> error "want to free parameter"
    where
        memoryMap' = M.delete ident memoryMap
        location = memoryMap M.! ident
        deleteStack :: StackSegments -> StackSegments
        deleteStack stack
            = case index of 
                Nothing -> error "unfound identifier free" 
                (Just i) -> xs >< (Pit (IR.sizeToInt size) <| ys)
                    where 
                        (xs, (StackSegment size _) :<| ys) = Sequence.splitAt i stack
            where 
                index = Sequence.findIndexL findIdent stack
                findIdent :: StackSegment -> Bool
                findIdent (StackSegment _ ident) = True
                findIdent _ = False
            
