module WACC.Backend.X64.Unix.Generate where

import qualified Data.Map as M
import qualified Data.Set as S

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

data MemoryPool = MemoryPool {
    registers :: S.Set PhysicalRegister,
    registerPool :: M.Map PhysicalRegister Identifier,
    stackPool :: [StackSegment]
}

allocate :: MemoryPool -> Size -> (MemoryPool, MemoryLocation)
allocate = undefined

free :: MemoryPool -> MemoryLocation -> MemoryPool
free = undefined
