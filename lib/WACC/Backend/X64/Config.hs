module WACC.Backend.X64.Config where

import Data.Set (Set)
import Data.Map (Map)
import Data.Sequence (Seq)

import qualified WACC.Backend.X64.Internal as Internal
import           WACC.Backend.X64.Structure

data Config = Config {
    parameterRegisters :: [PhysicalRegister],
    callerSaveRegisters :: Set PhysicalRegister,
    calleeSaveRegisters :: Set PhysicalRegister,
    rankRegister :: PhysicalRegister -> Int,
    availableRegisters :: Set PhysicalRegister,
    internalFunctions :: Map Internal.Function (Seq Instruction)
}
