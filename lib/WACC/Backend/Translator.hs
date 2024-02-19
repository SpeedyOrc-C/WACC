import WACC.Semantics.Structure
import WACC.Backend.Structure

translator :: Expression -> Status -> AppendList Commands
translator expr status
    = case expr of
        