module WACC.Backend.X64.CheckAddress where
import qualified Data.Set as S
import WACC.IR.Structure

class CheckAddress a where
    check :: a -> S.Set Identifier

instance CheckAddress a => CheckAddress [a] where
    check :: CheckAddress a => [a] -> S.Set Identifier
    check = S.unions . map check

instance CheckAddress (Program Statement) where
    check :: Program -> S.Set Identifier
    check (Program _ fs) =
        check (S.toList fs) `S.union` check ss

instance CheckAddress Function where
    check :: Function -> S.Set Identifier
    check (Function _ _ _ ss) = check ss

instance CheckAddress Expression where
    check :: Expression -> S.Set Identifier
    check = \case
        (Address t ident) -> S.singleton ident
        
-- instance HasLiteralStrings Statement where
--     getLiteralStrings :: Statement -> S.Set String
--     getLiteralStrings = \case
--         Declare _ _ e -> getLiteralStrings e
--         Assign _ _ e -> getLiteralStrings e
--         Return _ e -> getLiteralStrings e
--         Print String e -> getLiteralStrings e
--         PrintLine String e -> getLiteralStrings e
--         Scope (Block ss') -> getLiteralStrings ss'
--         If _ (Block ss') (Block ss'') ->
--             getLiteralStrings ss' `S.union` getLiteralStrings ss''
--         While _ (Block ss') -> getLiteralStrings ss'
--         _ -> S.empty