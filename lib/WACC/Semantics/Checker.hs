module WACC.Semantics.Checker where

import Prelude hiding (error)
import Text.Parser (Range, parseString, Parsed (Parsed))
import qualified WACC.Syntax.Structure as Syntax
import qualified Data.Map as M
import WACC.Syntax.Parser (statement)
import WACC.Semantics.Error ( WaccSemanticsErrorType(..) )
import WACC.Semantics.Structure
    ( Expression(..), Type(..), Statement(Declare), Function, Program )
import WACC.Syntax.Validation (expressionRange)

type MappindStack = [String `M.Map` Type]
data SemanticError = SemanticError Range WaccSemanticsErrorType deriving Show
newtype CheckerState = CheckerState MappindStack

-- | Can the right type take the place of the left type?
(<|) :: Type -> Type -> Bool
Any <| _ = True
_ <| Any = True
String <| Array Char = True
String <| Array Any = True
Array a <| Array b = a <| b
Pair (a, b) <| Pair (c, d) = a <| c && b <| d
a <| b = a == b

lookUpIdentifierType :: MappindStack -> String -> Maybe Type
lookUpIdentifierType mappings name =
    case mappings of
        [] -> Nothing
        mapping:rest ->
            case M.lookup name mapping of
                Just t -> Just t
                Nothing -> lookUpIdentifierType rest name

fromSyntaxType :: Syntax.Type -> Type
fromSyntaxType = \case
    Syntax.Int {} -> Int
    Syntax.Bool {} -> Bool
    Syntax.Char {} -> Char
    Syntax.String {} -> String
    Syntax.Array t _ -> Array (fromSyntaxType t)
    Syntax.Pair Nothing _ -> Pair (Any, Any)
    Syntax.Pair (Just (a, b)) _ -> Pair (fromSyntaxType a, fromSyntaxType b)

-- | Debug use
d input = checkSemantics (CheckerState [M.fromList [
    ("foo", Callable [Int] String), ("x", Int)
    ]]) e
    where
    Right (Parsed _ e _) = parseString statement input

-- | Given that two types are compatible with each other,
--   find the common type between two types. This can find the type of an array.
common :: Type -> Type -> Type
common Any t = t
common t Any = t

common String (Array Char) = String
common (Array Char) String = String

common (Array Any) String = String
common String (Array Any) = String

common (Array a) (Array b) = Array (common a b)
common (Pair (a, b)) (Pair (c, d)) = Pair (common a c, common b d)

common a b = if a == b then a else undefined

-- computeType :: CheckerState -> Syntax.Expression -> SemanticError `Either` Type
-- computeType state = \case
--     Syntax.LiteralArray array _ -> do
--         types <- computeType state `mapM` array

--         let check [] = Right $ Array Any
--             check ((t, _):es) = check' t es

--             check' t [] = Right $ Array t
--             check' t ((t', e'):es) =
--                 let strongerType = common t t' in
--                 if t <| t' || t' <| t
--                     then check' strongerType es
--                     else Left $ SemanticError (expressionRange e') $
--                         InconsistentTypesInArray t t'

--         check (zip types array)

--     Syntax.LiteralPair (a, b) _ -> do
--         a' <- computeType state a
--         b' <- computeType state b
--         return $ Pair (a', b')

--     Syntax.FunctionCall (name, args) range -> do
--         case lookUpIdentifierType state name of

--             Just (Callable paramsType returnType) -> do
--                 argsType <- computeType state `mapM` args

--                 let check :: [(Type, Type, Syntax.Expression)] -> SemanticError `Either` Type
--                     check [] = case compare (length args) (length paramsType) of
--                         LT -> Left $ SemanticError range $
--                             TooFewArguments (length paramsType) (length argsType)
--                         EQ -> Right returnType
--                         GT -> Left $ SemanticError range $
--                             TooManyArguments (length paramsType) (length argsType)
--                     check ((paramType, argType, e):es) =
--                         if paramType <| argType
--                             then check es
--                             else Left $ SemanticError (expressionRange e) $
--                                 IncompatibleAssignment paramType argType

--                 check (zip3 paramsType argsType args)

--             Just t -> do
--                 Left $ SemanticError range $ NotCallable name t

--             Nothing ->
--                 Left $ SemanticError range $ UndefinedFunction name

--     _ -> Left undefined

class CheckSemantics syntaxTree result
    | syntaxTree -> result, result -> syntaxTree where
    {- |
    Given a state and a part of the syntax tree,
    return the new bindings and the tree passed the semantic check,
    Or some errors if the semantic check failed.
    -}
    checkSemantics :: CheckerState -> syntaxTree -> Either [SemanticError] result

instance CheckSemantics Syntax.Expression (Type, Expression) where
    checkSemantics state@(CheckerState mappings) = \case
        Syntax.LiteralInt x _ -> Right (Int, LiteralInt x)
        Syntax.LiteralBool x _ -> Right (Bool, LiteralBool x)
        Syntax.LiteralChar x _ -> Right (Char, LiteralChar x)
        Syntax.LiteralString x _ -> Right (String, LiteralString x)

        Syntax.Identifier name range ->
            case lookUpIdentifierType mappings name of
                Just t -> Right (t, Identifier name)
                Nothing -> Left [SemanticError range (UndefinedIdentifier name)]
        
        Syntax.LiteralArray array _ -> do
            typesAndExpressions <- checkSemantics state `mapM` array

            case zip (fst <$> typesAndExpressions) (expressionRange <$> array) of

                [] -> Right (Array Any, LiteralArray Any [])

                (firstType, _):restTypesAndRanges ->
                    check firstType restTypesAndRanges
                    where
                    check commonType ((t, range):rest) =
                        if t <| commonType || commonType <| t
                            then check (common commonType t) rest
                            else Left [SemanticError range $
                                       InconsistentTypesInArray commonType t]
                    check commonType [] = Right
                        (Array commonType, LiteralArray commonType expressions)
                        where expressions = snd <$> typesAndExpressions

        _ -> undefined

instance CheckSemantics Syntax.Statement ((String, Type), Statement) where
    checkSemantics state@(CheckerState mappings) =
        let mapping = head mappings in
        \case
        Syntax.Skip {} -> Left []

        Syntax.Declare (fromSyntaxType -> declaredType, name, value) range ->

            case lookUpIdentifierType [mapping] name of
            
                Just {} -> Left [SemanticError range $ RedefinedIdentifier name]
            
                Nothing ->
                    case checkSemantics state value of
            
                        Right (computedType, newValue) ->
                            if declaredType <| computedType

                                then Right ((name, declaredType),
                                            Declare declaredType name newValue)

                                else Left [SemanticError range $
                                            IncompatibleAssignment
                                                declaredType computedType]
            
                        Left x -> Left x
        
        _ -> undefined

instance CheckSemantics [Syntax.Statement] [Statement] where
    checkSemantics = undefined

instance CheckSemantics Syntax.Function Function where
    checkSemantics = undefined

instance CheckSemantics Syntax.Program Program where
    checkSemantics = undefined

