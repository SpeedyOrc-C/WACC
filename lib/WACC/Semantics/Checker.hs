module WACC.Semantics.Checker where

import Prelude hiding (error)
import Text.Parser (Range, parseString, Parsed (Parsed))
import qualified WACC.Syntax.Structure as Syntax
import qualified Data.Map as M
import WACC.Syntax.Parser as Parser
import WACC.Semantics.Error ( WaccSemanticsErrorType(..) )
import WACC.Semantics.Structure
    ( Expression(..), Type(..), Statement(Declare), Function, Program )
import WACC.Syntax.Validation (expressionRange)

type MappindStack = [String `M.Map` Type]

goDeeper :: CheckerState -> CheckerState
goDeeper (CheckerState stack) = CheckerState $ M.empty : stack

addIdentifier :: String -> Type -> CheckerState -> CheckerState
addIdentifier name t (CheckerState stack) = CheckerState $ M.insert name t (head stack) : tail stack

lookUp :: CheckerState -> String -> Maybe Type
lookUp (CheckerState mappings) name =
    case mappings of
        [] -> Nothing
        mapping:rest ->
            case M.lookup name mapping of
                Just t -> Just t
                Nothing -> lookUp (CheckerState rest) name

lookUpInnermost :: CheckerState -> String -> Maybe Type
lookUpInnermost (CheckerState mappings) = lookUp (CheckerState [head mappings])

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
d input =
    check (CheckerState [M.fromList [
        ("f", Callable [Int] String),
        ("i", Int),
        ("a", Array $ Array Int),
        ("p", Pair (Int, Int))
    ]])
    e
    where
    Right (Parsed _ e _) = parseString Parser.statements input

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

merge :: (a -> b -> c) -> Either [x] a -> Either [x] b -> Either [x] c
merge _ (Left errors1) (Left errors2) = Left $ errors1 ++ errors2
merge _ (Right {}) (Left errors) = Left errors
merge _ (Left errors) (Right {}) = Left errors
merge f (Right result1) (Right results2) = Right $ result1 `f` results2

class CheckSemantics syntaxTree result
    | syntaxTree -> result, result -> syntaxTree where
    {- |
    Given a state and a part of the syntax tree,
    return the new bindings and the tree passed the semantic check,
    Or some errors if the semantic check failed.
    -}
    check :: CheckerState -> syntaxTree -> Either [SemanticError] result

instance CheckSemantics Syntax.Expression (Type, Expression) where
    check state = \case
        Syntax.LiteralInt x _ -> Right (Int, LiteralInt x)
        Syntax.LiteralBool x _ -> Right (Bool, LiteralBool x)
        Syntax.LiteralChar x _ -> Right (Char, LiteralChar x)
        Syntax.LiteralString x _ -> Right (String, LiteralString x)

        Syntax.Identifier name range ->
            case lookUp state name of
                Just t -> Right (t, Identifier name)
                Nothing -> Left [SemanticError range (UndefinedIdentifier name)]

        Syntax.LiteralArray array _ -> do
            typesAndExpressions <- check state `mapM` array

            case zip (fst <$> typesAndExpressions) (expressionRange <$> array) of

                [] -> Right (Array Any, LiteralArray Any [])

                (firstType, _):restTypesAndRanges ->
                    getCommonTypes firstType restTypesAndRanges
                    where
                    getCommonTypes commonType ((t, range):rest) =
                        if t <| commonType || commonType <| t
                            then getCommonTypes (common commonType t) rest
                            else Left [SemanticError range $
                                       InconsistentTypesInArray commonType t]
                    getCommonTypes commonType [] = Right
                        (Array commonType, LiteralArray commonType expressions)
                        where expressions = snd <$> typesAndExpressions

        Syntax.ArrayElement (array, index) _ ->
            case merge (,)
                (check state array) (check state index) of
                Left x -> Left x

                Right ((arrayType, array'), (indexType, index')) ->
                    if Int <| indexType

                    then case arrayType of
                        Array arrayElementType ->
                            Right (arrayElementType, ArrayElement array' index')
                
                        _ -> Left [SemanticError (expressionRange array) $
                                    InvalidArray arrayType]
                
                    else Left [SemanticError (expressionRange index) $
                                InvalidIndex indexType]

        Syntax.LiteralPair (left, right) _ ->
            case merge (,)
                (check state left) (check state right) of
                Left x -> Left x

                Right ((leftType, left'), (rightType, right')) -> 
                    Right (Pair (leftType, rightType),
                            LiteralPair (leftType, rightType) (left', right'))

        Syntax.PairFirst pair _ -> do
            (pairType, pair') <- check state pair

            case pairType of
                Pair (leftType, _) -> Right (leftType, PairFirst pair')
                _ -> Left [SemanticError (expressionRange pair) $
                            InvalidPair pairType]

        Syntax.PairSecond pair _ -> do
            (pairType, pair') <- check state pair

            case pairType of
                Pair (_, rightType) -> Right (rightType, PairSecond pair')
                _ -> Left [SemanticError (expressionRange pair) $
                            InvalidPair pairType]
        
        Syntax.Equal (left, right) range -> do
            ((leftType, left'), (rightType, right')) <-
                merge (,) (check state left) (check state right)
            
            if leftType == rightType then
                Right (Bool, Equal leftType left' right')
            else
                Left [SemanticError range $ InvalidEqual leftType rightType]

        _ -> undefined

instance CheckSemantics Syntax.Statement Statement where
    check state = \case
        Syntax.Declare (fromSyntaxType -> declaredType, name, value) range ->
            case check state value of
                Right (computedType, newValue) ->
                    if declaredType <| computedType then
                        case lookUpInnermost state name of
                            Nothing -> Right (Declare declaredType name newValue)
                            Just {} -> Left [SemanticError range $ RedefinedIdentifier name]
                    else
                        Left [SemanticError range $
                                IncompatibleAssignment declaredType computedType]

                Left x -> Left x

        _ -> undefined

instance CheckSemantics [Syntax.Statement] [Statement] where
    check state = \case
        (s@(Syntax.Declare (fromSyntaxType -> declaredType, name, _) _):ss) -> do
            let state' = addIdentifier name declaredType state
            merge (:) (check state s) (check state' ss)

        (s@(Syntax.Assign {}):ss) ->
            merge (:) (check state s) (check state ss)
        
        (Syntax.Skip {}:ss) -> check state ss

        [] -> Right []

instance CheckSemantics Syntax.Function Function where
    check = undefined

instance CheckSemantics Syntax.Program Program where
    check = undefined

