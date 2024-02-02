module WACC.Semantics.Checker where

import Prelude hiding (error)
import Text.Parser (Range, parseString, Parsed (Parsed))
import qualified WACC.Syntax.Structure as Syntax
import qualified Data.Map as M
import Data.List ((\\))
import WACC.Syntax.Parser as Parser
import WACC.Semantics.Error ( WaccSemanticsErrorType(..), OperandDirection (OperandLeft, OperandRight) )
import WACC.Semantics.Structure
    ( Expression(..), Type(..), Statement(Declare), Function, Program, ComparisonType (..) )
import WACC.Syntax.Validation (expressionRange)
import Data.Functor ((<&>), void)

goDeeper :: CheckerState -> CheckerState
goDeeper state = state { mappingStack = M.empty : mappingStack state }

addIdentifier :: String -> Type -> CheckerState -> CheckerState
addIdentifier name t state = state {
    mappingStack =
        M.insert name t (head (mappingStack state)) :
        tail (mappingStack state)
}

lookUp :: CheckerState -> String -> Maybe Type
lookUp state name = case mappingStack state of
    [] -> Nothing
    mapping : rest -> case M.lookup name mapping of
        Just t -> Just t
        Nothing -> lookUp (state {mappingStack = rest}) name

lookUpInnermost :: CheckerState -> String -> Maybe Type
lookUpInnermost state =
    lookUp $ state { mappingStack = [head (mappingStack state)] }

data SemanticError = SemanticError Range WaccSemanticsErrorType deriving Show
data CheckerState = CheckerState {
    -- Is it inside a function? If so, what type does this function returns?
    functionContext :: Maybe Type,
    -- All functions' types of parameters and return value
    functionMapping :: String `M.Map` ([Type], Type),
    -- All variables' types
    mappingStack :: [String `M.Map` Type]
}

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
db input =
    check (CheckerState {
        functionContext = Nothing,
        functionMapping = M.fromList [
            ("f", ([Int, Char, String], Bool))
        ],
        mappingStack =
        [M.fromList [
            ("x", Int), ("y", Int),
            ("c", Char), ("d", Char),
            ("a", Array $ Array Int),
            ("p", Pair (Int, Int))
        ]]
    })
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

merge :: (a -> b -> c) -> Either [x] a -> Either [x] b -> Either [x] c
merge _ (Left errors1) (Left errors2) = Left $ errors1 ++ errors2
merge _ (Right {}) (Left errors) = Left errors
merge _ (Left errors) (Right {}) = Left errors
merge f (Right result1) (Right results2) = Right $ result1 `f` results2

mergeMany :: Foldable t => t (Either [x] a) -> Either [x] [a]
mergeMany xs = reverse <$> foldl (merge (flip (:))) (Right []) xs

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
        Syntax.LiteralPairNull {} -> Right (Pair (Any, Any), LiteralPairNull)

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

        Syntax.ArrayElement (array, index) _ -> do
            ((arrayType, array'), (indexType, index')) <-
                merge (,) (check state array) (check state index)

            if Int <| indexType then case arrayType of
                Array arrayElementType ->
                    Right (arrayElementType, ArrayElement array' index')

                _ -> Left [SemanticError (expressionRange array) $
                            InvalidArray arrayType]
            else
                Left [SemanticError (expressionRange index) $
                        InvalidIndex indexType]

        Syntax.LiteralPair (left, right) _ -> do
            ((leftType, left'), (rightType, right')) <- merge (,)
                (check state left) (check state right)

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

        Syntax.Less xy _ -> checkComparison xy Less
        Syntax.LessEqual xy _ -> checkComparison xy LessEqual
        Syntax.Greater xy _ -> checkComparison xy Greater
        Syntax.GreaterEqual xy _ -> checkComparison xy GreaterEqual

        Syntax.Equal xy range -> checkEquality xy range Equal
        Syntax.NotEqual xy range -> checkEquality xy range NotEqual

        Syntax.Not e range -> checkUnary e range (Bool, Bool) (Not, InvalidNot)
        Syntax.Negate e range -> checkUnary e range (Int, Int) (Negate, InvalidNegate)
        Syntax.Length e range -> checkUnary e range (Array Any, Int) (Length, InvalidLength)
        Syntax.Order e range -> checkUnary e range (Char, Int) (Order, InvalidOrder)
        Syntax.Character e range -> checkUnary e range (Int, Char) (Character, InvalidCharacter)

        Syntax.Multiply xy _ -> checkArithmetic xy Multiply
        Syntax.Divide xy _ -> checkArithmetic xy Divide
        Syntax.Remainder xy _ -> checkArithmetic xy Remainder
        Syntax.Add xy _ -> checkArithmetic xy Add
        Syntax.Subtract xy _ -> checkArithmetic xy Subtract

        Syntax.And xy _ -> checkLogical xy And
        Syntax.Or xy _ -> checkLogical xy Or

        Syntax.FunctionCall (name, arguments) range -> do
            case M.lookup name (functionMapping state) of
                Nothing -> Left [SemanticError range $ UndefinedFunction name]
                Just (parametersTypes, returnType) -> do
                    (unzip -> (types, arguments')) <- mergeMany $
                        check state <$> arguments

                    let parameterChecker expectedType (range', actualType) =
                            if expectedType <| actualType
                            then Right expectedType
                            else Left [SemanticError range' $
                                        IncompatibleAssignment expectedType actualType]

                    void $ mergeMany $ zipWith parameterChecker
                        parametersTypes
                        (zip (expressionRange <$> arguments) types)

                    Right (returnType, FunctionCall name arguments')
        where

        checkLogical (left, right) constructor = do
            ((leftType, left'), (rightType, right')) <-
                merge (,) (check state left) (check state right)

            case leftType of
                Bool -> case rightType of
                    Bool -> Right (Bool, constructor left' right')
                    _ -> Left [SemanticError (expressionRange right) $
                                InvalidLogical OperandRight rightType]
                _ -> Left [SemanticError (expressionRange left) $
                            InvalidLogical OperandLeft leftType]

        checkUnary expr range (inputType, outputType) (constructor, error) = do
            (exprType, e') <- check state expr
            if inputType <| exprType
                then Right (outputType, constructor e')
                else Left [SemanticError range $ error exprType]

        checkArithmetic (left, right) constructor = do
            ((leftType, left'), (rightType, right')) <-
                merge (,) (check state left) (check state right)

            case leftType of
                Int -> case rightType of
                    Int -> Right (Int, constructor left' right')
                    _ -> Left [SemanticError (expressionRange right) $
                                InvalidArithmetic OperandLeft rightType]
                _ -> Left [SemanticError (expressionRange left) $
                            InvalidArithmetic OperandRight leftType]

        checkComparison (left, right) constructor = do
            ((leftType, left'), (rightType, right')) <-
                merge (,) (check state left) (check state right)

            case leftType of
                Int -> case rightType of
                    Int -> Right (Bool, constructor CompareInt left' right')
                    _ -> Left [SemanticError (expressionRange right) $
                                InvalidComparisonRight CompareInt rightType]
                Char -> case rightType of
                    Char -> Right (Bool, constructor CompareChar left' right')
                    _ -> Left [SemanticError (expressionRange right) $
                                InvalidComparisonRight CompareChar rightType]
                _ -> Left [SemanticError (expressionRange left) $
                            InvalidComparisonLeft leftType]

        checkEquality (left, right) range constructor = do
            ((leftType, left'), (rightType, right')) <-
                merge (,) (check state left) (check state right)

            if leftType == rightType then
                Right (Bool, constructor leftType left' right')
            else
                Left [SemanticError range $ InvalidEquality leftType rightType]

instance CheckSemantics Syntax.Statement Statement where
    check state = \case
        Syntax.Declare (fromSyntaxType -> declaredType, name, value) range
            -> case check state value of
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

        _ -> undefined

instance CheckSemantics Syntax.Function Function where
    check state (Syntax.Function (fromSyntaxType -> returnType, name, parameters, body) _) = do
        undefined
        where

instance CheckSemantics Syntax.Program Program where
    check state (Syntax.Program (functions, body) _) = do
        undefined
        where

        stateBody = state' { functionContext = Nothing }

        state' = state {
            functionMapping = functionMapping',
            mappingStack = []
        }

        functionMapping' = M.fromList
            [(name, (fromSyntaxType <$> parametersTypes, fromSyntaxType returnType))
            | Syntax.Function (returnType, name, unzip -> (parametersTypes, _), _) _
            <- functions]


checkProgram
    :: CheckSemantics syntaxTree result
    => syntaxTree -> Either [SemanticError] result
checkProgram = check $ CheckerState undefined undefined undefined
