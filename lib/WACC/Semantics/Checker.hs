module WACC.Semantics.Checker where

import Prelude hiding (error)
import Text.Parser (parseString, Parsed (Parsed))
import qualified WACC.Syntax.Structure as Syntax
import qualified Data.Map as M
import WACC.Syntax.Parser as Parser
import WACC.Semantics.Error
    ( WaccSemanticsErrorType(..), OperandDirection (..) )
import WACC.Semantics.Structure
    ( Expression(..), Type(..), Statement(..), Function, Program, ComparisonType (..) )
import WACC.Syntax.Validation (expressionRange)
import Data.Functor (void)
import WACC.Semantics.Utils

-- | Debug use
db input =
    check (CheckerState {
        functionContext = Just Char,
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

class CheckSemantics syntaxTree result
    | syntaxTree -> result, result -> syntaxTree where
    {- |
    Given a state and a part of the syntax tree,
    return the new bindings and the tree passed the semantic check,
    Or some errors if the semantic check failed.
    -}
    check :: CheckerState -> syntaxTree -> CheckerResult result

instance CheckSemantics Syntax.Expression (Type, Expression) where
    check state = \case
        Syntax.LiteralInt x _ -> Ok (Int, LiteralInt x)
        Syntax.LiteralBool x _ -> Ok (Bool, LiteralBool x)
        Syntax.LiteralChar x _ -> Ok (Char, LiteralChar x)
        Syntax.LiteralString x _ -> Ok (String, LiteralString x)
        Syntax.LiteralPairNull {} -> Ok (Pair (Any, Any), LiteralPairNull)

        Syntax.Identifier name range ->
            case lookUp state name of
                Just t -> Ok (t, Identifier name)
                Nothing -> Bad [SemanticError range (UndefinedIdentifier name)]

        Syntax.LiteralArray array _ -> do
            typesAndExpressions <- check state `mapM` array

            case zip (fst <$> typesAndExpressions) (expressionRange <$> array) of

                [] -> Ok (Array Any, LiteralArray Any [])

                (firstType, _):restTypesAndRanges ->
                    getCommonTypes firstType restTypesAndRanges
                    where
                    getCommonTypes commonType ((t, range):rest) =
                        if t <| commonType || commonType <| t
                        then getCommonTypes (common commonType t) rest
                        else Bad [SemanticError range $
                                    InconsistentTypesInArray commonType t]

                    getCommonTypes commonType [] = Ok
                        (Array commonType, LiteralArray commonType expressions)
                        where expressions = snd <$> typesAndExpressions

        Syntax.ArrayElement (array, index) _ -> do
            ((arrayType, array'), (indexType, index')) <-
                (,) <$> check state array <*> check state index

            if Int <| indexType then case arrayType of
                Array arrayElementType ->
                    Ok (arrayElementType, ArrayElement array' index')

                _ -> Bad [SemanticError (expressionRange array) $
                            InvalidArray arrayType]
            else
                Bad [SemanticError (expressionRange index) $
                        InvalidIndex indexType]

        Syntax.LiteralPair (left, right) _ -> do
            ((leftType, left'), (rightType, right')) <-
                (,) <$> check state left <*> check state right

            Ok (Pair (leftType, rightType),
                    LiteralPair (leftType, rightType) (left', right'))

        Syntax.PairFirst pair _ -> do
            (pairType, pair') <- check state pair

            case pairType of
                Pair (leftType, _) -> Ok (leftType, PairFirst pair')
                _ -> Bad [SemanticError (expressionRange pair) $
                            InvalidPair pairType]

        Syntax.PairSecond pair _ -> do
            (pairType, pair') <- check state pair

            case pairType of
                Pair (_, rightType) -> Ok (rightType, PairSecond pair')
                _ -> Bad [SemanticError (expressionRange pair) $
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
                Nothing -> Bad [SemanticError range $ UndefinedFunction name]
                Just (parametersTypes, returnType) -> do
                    (unzip -> (types, arguments')) <- mergeMany $
                        check state <$> arguments

                    let parameterChecker expectedType (range', actualType) =
                            if expectedType <| actualType
                            then Ok expectedType
                            else Bad [SemanticError range' $
                                        IncompatibleAssignment expectedType actualType]

                    void $ mergeMany $ zipWith parameterChecker
                        parametersTypes
                        (zip (expressionRange <$> arguments) types)

                    Ok (returnType, FunctionCall name arguments')
        where

        checkLogical (left, right) constructor = do
            ((leftType, left'), (rightType, right')) <-
                (,) <$> check state left <*> check state right

            case leftType of
                Bool -> case rightType of
                    Bool -> Ok (Bool, constructor left' right')
                    _ -> Bad [SemanticError (expressionRange right) $
                                InvalidLogical OperandRight rightType]
                _ -> Bad [SemanticError (expressionRange left) $
                            InvalidLogical OperandLeft leftType]

        checkUnary expr range (inputType, outputType) (constructor, error) = do
            (exprType, e') <- check state expr
            if inputType <| exprType
                then Ok (outputType, constructor e')
                else Bad [SemanticError range $ error exprType]

        checkArithmetic (left, right) constructor = do
            ((leftType, left'), (rightType, right')) <-
                (,) <$> check state left <*> check state right

            case leftType of
                Int -> case rightType of
                    Int -> Ok (Int, constructor left' right')
                    _ -> Bad [SemanticError (expressionRange right) $
                                InvalidArithmetic OperandLeft rightType]
                _ -> Bad [SemanticError (expressionRange left) $
                            InvalidArithmetic OperandRight leftType]

        checkComparison (left, right) constructor = do
            ((leftType, left'), (rightType, right')) <-
                (,) <$> check state left <*> check state right

            case leftType of
                Int -> case rightType of
                    Int -> Ok (Bool, constructor CompareInt left' right')
                    _ -> Bad [SemanticError (expressionRange right) $
                                InvalidComparisonRight CompareInt rightType]
                Char -> case rightType of
                    Char -> Ok (Bool, constructor CompareChar left' right')
                    _ -> Bad [SemanticError (expressionRange right) $
                                InvalidComparisonRight CompareChar rightType]
                _ -> Bad [SemanticError (expressionRange left) $
                            InvalidComparisonLeft leftType]

        checkEquality (left, right) range constructor = do
            ((leftType, left'), (rightType, right')) <-
                (,) <$> check state left <*> check state right

            if leftType == rightType then
                Ok (Bool, constructor leftType left' right')
            else
                Bad [SemanticError range $ InvalidEquality leftType rightType]

instance CheckSemantics Syntax.Statement Statement where
    check state = \case
        Syntax.Declare (fromSyntaxType -> declaredType, name, value) range
            -> case check state value of
            Ok (computedType, newValue) ->
                if declaredType <| computedType then
                    case lookUpInnermost state name of
                        Nothing -> Ok (Declare declaredType name newValue)
                        Just {} -> Bad [SemanticError range $ RedefinedIdentifier name]
                else
                    Bad [SemanticError range $
                            IncompatibleAssignment declaredType computedType]

            Bad x -> Bad x

        Syntax.Assign (left, right) range -> do
            ((leftType, left'), (rightType, right')) <-
                (,) <$> check state left <*> check state right

            if leftType <| rightType then
                Ok (Assign left' right')
            else
                Bad [SemanticError (expressionRange right) $
                        IncompatibleAssignment leftType rightType]

        Syntax.Read destination range -> do
            (destinationType, destination') <- check state destination
            if Int <| destinationType || Char <| destinationType
                then Ok (Read destination')
                else Bad [SemanticError range $ InvalidRead destinationType]

        Syntax.Free address range -> do
            (addressType, address') <- check state address
            if Array Any <| addressType || Pair (Any, Any) <| addressType
                then Ok (Free address')
                else Bad [SemanticError range $ InvalidFree addressType]

        Syntax.Return value range -> do
            (valueType, value') <- check state value
            case functionContext state of
                Just expectedType ->
                    if expectedType <| valueType
                        then Ok (Return value')
                        else Bad [SemanticError (expressionRange value) $
                                    InvalidReturn valueType]
                Nothing -> Bad [SemanticError range $ ReturnInMain]

        Syntax.Exit number range -> do
            (numberType, number') <- check state number
            if Int <| numberType
                then Ok (Exit number')
                else Bad [SemanticError range $ InvalidReturn numberType]

        Syntax.Print value _ -> do
            (valueType, value') <- check state value
            Ok (Print valueType value')

        Syntax.PrintLine value _ -> do
            (valueType, value') <- check state value
            Ok (PrintLine valueType value')

        Syntax.If (condition, thenBranch, elseBranch) _ -> do
            ((conditionType, condition'), thenBranch', elseBranch') <- (,,)
                <$> check state condition
                <*> check (goDeeper state) thenBranch
                <*> check (goDeeper state) elseBranch
            
            if Bool <| conditionType
                then Ok (If condition' thenBranch' elseBranch')
                else Bad [SemanticError (expressionRange condition) $
                            InvalidIfCondition conditionType]
        
        Syntax.While (condition, body) _ -> do
            ((conditionType, condition'), body') <- (,)
                <$> check state condition
                <*> check (goDeeper state) body
            
            if Bool <| conditionType
                then Ok (While condition' body')
                else Bad [SemanticError (expressionRange condition) $
                            InvalidWhileCondition conditionType]
        
        Syntax.Skip {} -> undefined

        Syntax.Scope statements _ -> do
            statements' <- check (goDeeper state) statements
            Ok (Scope statements')

instance CheckSemantics [Syntax.Statement] [Statement] where
    check state = \case
        [] -> Ok []

        (Syntax.Skip {}:ss) -> check state ss

        (s@(Syntax.Declare (fromSyntaxType -> declaredType, name, _) _):ss) ->
            let state' = addIdentifier name declaredType state in
            (:) <$> check state s <*> check state' ss

        (s : ss) -> (:) <$> check state s <*> check state ss

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
    => syntaxTree -> CheckerResult result
checkProgram = check $ CheckerState undefined undefined undefined
