module WACC.Semantics.Checker where

import Prelude hiding (error)
import Text.Parser (parseString, Parsed (Parsed))
import qualified WACC.Syntax.Structure as Syntax
import qualified Data.Map as M
import WACC.Syntax.Parser as Parser
import WACC.Semantics.Error
    ( WaccSemanticsErrorType(..), OperandDirection (..) )
import WACC.Semantics.Structure
    ( Expression(..), Type(..), Statement(..), Function(..), Program(..), ComparisonType (..) )
import WACC.Syntax.Validation (expressionRange)
import WACC.Semantics.Utils
import Data.List ((\\))
import WACC.Syntax.Structure (Name(..))

-- | Debug use
db input =
    check (CheckerState {
        functionContext = Nothing,
        functionMapping = M.fromList [
            -- ("f", ([Int, Char, String], Bool))
        ],
        mappingStack =
        [M.fromList [
            -- ("x", Int), ("y", Int),
            -- ("c", Char), ("d", Char),
            -- ("a", Array $ Array Int),
            -- ("p", Pair (Int, Int))
        ]]
    })
    e
    where
    Right (Parsed _ e _) = parseString Parser.function input

class CheckSemantics syntaxTree result
    | syntaxTree -> result, result -> syntaxTree where
    {- |
    Given a state and a part of the syntax tree,
    return the new bindings and the tree passed the semantic check,
    Or some errors if the semantic check failed.
    -}
    check :: CheckerState -> syntaxTree -> LogEither SemanticError result

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
                Nothing -> Log [SemanticError range (UndefinedIdentifier name)]

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
                        else Log [SemanticError range $
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

                _ -> Log [SemanticError (expressionRange array) $
                            InvalidArray arrayType]
            else
                Log [SemanticError (expressionRange index) $
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
                _ -> Log [SemanticError (expressionRange pair) $
                            InvalidPair pairType]

        Syntax.PairSecond pair _ -> do
            (pairType, pair') <- check state pair

            case pairType of
                Pair (_, rightType) -> Ok (rightType, PairSecond pair')
                _ -> Log [SemanticError (expressionRange pair) $
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

        Syntax.FunctionCall (name, args) range -> do
            case M.lookup name (functionMapping state) of
                Nothing -> Log [SemanticError range $ UndefinedFunction name]
                Just (paramsTypes, returnType) -> do
                    (unzip -> (argsTypes, args')) <- check state `traverse` args

                    let checkParamsArgsTypes paramType argType range' =
                            if paramType <| argType
                            then Ok paramType
                            else Log [SemanticError range' $
                                        IncompatibleArgument paramType argType]

                    sequence_ $ zipWith3 checkParamsArgsTypes
                        paramsTypes
                        argsTypes
                        (expressionRange <$> args)

                    case compare (length args) (length paramsTypes) of
                        EQ -> Ok (returnType, FunctionCall name args')
                        GT -> Log [SemanticError range $ TooManyArguments
                                    (length paramsTypes) (length args)]
                        LT -> Log [SemanticError range $ TooFewArguments
                                    (length paramsTypes) (length args)]

        where

        checkLogical (left, right) constructor = do
            ((leftType, left'), (rightType, right')) <-
                (,) <$> check state left <*> check state right

            case leftType of
                Bool -> case rightType of
                    Bool -> Ok (Bool, constructor left' right')
                    _ -> Log [SemanticError (expressionRange right) $
                                InvalidLogical OperandRight rightType]
                _ -> Log [SemanticError (expressionRange left) $
                            InvalidLogical OperandLeft leftType]

        checkUnary expr range (inputType, outputType) (constructor, error) = do
            (exprType, e') <- check state expr
            if inputType <| exprType
                then Ok (outputType, constructor e')
                else Log [SemanticError range $ error exprType]

        checkArithmetic (left, right) constructor = do
            ((leftType, left'), (rightType, right')) <-
                (,) <$> check state left <*> check state right

            case leftType of
                Int -> case rightType of
                    Int -> Ok (Int, constructor left' right')
                    _ -> Log [SemanticError (expressionRange right) $
                                InvalidArithmetic OperandRight rightType]
                _ -> Log [SemanticError (expressionRange left) $
                            InvalidArithmetic OperandLeft leftType]

        checkComparison (left, right) constructor = do
            ((leftType, left'), (rightType, right')) <-
                (,) <$> check state left <*> check state right

            case leftType of
                Int -> case rightType of
                    Int -> Ok (Bool, constructor CompareInt left' right')
                    _ -> Log [SemanticError (expressionRange right) $
                                InvalidComparisonRight CompareInt rightType]
                Char -> case rightType of
                    Char -> Ok (Bool, constructor CompareChar left' right')
                    _ -> Log [SemanticError (expressionRange right) $
                                InvalidComparisonRight CompareChar rightType]
                _ -> Log [SemanticError (expressionRange left) $
                            InvalidComparisonLeft leftType]

        checkEquality (left, right) range constructor = do
            ((leftType, left'), (rightType, right')) <-
                (,) <$> check state left <*> check state right

            if leftType == rightType then
                Ok (Bool, constructor leftType left' right')
            else
                Log [SemanticError range $ InvalidEquality leftType rightType]

instance CheckSemantics Syntax.Statement Statement where
    check state = \case
        Syntax.Declare (fromSyntaxType -> declaredType, name, value) range
            -> case check state value of
            Ok (computedType, newValue) ->
                if declaredType <| computedType then
                    case lookUpInnermost state name of
                        Nothing -> Ok (Declare declaredType name newValue)
                        Just {} -> Log [SemanticError range $ RedefinedIdentifier name]
                else
                    Log [SemanticError range $
                            IncompatibleAssignment declaredType computedType]

            Log x -> Log x

        Syntax.Assign (left, right) range -> do
            ((leftType, left'), (rightType, right')) <-
                (,) <$> check state left <*> check state right

            if leftType <| rightType then
                Ok (Assign left' right')
            else
                Log [SemanticError (expressionRange right) $
                        IncompatibleAssignment leftType rightType]

        Syntax.Read destination range -> do
            (destinationType, destination') <- check state destination
            if Int <| destinationType || Char <| destinationType
                then Ok (Read destination')
                else Log [SemanticError range $ InvalidRead destinationType]

        Syntax.Free address range -> do
            (addressType, address') <- check state address
            if Array Any <| addressType || Pair (Any, Any) <| addressType
                then Ok (Free address')
                else Log [SemanticError range $ InvalidFree addressType]

        Syntax.Return value range -> do
            (valueType, value') <- check state value
            case functionContext state of
                Just expectedType ->
                    if expectedType <| valueType
                        then Ok (Return value')
                        else Log [SemanticError (expressionRange value) $
                                    InvalidReturn valueType]
                Nothing -> Log [SemanticError range $ ReturnInMain]

        Syntax.Exit number range -> do
            (numberType, number') <- check state number
            if Int <| numberType
                then Ok (Exit number')
                else Log [SemanticError range $ InvalidReturn numberType]

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
                else Log [SemanticError (expressionRange condition) $
                            InvalidIfCondition conditionType]

        Syntax.While (condition, body) _ -> do
            ((conditionType, condition'), body') <- (,)
                <$> check state condition
                <*> check (goDeeper state) body

            if Bool <| conditionType
                then Ok (While condition' body')
                else Log [SemanticError (expressionRange condition) $
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

-- | If no repetition found, which is good, nothing happens.
--   Otherwise returns the repeated entries.
findRepetition :: (Ord k, Eq a) => [(k, a)] -> (Maybe [(k, a)], [(k, a)])
findRepetition entries =
    if length entries == length entriesNoRepeat
        then (Nothing, entriesNoRepeat)
        else (Just (entries \\ entriesNoRepeat), entriesNoRepeat)
    where
    entriesNoRepeat = M.toList (M.fromList entries)

instance CheckSemantics Syntax.Function Function where
    check state (Syntax.Function
                    (fromSyntaxType -> returnType,
                    Name functionName _,
                    params,
                    body) _) =
        let
        paramsWithRange = [(param, (range, fromSyntaxType t))
                            | (Name param range, t) <- params]

        (maybeParamsRepeated, params') = findRepetition paramsWithRange

        parameterCheck = case maybeParamsRepeated of
            Just paramsRepeated -> Log [SemanticError range $
                RedefinedParameter nameRepeated
                | (nameRepeated, (range, _)) <- paramsRepeated]
            Nothing -> Ok ()

        paramsMappingLayer = [(param, t) | (param, (_, t)) <- params']
        in do
        parameterCheck
        Function returnType functionName paramsMappingLayer
            <$> check (addMappingLayer paramsMappingLayer $
                        state {functionContext = Just returnType}) body

instance CheckSemantics Syntax.Program Program where
    check state (Syntax.Program (functions, body) _) =
        let
        
        in do
        undefined

checkProgram
    :: CheckSemantics syntaxTree result
    => syntaxTree -> LogEither SemanticError result
checkProgram = check $ CheckerState undefined undefined undefined
