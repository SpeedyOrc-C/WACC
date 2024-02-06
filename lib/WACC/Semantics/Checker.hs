module WACC.Semantics.Checker where

import Prelude hiding (error)

import qualified Prelude  as P
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.List

import qualified WACC.Syntax.Structure as Syntax
import           WACC.Semantics.Error
import           WACC.Semantics.Structure
import           WACC.Semantics.Utils
import           WACC.Syntax.Validation

class CheckSemantics syntaxTree result
    | syntaxTree -> result, result -> syntaxTree where
    {- |
    Given a state and a part of the syntax tree,
    return the new bindings and the tree passed the semantic check,
    Or some errors if the semantic check failed.
    -}
    check :: CheckerState -> syntaxTree -> LogEither SemanticError result

-- function to check semantically of each sentances
instance CheckSemantics Syntax.Expression (Type, Expression) where
    check state = \case
        Syntax.LiteralInt      x _ -> Ok (Int            , LiteralInt x)
        Syntax.LiteralBool     x _ -> Ok (Bool           , LiteralBool x)
        Syntax.LiteralChar     x _ -> Ok (Char           , LiteralChar x)
        Syntax.LiteralString   x _ -> Ok (String         , LiteralString x)
        Syntax.LiteralPairNull {}  -> Ok (Pair (Any, Any), LiteralPairNull)

        -- if left handside is a indentifer which must be in the state
        Syntax.Identifier name range ->
            case lookUp state name of
                Just t  -> Ok (t, Identifier name)
                Nothing -> Log [SemanticError range (UndefinedIdentifier name)]

        Syntax.LiteralArray array _ -> do
            -- check if the elements by themselves in the array are semantically legal
            typesAndExpressions <- check state `mapM` array

            case zip (fst <$> typesAndExpressions) (expressionRange <$> array) of

                [] -> Ok (Array Any, LiteralArray Any [])
                -- it is kind of foldl to fold all types to common type
                (firstType, _):restTypesAndRanges ->
                    getCommonTypes firstType restTypesAndRanges
                    where
                    getCommonTypes commonType ((t, range):rest) =
                        -- if one of each is compactable with th other, then
                        -- the type of this array must be the common type of both
                        if t <| commonType || commonType <| t
                        then getCommonTypes (common commonType t) rest
                        else Log [SemanticError range $
                                    InconsistentTypesInArray commonType t]

                    -- if it is empty then this array is fine
                    getCommonTypes commonType [] = Ok
                        (Array commonType, LiteralArray commonType expressions)
                        where expressions = snd <$> typesAndExpressions

        -- the case of array[index]
        Syntax.ArrayElement (array, index) _ -> do
            -- get the type of array and index from state
            ((arrayType, array'), (indexType, index')) <-
                (,) <$> check state array <*> check state index

            -- if the index type is compatible of Int type (which is the only 
            -- type in the wacc that can be index type then check if the array type
            -- is a type of Array)
            if Int <| indexType then case arrayType of
                Array arrayElementType ->
                    Ok (arrayElementType, ArrayElement array' index')
                -- if the array is not a type of array then report error
                _ -> Log [SemanticError (expressionRange array) $
                            InvalidArray arrayType]
            else
                -- if index type is not compatible to int
                Log [SemanticError (expressionRange index) $
                        InvalidIndex indexType]

        -- case of newpair
        Syntax.LiteralPair (left, right) _ -> do
            -- using check to get the type of left and right
            ((leftType, left'), (rightType, right')) <-
                (,) <$> check state left <*> check state right

            Ok (Pair (leftType, rightType),
                    LiteralPair (leftType, rightType) (left', right'))

        -- get the first element of a pair
        Syntax.PairFirst pair _ -> do
            (pairType, pair') <- check state pair

            case pairType of
                -- the indentifer must be of pair type to allow using fst
                Pair (leftType, _) -> Ok (leftType, PairFirst pair')
                _ -> Log [SemanticError (expressionRange pair) $
                            InvalidPair pairType]
        -- get the second element of a pair
        Syntax.PairSecond pair _ -> do
            (pairType, pair') <- check state pair

            case pairType of
                -- the indentifer must be of pair type to allow using snd
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
            -- firstly check if the function appears in the function mapping, 
            -- which contains all functions name in the current program
            case M.lookup name (functionMapping state) of
                Nothing -> Log [SemanticError range $ UndefinedFunction name]
                Just (paramsTypes, returnType) -> do
                    -- get the type of each arguments
                    (unzip -> (argsTypes, args')) <- check state `traverse` args

                    -- check if the argument type being compatible of the parameter
                    -- type
                    let checkParamsArgsTypes paramType argType range' =
                            if paramType <| argType
                            then Ok paramType
                            else Log [SemanticError range' $
                                        IncompatibleArgument paramType argType]

                    -- using the checkParamsArgTypes to check if the type of each
                    -- arguments is compatible to the type of the coresponding parameter
                    sequence_ $ zipWith3 checkParamsArgsTypes
                        paramsTypes
                        argsTypes
                        (expressionRange <$> args)

                    -- check if the number of parameters the same as the arguments number
                    case compare (length args) (length paramsTypes) of
                        EQ -> Ok (returnType, FunctionCall name args')
                        GT -> Log [SemanticError range $ TooManyArguments
                                    name (length paramsTypes) (length args)]
                        LT -> Log [SemanticError range $ TooFewArguments
                                    name (length paramsTypes) (length args)]

        where
        checkLogical (left, right) constructor = do
            -- using check get the type of the left and right
            ((leftType, left'), (rightType, right')) <-
                (,) <$> check state left <*> check state right

            -- the left type and right type must be Bool type
            case leftType of
                Bool -> case rightType of
                    Bool -> Ok (Bool, constructor left' right')
                    _ -> Log [SemanticError (expressionRange right) $
                                InvalidLogical rightType]
                _ -> Log [SemanticError (expressionRange left) $
                            InvalidLogical leftType]

        -- chekc the unary operator
        checkUnary expr range (inputType, outputType) (constructor, error) = do
            -- using check to get the type of expr
            (exprType, e') <- check state expr
            -- the expr type must be compatible with the input type
            if inputType <| exprType
                then Ok (outputType, constructor e')
                else Log [SemanticError range $ error exprType]

        checkArithmetic (left, right) constructor = do
            -- using check to get the type of left and right
            ((leftType, left'), (rightType, right')) <-
                (,) <$> check state left <*> check state right

            -- as it is arithmetic
            -- left and right must be of Int type
            case leftType of
                Int -> case rightType of
                    Int -> Ok (Int, constructor left' right')
                    _ -> Log [SemanticError (expressionRange right) $
                                InvalidArithmetic rightType]
                _ -> Log [SemanticError (expressionRange left) $
                            InvalidArithmetic leftType]

        checkComparison (left, right) constructor = do
            -- using check to get the type of left and right
            ((leftType, left'), (rightType, right')) <-
                (,) <$> check state left <*> check state right

            case leftType of
                Int -> case rightType of
                    -- if left and right of compare are of Int type,
                    -- then convert the AST to compare Int
                    -- so that in the back-end it won't be re analysis
                    Int -> Ok (Bool, constructor CompareInt left' right')
                    _ -> Log [SemanticError (expressionRange right) $
                                InvalidComparisonRight CompareInt rightType]
                Char -> case rightType of
                    -- if left and right of compare are of Char type,
                    Char -> Ok (Bool, constructor CompareChar left' right')
                    _ -> Log [SemanticError (expressionRange right) $
                                InvalidComparisonRight CompareChar rightType]
                _ -> Log [SemanticError (expressionRange left) $
                            InvalidComparisonLeft leftType]

        checkEquality (left, right) range constructor = do
            -- using check to get the type of left and right
            ((leftType, left'), (rightType, right')) <-
                (,) <$> check state left <*> check state right

            -- left type must be compatible with right type or another way around
            if leftType <| rightType || rightType <| leftType then
                Ok (Bool, constructor leftType left' right')
            else
                Log [SemanticError range $ InvalidEquality leftType rightType]

instance CheckSemantics Syntax.Statement Statement where
    check state = \case
        -- firstly use fromSyntaxType to the type of the declare
        Syntax.Declare (fromSyntaxType -> declaredType, name, value) range
            -> case check state value of
            
            Ok (computedType, newValue) ->
                -- if the type at the right hand side can be compatible to the left
                -- hand side, then it is fine else return error
                if declaredType <| computedType then
                    -- the name of the identifier must not appear in the inner most layer
                    -- of the stack of variable tables
                    case lookUpInnermost state name of
                        Nothing -> Ok (Declare declaredType name newValue)
                        Just {} -> Log [SemanticError range $ RedefinedIdentifier name]
                
                else Log [SemanticError range $
                            IncompatibleAssignment declaredType computedType]

            Log x -> Log x

        Syntax.Assign (left, right) _ -> do
            ((leftType, left'), (rightType, right')) <-
                (,) <$> check state left <*> check state right
            
            if leftType == Any && rightType == Any
                then Log [SemanticError (expressionRange right) $
                                BothSideAnyAssignment]
                else if leftType <| rightType
                     then Ok (Assign left' right')
                     else Log [SemanticError (expressionRange right) $
                                IncompatibleAssignment leftType rightType]

        -- Read an character or an integer.
        Syntax.Read destination range -> do
            (destinationType, destination') <- check state destination

            if Int == destinationType || Char == destinationType
                then Ok (Read destination')
                else Log [SemanticError range $ InvalidRead destinationType]

        -- Free an array or a pair, this does not include string.
        Syntax.Free address range -> do
            (addressType, address') <- check state address

            if isArray addressType || isPair addressType
                then Ok (Free address')
                else Log [SemanticError range $ InvalidFree addressType]

        -- Return a value when it is in a function context.
        Syntax.Return value range -> do
            (valueType, value') <- check state value

            case functionContext state of
                -- Cannot return in main program, use `exit` instead.
                Nothing -> Log [SemanticError range ReturnInMain]
                -- Type of return value must match function's definition.
                Just functionReturnType ->
                    if functionReturnType <| valueType
                        then Ok (Return value')
                        else Log [SemanticError (expressionRange value) $
                                    InvalidReturn functionReturnType valueType]

        -- Exit with an integer exit code
        Syntax.Exit code range -> do
            (codeType, code') <- check state code

            if Int <| codeType
                then Ok (Exit code')
                else Log [SemanticError range $ InvalidExit codeType]

        -- Print the value to the standard output.
        Syntax.Print value _ -> do
            (valueType, value') <- check state value
            Ok (Print valueType value')

        -- Same as `print` but with an additional line break.
        Syntax.PrintLine value _ -> do
            (valueType, value') <- check state value
            Ok (PrintLine valueType value')

        Syntax.If (condition, thenBranch, elseBranch) _ -> do
            ((conditionType, condition'), thenBranch', elseBranch') <- (,,)
                <$> check           state  condition
                <*> check (addScope state) thenBranch
                <*> check (addScope state) elseBranch
            
            if Bool <| conditionType
                then Ok (If condition' thenBranch' elseBranch')
                else Log [SemanticError (expressionRange condition) $
                            InvalidIfCondition conditionType]

        Syntax.While (condition, body) _ -> do
            ((conditionType, condition'), body') <- (,)
                <$> check           state  condition
                <*> check (addScope state) body
            
            if Bool <| conditionType
                then Ok (While condition' body')
                else Log [SemanticError (expressionRange condition) $
                            InvalidWhileCondition conditionType]
        
        Syntax.Scope statements _ -> Scope <$> check (addScope state) statements
        
        Syntax.Skip {} -> P.error "`skip` statement is not eradicated."

instance CheckSemantics [Syntax.Statement] [Statement] where
    -- check for statements
    check state = \case
        [] -> Ok []

        -- `skip` does nothing in WACC, ignore it.
        (Syntax.Skip {} : ss) -> check state ss

        -- if it is a declare, then add the indentifer with the type which get from
        -- using the check function to the current scope
        (s@(Syntax.Declare (fromSyntaxType -> declaredType, name, _) _) : ss) ->
            let state' = state {
                mappingStack = case mappingStack state of
                    m : ms -> M.insert name declaredType m : ms
                    _ -> P.error "Mapping stack is empty."
                }
            in (:) <$> check state s <*> check state' ss

        -- Other statements do not affect the state.
        (s : ss) -> (:) <$> check state s <*> check state ss

-- | Try to find repetition in a list of entries.
--   If no repetition found, which is good, nothing happens.
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
                    Syntax.Name functionName _,
                    params,
                    body)
                _) = do
        let paramsWithRange = [(param, (range, fromSyntaxType t))
                                | (Syntax.Name param range, t) <- params]

        -- Find repeated parameter definitions.
        let (maybeParamsRepeated, paramsNoRepeat) =
                findRepetition paramsWithRange

        case maybeParamsRepeated of
            Nothing -> Ok ()
            -- Any of two parameters cannot have identical names.
            Just paramsRepeated -> Log
                [SemanticError range $ RedefinedParameter name
                    | (name, (range, _)) <- paramsRepeated]
        
        let paramsMapping = (snd <$>) <$> paramsNoRepeat
        
        let state' = state {
                mappingStack =
                    -- Variables in the body can shadow parameters.
                    M.empty :

                    -- Function's body can access parameters.
                    M.fromList paramsMapping :
                    
                    mappingStack state,
                
                -- Function's body allows `return` statement.
                functionContext = Just returnType
            }
        
        let body' = check state' body
        
        Function returnType functionName paramsMapping <$> body'

instance CheckSemantics Syntax.Program Program where
    check state (Syntax.Program (functions, body) _) = do
        let functionsWithRange = [(name, (range, (paramsTypes, returnType)))
                | Syntax.Function (
                    fromSyntaxType -> returnType,
                    Syntax.Name name range,
                    map (fromSyntaxType . snd) -> paramsTypes,
                    _
                    ) _ <- functions]

        -- Find repeated function definitions.
        let (maybeFunctionsRepeated, functionsNoRepeat) =
                findRepetition functionsWithRange

        case maybeFunctionsRepeated of
            Nothing -> Ok ()
            -- Any of two functions cannot have identical names.
            Just functionsRepeated -> Log
                [SemanticError range (RedefinedFunction name)
                    | (name, range) <- (fst <$>) <$> functionsRepeated]

        let state' = state {
                -- In both main program and functions' body
                -- can access other functions.
                functionMapping = M.fromList ((snd <$>) <$> functionsNoRepeat)
            }
        let functions' = S.fromList <$> check state' `mapM` functions
        let body'      =                check state'        body

        Program <$> functions' <*> body'

checkProgram :: CheckSemantics syntaxTree result
    => syntaxTree -> LogEither SemanticError result
checkProgram = check $ CheckerState Nothing M.empty [M.empty]
