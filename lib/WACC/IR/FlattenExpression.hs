module WACC.IR.FlattenExpression where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Maybe
import           Data.Function
import           Control.Arrow

import qualified WACC.Semantics.Structure as SM
import           WACC.IR.Structure
import           WACC.Backend.LiteralString

lookUp :: Ord k => k -> [M.Map k a] -> a
lookUp _ [] = error "Semantic check has failed!"
lookUp k (m:ms) = lookUp k ms `fromMaybe` (m M.!? k)

data FlattenerState = FlattenerState {
    mappingStack :: [String `M.Map` Identifier],
    variableCounter :: Int,
    dataSegment :: String `M.Map` Int
} deriving Show

initialState :: String `M.Map` Int -> FlattenerState
initialState ds = FlattenerState {
    mappingStack = [],
    variableCounter = M.size ds + 1,
    dataSegment = ds
}

nextVariable :: FlattenerState -> FlattenerState
nextVariable state = state { variableCounter = variableCounter state + 1 }

class FlattenExpression a b where
    flatten :: FlattenerState -> a -> (FlattenerState, b)

instance FlattenExpression SM.Expression (Scalar, [SingleStatement]) where
    flatten :: FlattenerState
        -> SM.Expression -> (FlattenerState, (Scalar, [SingleStatement]))
    flatten state = \case
        SM.Identifier _ name ->
            let identifier = lookUp name (mappingStack state)
            in (state, (Variable identifier, []))

        SM.LiteralBool x -> (state, (Immediate (if x then 1 else 0), []))
        SM.LiteralInt x -> (state, (Immediate x, []))
        SM.LiteralString x -> (state, (String (dataSegment state M.! x), []))

        SM.Add a b ->
            let
            (state', (a', evaluateLeft)) = flatten state a
            (state'', (b', evaluateRight)) = flatten state' b
            tmp = Temporary "var" (variableCounter state'')
            in
            ( nextVariable state''
            , ( Variable tmp
              , evaluateLeft ++ evaluateRight ++
                [Assign B4 tmp (Add a' b')]
              )
            )

        SM.LiteralArray t es ->
            let
            (state', (scalars, evaluateElements)) = flatten state es
            tmp = Temporary "var" (variableCounter state')
            in
            ( nextVariable state'
            , ( Variable tmp
              , evaluateElements ++
                [Assign B8 tmp (NewArray (getSize t) scalars)]
              )
            )

        SM.FunctionCall t f args ->
            let
            (state', (scalars, evaluateArgs)) = flatten state args
            tmp = Temporary "var" (variableCounter state')
            in
            ( state' { variableCounter = variableCounter state' + 1 }
            , ( Variable tmp
              , evaluateArgs ++
                [Assign (getSize t) tmp (Call f scalars)]
              )
            )

instance FlattenExpression [SM.Expression] ([Scalar], [SingleStatement]) where
    flatten state = foldr
        (\expression (state, (allScalars, allStatements)) ->
            let (state', (scalar, statements)) = flatten state expression
            in  (state', (scalar:allScalars, statements++allStatements))
        )
        (state, ([], []))

flattenIndirect ::
    FlattenerState -> SM.Expression -> (FlattenerState, (Scalar, [SingleStatement]))
flattenIndirect state = \case
    i@(SM.Identifier {}) -> flatten state i

    SM.ArrayElement _ array index ->
        let
        (state', (index', evaluateIndex)) = flatten state index
        (state'', (array', evaluateElementAddress)) = flattenIndirect state' array
        variable = Temporary "var" (variableCounter state'')
        in
        ( nextVariable state''
        , ( Variable variable
          , evaluateIndex ++ evaluateElementAddress ++
            [Assign B8 variable (SeekArrayElement array' index')]
          )
        )

    _ -> error "Semantic check has failed."

instance FlattenExpression SM.Statement [NoExpressionStatement] where
    flatten state = \case
        SM.Declare t name expression ->
            let
            (state', (result, evaluation)) = flatten state expression
            identifier = Identifier name (variableCounter state')
            in
            ( nextVariable state' {
                mappingStack =
                    M.insert name identifier
                    (head (mappingStack state)) : tail (mappingStack state)
            }
            , map NE evaluation ++
              [NE $ Assign (getSize t) identifier (Scalar result)]
            )

        SM.Assign t (SM.Identifier _ name) expression ->
            let
            variable = lookUp name (mappingStack state)
            (state', (result, evaluation)) = flatten state expression
            in
            ( state'
            , map NE evaluation ++
              [NE $ Assign (getSize t) variable (Scalar result)]
            )

        SM.Assign t left expression ->
            let
            (state', (result, evaluateExpression)) = flatten state expression
            (state'', (Variable address, evaluateAddress)) =
                flattenIndirect state' left
            in
            ( state''
            , map NE evaluateExpression ++ map NE evaluateAddress ++
              [NE $ AssignIndirect (getSize t) address (Scalar result)]
            )

        SM.While condition body ->
            let
            (state', (condition', evaluateCondition)) = flatten state condition
            (state'', body') = flatten state' body
            possibleFreeVariables =
                S.fromList $ snd <$> concatMap M.toList (mappingStack state')
            in
            ( state''
            , map NE evaluateCondition ++
              [While condition' body'
                (reference body' `S.intersection` possibleFreeVariables)]
            )

        SM.Scope block ->
            flatten state block &
                first (\s -> s { mappingStack = tail $ mappingStack s })

        SM.Return expression ->
            let
            (state', (result, evaluateExpression)) = flatten state expression
            in
            ( state'
            , map NE evaluateExpression ++
              [NE $ Return result]
            )

instance FlattenExpression [SM.Statement] [NoExpressionStatement] where
    flatten state = \case
        [] -> (state, [])
        statement:statements ->
            let
            (state', statement') = flatten state statement
            (state'', statements'') = flatten state' statements
            in
            (state'', statement' ++ statements'')

class HasReference a where
    reference :: a -> S.Set Identifier

instance HasReference a => HasReference [a] where
    reference = S.unions . map reference

instance HasReference Scalar where
    reference = \case
        (Variable var@(Identifier {})) -> S.singleton var
        _ -> S.empty

instance HasReference Expression where
    reference = \case
        Scalar s -> reference s
        Add a b -> reference a `S.union` reference b
        NewArray _ xs -> reference xs
        SeekArrayElement a i -> reference a `S.union` reference i
        Call _ args -> reference args

instance HasReference SingleStatement where
    reference = \case
        Assign _ var@(Identifier {}) e -> var `S.insert` reference e
        Assign _ _ e -> reference e
        AssignIndirect _ _ e -> reference e
        Return e -> reference e

instance HasReference NoExpressionStatement where
    reference = \case
        NE s -> reference s

        While condition body _ -> reference body `S.union` reference condition

        If condition thenClause elseClause ->
            S.unions
                [ reference thenClause
                , reference elseClause
                , reference condition]

instance FlattenExpression SM.Block [NoExpressionStatement] where
    flatten state (SM.Block statements) =
        let
        (state', statements') =
            flatten
                state { mappingStack = M.empty : mappingStack state }
                statements
        in
        (state', statements')

flattenFunction ::
    M.Map String Int -> String -> [(String, SM.Type)] -> SM.Block
    -> Function NoExpressionStatement
flattenFunction dataSegment name params block =
    let
    state = initialState dataSegment
    state' = state {
        mappingStack = M.fromList
            ((fst <$> params) `zip` (Identifier "param" <$> [variableCounter state' ..]))
            : mappingStack state
    }
    (_, body) = flatten state' block
    in
    Function
        name
        ([variableCounter state' ..] `zip` (getSize . snd <$> params))
        body

flattenExpression :: SM.Program -> Program NoExpressionStatement
flattenExpression p@(SM.Program functions main) =
    let
    dataSegment = createDataSegments (getLiteralStrings p)
    in
    Program dataSegment (
        flattenFunction dataSegment "main" [] main
        : [flattenFunction dataSegment name params block
            | (SM.Function _ name params block) <- S.toList functions]
    )
