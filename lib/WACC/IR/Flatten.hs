module WACC.IR.Flatten where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Function
import Control.Arrow

import qualified WACC.Semantics.Structure as SM
import WACC.IR.Structure
import Text.Parser (parseString, Parsed (Parsed))
import WACC.Syntax.Parser (program)
import WACC.Semantics.Checker (checkProgram)
import WACC.Semantics.Utils (LogEither(Ok))

lookUp :: Ord k => k -> [M.Map k a] -> a
lookUp _ [] = error "Semantic check has failed!"
lookUp k (m:ms) = lookUp k ms `fromMaybe` (m M.!? k)

data FlattenerState = FlattenerState {
    mappingStack :: [String `M.Map` Int],
    variableCounter :: Int,
    labelCounter :: Int
} deriving Show

initialState :: FlattenerState
initialState = FlattenerState {
    mappingStack = [],
    variableCounter = 1,
    labelCounter = 1
}

db = do
    raw <- readFile "draft.wacc"
    let Right (Parsed _ ast _) = parseString program raw
    let Ok ast' = checkProgram ast
    let SM.Program _ block = ast'
    traverse print $ snd $ flattenBlock block

class Flatten a b where
    flatten :: FlattenerState -> a -> (FlattenerState, b)

instance Flatten SM.Expression (Scalar, [Statement]) where
    flatten state = \case
        SM.Identifier _ name ->
            let identifier = Identifier name (lookUp name (mappingStack state))
            in (state, (Variable identifier, []))

        SM.LiteralInt x ->
            (state, (Immediate x, []))

        SM.Add x y ->
            let
            (state', (left, evaluateLeft)) = flatten state x
            (state'', (right, evaluateRight)) = flatten state' y
            tmp = Temporary "var" (variableCounter state'')
            in
            ( state'' { variableCounter = variableCounter state'' + 1 }
            , ( Variable tmp
              , evaluateLeft ++ evaluateRight ++
                [Assign B4 tmp (Add left right)]
              )
            )

        SM.LiteralArray t es ->
            let
            (state', (scalars, evaluateElements)) = flatten state es
            tmp = Temporary "var" (variableCounter state')
            in
            ( state' { variableCounter = variableCounter state' + 1 }
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

        _ -> undefined

instance Flatten [SM.Expression] ([Scalar], [Statement]) where
    flatten state = foldr
        (\expression (state, (allScalars, allStatements)) ->
            let (state', (scalar, statements)) = flatten state expression
            in  (state', (scalar:allScalars, statements++allStatements))
        )
        (state, ([], []))

flattenIndirect ::
    FlattenerState -> SM.Expression -> (FlattenerState, (Scalar, [Statement]))
flattenIndirect state = \case
    i@(SM.Identifier {}) -> flatten state i

    SM.ArrayElement _ array index ->
        let
        (state', (index', evaluateIndex)) = flatten state index
        (state'', (array', evaluateElementAddress)) = flattenIndirect state' array
        variable = Temporary "var" (variableCounter state'')
        in
        ( state'' { variableCounter = variableCounter state'' + 1}
        , ( Variable variable
          , evaluateIndex ++ evaluateElementAddress ++
            [Assign B8 variable (SeekArrayElement array' index')]
          )
        )

    _ -> error "Semantic check has failed."

instance Flatten SM.Statement [Statement] where
    flatten state = \case
        SM.Declare t name expression ->
            let
            (state', (result, evaluation)) = flatten state expression
            identifier = Identifier name (variableCounter state')
            in
            ( state' {
                mappingStack =
                    M.insert name (variableCounter state')
                    (head (mappingStack state)) : tail (mappingStack state),
                variableCounter = variableCounter state' + 1
            }
            , evaluation ++ [Assign (getSize t) identifier (Scalar result)]
            )

        SM.Assign t (SM.Identifier _ name) expression ->
            let
            variableNumber = lookUp name (mappingStack state)
            (state', (result, evaluation)) = flatten state expression
            in
            ( state'
            , evaluation ++
              [Assign (getSize t)
                (Identifier name variableNumber) (Scalar result)]
            )

        SM.Assign t left expression ->
            let
            (state', (result, evaluateExpression)) = flatten state expression
            (state'', (Variable address, evaluateAddress)) =
                flattenIndirect state' left
            in
            ( state''
            , evaluateExpression ++ evaluateAddress ++
              [AssignIndirect (getSize t) address (Scalar result)]
            )

        SM.Scope block ->
            flatten state block &
                first (\s -> s { mappingStack = tail $ mappingStack s })

        _ -> undefined

instance Flatten [SM.Statement] [Statement] where
    flatten state = \case
        [] -> (state, [])
        s:ss ->
            let
            (state', s') = flatten state s
            (state'', s'') = flatten state' ss
            in
            (state'', s' ++ s'')

class HasReference a where
    reference :: a -> S.Set Identifier

instance HasReference Scalar where
    reference = \case
        (Variable var) -> S.singleton var
        _ -> S.empty

instance HasReference [Scalar] where
    reference xs = foldl S.intersection S.empty (reference <$> xs)

instance HasReference Expression where
    reference = \case
        Scalar s -> reference s
        Add a b -> reference a `S.union` reference b
        NewArray _ xs -> reference xs
        SeekArrayElement a i -> reference a `S.union` reference i
        Call _ args -> reference args

instance HasReference Statement where
    reference = \case
        Assign _ var@(Identifier {}) e -> var `S.insert` reference e
        Assign _ _ e -> reference e
        AssignIndirect _ _ e -> reference e
        Print e -> reference e
        _ -> S.empty

free :: S.Set Identifier -> [Statement] -> [Statement]
free allocatedVariables = \case
    [] -> []
    s:ss ->
        let
        referencedVariables = reference s
        freedVariables = allocatedVariables `S.intersection` referencedVariables
        allocatedVariables' = allocatedVariables S.\\ freedVariables
        in
        [DirectiveFree var | var<- S.toList freedVariables] ++
        s : free allocatedVariables' ss

instance Flatten SM.Block [Statement] where
    flatten state (SM.Block statements) =
        let
        (state', statements') = flatten
            state {
                mappingStack = M.empty : mappingStack state
            }
            statements

        statementsWithFreeDirectives =
            statements'
            & reverse
            & free (
                head (mappingStack state')
                & M.toList
                & map (uncurry Identifier)
                & S.fromList)
            & reverse
        in
        (state', statementsWithFreeDirectives)

flattenBlock :: SM.Block -> (FlattenerState, [Statement])
flattenBlock = flatten initialState
