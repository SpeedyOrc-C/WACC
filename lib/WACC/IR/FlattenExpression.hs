{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module WACC.IR.FlattenExpression where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.List
import           Data.Char
import           Data.Maybe
import           Data.Traversable
import           Data.Functor.Identity
import           Control.Monad.Trans.State.Lazy

import qualified WACC.Semantics.Structure as SM
import           WACC.IR.Structure

lookUp :: Ord k => k -> [M.Map k a] -> a
lookUp _ [] = error "Semantic check has failed!"
lookUp k (m:ms) = lookUp k ms `fromMaybe` (m M.!? k)


nextVariable :: FlattenerState -> FlattenerState
nextVariable s = s { variableCounter = variableCounter s + 1 }

newTemporary :: State FlattenerState Identifier
newTemporary = do
    number <- gets variableCounter
    modify nextVariable
    return $ Temporary "var" number

newVariable :: String -> State FlattenerState Identifier
newVariable name = do
    number <- gets variableCounter
    modify nextVariable
    return $ Identifier name number

newParameter :: String -> State FlattenerState Identifier
newParameter name = do
    number <- gets variableCounter
    modify nextVariable
    return $ Identifier name number

expressions :: [SM.Expression] -> State FlattenerState ([Scalar],
                [SingleStatement])
expressions xs = do
    (unzip -> (scalars, concat -> evaluate)) <- traverse expression xs
    return (scalars, evaluate)

getIdentifier :: Scalar -> Identifier
getIdentifier (Variable ident') = ident'
getIdentifier (Reference ident') = ident'
getIdentifier _ = error "cannot get identifier at this stage"

paramExpression :: (SM.Type, (SM.Type, SM.Expression))
    -> State FlattenerState (Scalar,[SingleStatement])
paramExpression x = do
    case x of
        (SM.RefType _, (SM.RefType _, exp')) ->
            indirectExpression exp'
        -- (SM.Struct _ _, (SM.RefType _, exp')) ->
        --     expression exp'
        (_, (SM.RefType _, exp'@(SM.Identifier {}))) -> do
            (getIdentifier -> s, stats) <- expression exp'
            return (Reference s, stats)
        (_, (SM.RefType t, exp')) -> do
            tmp <- newTemporary
            (s, stats) <- indirectExpression exp'
            _ <- getSize' t
            return (Variable tmp, stats ++ [Assign B8 tmp (Scalar s)])
        (SM.RefType _, (t, exp')) -> do
            tmp <- newTemporary
            (s, stats) <- expression exp'
            t' <- getSize' t
            return (Variable tmp, stats ++ [Assign t' tmp (Dereference t' s)])
        (_, (_, exp')) ->
            expression exp'

paramExpressions :: [(SM.Type, (SM.Type, SM.Expression))] -> State FlattenerState ([Scalar], [SingleStatement])
paramExpressions xs = do
    (unzip -> (scalars, concat -> evaluate)) <- traverse paramExpression xs
    return (scalars, evaluate)

expression :: SM.Expression -> State FlattenerState (Scalar, [SingleStatement])
expression = \case
    SM.Identifier (SM.RefType (SM.Struct {})) name -> do
        identifier <- gets $ lookUp name . mappingStack
        return (Variable identifier, [])

    SM.Identifier (SM.RefType t) name -> do
        identifier <- gets $ lookUp name . mappingStack
        tmp <- newTemporary
        return (Variable tmp, [Assign (getSize t) tmp (Dereference (getSize t) (Variable identifier))])

    SM.Identifier _ name -> do
        identifier <- gets $ lookUp name . mappingStack
        return (Variable identifier, [])

    SM.LiteralBool bool ->
        return (Immediate (if bool then 1 else 0), [])

    SM.LiteralInt int ->
        return (Immediate int, [])

    SM.LiteralChar char ->
        return (Immediate (ord char), [])

    SM.LiteralString str -> do
        number <- gets $ (M.! str) . dataSegment
        return (String number, [])
    SM.NewStruct _ -> do
        tmp <- newTemporary
        return (Variable tmp, [])
    SM.LiteralArray t xs -> do
        (scalars, evaluate) <- expressions xs
        tmp <- newTemporary
        t' <- getSize' t
        return (Variable tmp,
            evaluate ++
            [Assign B8 tmp (NewArray t' scalars)])

    e@(SM.ArrayElement t _ _) -> do
        (result, evaluate) <- indirectExpression e
        tmp <- newTemporary
        t'  <- getSize' t
        return (Variable tmp,
            evaluate ++
            [Assign t' tmp (Dereference t' result)])

    SM.LiteralPairNull ->
        return (Immediate 0, [])
    SM.LiteralPair (sizeA, sizeB) (a, b) -> do
        (a', evaluateA) <- expression a
        (b', evaluateB) <- expression b
        tmp <- newTemporary
        sizeA' <- getSize' sizeA
        sizeB' <- getSize' sizeB
        return (Variable tmp,
            evaluateA ++ evaluateB ++
            [Assign B8 tmp (NewPair (sizeA', sizeB') (a', b'))])

    e@(SM.PairFirst t _) -> do
        (pair, evaluatePair) <- indirectExpression e
        tmp <- newTemporary
        t'  <- getSize' t
        return (Variable tmp,
            evaluatePair ++
            [Assign t' tmp (Dereference t' pair)])

    e@(SM.PairSecond t _) -> do
        (pair, evaluatePair) <- indirectExpression e
        tmp <- newTemporary
        t' <- getSize' t
        return (Variable tmp,
            evaluatePair ++
            [Assign t' tmp (Dereference t' pair)])

    SM.Not       e -> unary B1 Not       e
    SM.Negate    e -> binary B4 Subtract (SM.LiteralInt 0) e
    SM.Length    e -> unary B4 Length    e
    SM.Order     e -> unary B4 Order     e
    SM.Character e -> unary B1 Character e

    SM.Multiply  a b -> binary B4 Multiply  a b
    SM.Divide    a b -> binary B4 Divide    a b
    SM.Remainder a b -> binary B4 Remainder a b
    SM.Add       a b -> binary B4 Add       a b
    SM.Subtract  a b -> binary B4 Subtract  a b

    SM.Greater      SM.CompareInt a b -> binary B1 (Greater      B4) a b
    SM.GreaterEqual SM.CompareInt a b -> binary B1 (GreaterEqual B4) a b
    SM.Less         SM.CompareInt a b -> binary B1 (Less         B4) a b
    SM.LessEqual    SM.CompareInt a b -> binary B1 (LessEqual    B4) a b

    SM.Greater      SM.CompareChar a b -> binary B1 (Greater      B1) a b
    SM.GreaterEqual SM.CompareChar a b -> binary B1 (GreaterEqual B1) a b
    SM.Less         SM.CompareChar a b -> binary B1 (Less         B1) a b
    SM.LessEqual    SM.CompareChar a b -> binary B1 (LessEqual    B1) a b
    op@(SM.Field (t, _) _ _) -> do
        (op', evaluateOp) <- indirectExpression op
        tmp <- newTemporary
        t' <- getSize' t
        return (Variable tmp,
            evaluateOp ++
            [Assign t' tmp (Dereference t' op')])

    SM.Equal    t a b -> do
        t' <- getSize' t
        binary B1 (Equal  t') a b
    SM.NotEqual t a b -> do
        t' <- getSize' t
        binary B1 (NotEqual t') a b

    SM.And a b -> binary B1 And a b
    SM.Or  a b -> binary B1 Or  a b

    SM.FunctionCall t f args@(unzip.map snd -> (ts, _)) -> do
        (scalars, evaluate) <- paramExpressions args
        tmp <- newTemporary
        t' <- getSize' t
        ts' <-for ts getSize'
        return (Variable tmp,
            evaluate ++
            [Assign t' tmp (Call t' ("fn_" ++ f) (ts' `zip` scalars))])

    where
    unary :: Size -> (Scalar -> Expression) -> SM.Expression
                -> State FlattenerState (Scalar, [SingleStatement])
    unary size f a = do
        (a', evaluateA) <- expression a
        tmp <- newTemporary
        return (Variable tmp,
            evaluateA ++ [Assign size tmp (f a')])

    binary :: Size -> (Scalar -> Scalar -> Expression)
                -> SM.Expression -> SM.Expression
                -> State FlattenerState (Scalar, [SingleStatement])
    binary size f a b = do
        (a', evaluateA) <- expression a
        (b', evaluateB) <- expression b
        tmp <- newTemporary
        return (Variable tmp,
            evaluateA ++ evaluateB ++ [Assign size tmp (f a' b')])

indirectExpression ::
    SM.Expression -> State FlattenerState (Scalar, [SingleStatement])
indirectExpression = \case
    SM.Identifier (SM.RefType _) name -> do
        identifier <- gets $ lookUp name . mappingStack
        return (Variable identifier, [])

    i@(SM.Identifier _ _) -> expression i

    SM.ArrayElement t array@(SM.Identifier {}) index -> do
        (index', evaluateIndex) <- expression index
        (array', evaluateElementAddress) <- indirectExpression array
        tmp <- newTemporary
        t' <- getSize' t
        return (Variable tmp,
            evaluateIndex ++ evaluateElementAddress ++
            [Assign B8 tmp (SeekArrayElement t' array' index')])

    SM.ArrayElement t array index -> do
        (index', evaluateIndex) <- expression index
        (array', evaluateElementAddress) <- indirectExpression array
        value <- newTemporary
        tmp <- newTemporary
        t' <- getSize' t
        return (Variable tmp,
            evaluateIndex ++ evaluateElementAddress ++
            [ Assign B8 value (Dereference B8 array')
            , Assign B8 tmp
                (SeekArrayElement t' (Variable value) index')])

    SM.PairFirst _ pair@(SM.Identifier {}) -> do
        (pair', evaluatePair) <- indirectExpression pair
        tmp <- newTemporary
        return (Variable tmp,
            evaluatePair ++
            [Assign B8 tmp (SeekPairFirst pair')])

    SM.PairSecond _ pair@(SM.Identifier {}) -> do
        (pair', evaluatePair) <- indirectExpression pair
        tmp <- newTemporary
        return (Variable tmp,
            evaluatePair ++
            [Assign B8 tmp (SeekPairSecond pair')])

    SM.Field _ op@(SM.Identifier (SM.Struct structName _) _) name' -> do
        (op', evaluateOp) <- indirectExpression op
        tmp <- newTemporary
        structures <- gets structures
        let
            struct = M.lookup structName structures
        case struct of
            (Just (Structure _ _ struct')) -> do
                let field = lookup name' struct'
                case field of
                    (Just (offset, _)) -> return (Variable tmp,
                        evaluateOp ++ [Assign B8 tmp (GetField offset op')])
                    Nothing           ->error "Unfound field after semantic check"
            _ -> error "Unfound struct after semantic check"

    SM.Field (_, structName) op name' -> do
        (address, evaluateOp) <- indirectExpression op
        value <- newTemporary
        tmp <- newTemporary
        structures <- gets structures
        let
            struct = M.lookup structName structures
        case struct of
            (Just (Structure _ _ struct')) -> do
                let field = lookup name' struct'
                case field of
                    (Just (offset, _)) -> return (Variable tmp,
                        evaluateOp ++
                            [ Assign B8 value (Scalar address)
                            , Assign B8 tmp (GetField offset (Variable value))])
                    Nothing           ->error "Unfound field after semantic check"
            _ -> error "Unfound struct after semantic check"

    SM.PairFirst _ pair -> do
        (address, evaluatePair) <- indirectExpression pair
        value <- newTemporary
        tmp <- newTemporary
        return (Variable tmp,
            evaluatePair ++
            [ Assign B8 value (Dereference B8 address)
            , Assign B8 tmp (SeekPairFirst (Variable value))])

    SM.PairSecond _ pair -> do
        (address, evaluatePair) <- indirectExpression pair
        value <- newTemporary
        tmp <- newTemporary
        return (Variable tmp,
            evaluatePair ++
            [ Assign B8 value (Dereference B8 address)
            , Assign B8 tmp (SeekPairSecond (Variable value))])

    x -> error $ "Semantic check has failed." ++ show x

statement :: SM.Statement -> State FlattenerState [NoExpressionStatement]
statement = \case
    SM.Declare t@(SM.Struct _ _)
            name right -> do
        identifier <- newVariable name
        modify $ \s -> s {
            mappingStack = M.insert name identifier
                (head (mappingStack s)) : tail (mappingStack s)
        }
        t' <- getSize' t
        assigns <- statement $ SM.Assign t (SM.Identifier t name) right
        return $ NE (Assign t' identifier NewStruct) : assigns

    SM.Declare t name rightValue -> do
        (result, evaluation) <- expression rightValue
        identifier <- newVariable name
        modify $ \s -> s {
            mappingStack = M.insert name identifier
                (head (mappingStack s)) : tail (mappingStack s)
        }
        t' <- getSize' t
        return $ map NE evaluation ++
            [NE $ Assign t' identifier (Scalar result)]

    SM.Assign (SM.Struct structName fts) leftValue (SM.NewStruct exps) -> do
        structures <- gets structures
        let
            struct = M.lookup structName structures
            declareFields :: (String, SM.Type) -> SM.Expression
                -> State FlattenerState [NoExpressionStatement]
            declareFields (field, ft) exp' = do
                statement (SM.Assign ft (SM.Field (ft, structName) leftValue field) exp')

        case struct of
            (Just (Structure _ _ (map fst -> fields'))) -> do
                (concat -> assigns) <- for (zip (zip fields' fts) exps)
                    (uncurry declareFields)
                return assigns
            _ -> error "should not happen not found struct at declaring"

    SM.Assign t@(SM.Struct _ _) leftValue (SM.FunctionCall _ f args@(unzip.map snd -> (ts, _))) -> do
        (scalar, evaluateLeft) <- expression leftValue
        (scalars, evaluate) <- paramExpressions args
        t' <- getSize' t
        ts' <- for ts getSize'
        case scalar of
            Variable x -> return $
                map NE evaluate ++ map NE evaluateLeft ++
                    [NE(FunctionReturnStruct t' x ("fn_" ++ f)
                        (ts' `zip` scalars))]
            _ -> error "should not happen left scalar must be an variable"
        -- return $ NE(Assign B8 tmp (Reference scalar)): [FunctionReturnStruct tmp ]

    SM.Assign t@(SM.Struct structName fts) leftValue rightValue -> do
        structures <- gets structures
        _ <- getSize' t
        let
            struct = M.lookup structName structures
            declareFields :: (String, SM.Type)
                -> State FlattenerState [NoExpressionStatement]
            declareFields (field, ft) = do
                statement
                    (SM.Assign ft (SM.Field (ft, structName) leftValue field)
                        (SM.Field (ft, structName) rightValue field))

        case struct of
            (Just (Structure _ _ (map fst -> fields'))) -> do
                (concat -> assigns) <- for (zip fields' fts) declareFields
                return assigns
            _ -> error "should not happen not found struct at declaring"

    SM.Assign _ (SM.Identifier (SM.RefType t) name) rightValue -> do
        identifier <- gets $ lookUp name . mappingStack
        (result, evaluation) <- expression rightValue
        t' <- getSize' t
        return $ map NE evaluation ++
            [NE $ AssignIndirect t' identifier (Scalar result)]

    SM.Assign _ (SM.Identifier t name) rightValue -> do
        identifier <- gets $ lookUp name . mappingStack
        (result, evaluation) <- expression rightValue
        t' <- getSize' t
        return $ map NE evaluation ++
            [NE $ Assign t' identifier (Scalar result)]

    SM.Assign t leftValue rightValue -> do
        (result, evaluateRight) <- expression rightValue
        (scalar, evaluateLeft) <- indirectExpression leftValue
        t' <- getSize' t
        let Variable identifier = scalar
        return $ map NE evaluateRight ++ map NE evaluateLeft ++
            [NE $ AssignIndirect t' identifier (Scalar result)]

    SM.If condition thenClause elseClause -> do
        (condition', evaluateCondition) <- expression condition
        thenClause' <- block thenClause
        modify $ \s -> s { mappingStack = tail $ mappingStack s }
        elseClause' <- block elseClause
        modify $ \s -> s { mappingStack = tail $ mappingStack s }
        return $ map NE evaluateCondition ++
            [If condition' thenClause' elseClause']

    SM.While condition body -> do
        preWhile@(condition', evaluateCondition) <- expression condition
        body' <- block body
        possibleFreeVariables <- gets $
            S.fromList . map snd . concatMap M.toList . mappingStack
        modify $ \s -> s { mappingStack = tail $ mappingStack s }

        return [While preWhile body' WhileInfo  {
            possibleFreeVars = possibleFreeVariables,
            referencedFreeVariables =
                S.unions [
                    reference condition',
                    reference evaluateCondition,
                    reference body'
                ] `S.intersection` possibleFreeVariables
        }]

    SM.Scope b -> do
        b' <- block b
        modify $ \s -> s { mappingStack = tail $ mappingStack s }
        return b'

    SM.Return t e -> do
        (result, evaluateExpression) <- expression e
        t' <- getSize' t
        return $ map NE evaluateExpression ++
            [NE $ Return t' result]

    SM.Exit e -> do
        (result, evaluateExpression) <- expression e
        return $ map NE evaluateExpression ++
            [NE $ Exit result]

    SM.Print (SM.Struct name types) e -> do
        structures <- gets structures
        let (Structure _ _ (unzip -> (fields, _))) = structures M.! name
        let printItems = zip types fields >>= \(t, field) ->
                [SM.Print t (SM.Field (t, name) e field)]

        printAll <- traverse statement $
            [SM.Print SM.String (SM.LiteralString "{ ")] ++
            SM.Print SM.String (SM.LiteralString ", ") `intersperse` printItems ++
            [SM.Print SM.String (SM.LiteralString " }")]

        return $ concat printAll

    SM.Print t e -> do
        (result, evaluateExpression) <- expression e
        return $ map NE evaluateExpression ++
            [NE $ printByType t result]

    SM.PrintLine t e -> do
        ss <- statement $ SM.Print t e
        return $ ss ++ [NE PrintLineBreak]

    SM.Read _ ident@(SM.Identifier t name) -> do
        var <- gets $ lookUp name . mappingStack
        (ident', _) <- expression ident
        t' <- getSize' t
        return [NE $ Assign t' var (readByType ident' t)]

    SM.Read t e -> do
        (scalar, evaluateExpression) <- indirectExpression e
        t' <- getSize' t
        let Variable identifier = scalar
        return $ map NE evaluateExpression ++
            [NE $ AssignIndirect t' identifier (readByType scalar t)]

    SM.Free SM.Array {} e -> do
        (scalar, evaluateExpression) <- indirectExpression e
        return $ map NE evaluateExpression ++
            [NE $ FreeArray scalar]

    SM.Free _ e -> do
        (scalar, evaluateExpression) <- indirectExpression e
        return $ map NE evaluateExpression ++
            [NE $ Free scalar]

    where
    printByType = \case
        SM.Bool -> PrintBool
        SM.Int -> PrintInt
        SM.Char -> PrintChar
        SM.String -> PrintString
        SM.Array SM.Char -> PrintString
        SM.Array {} -> PrintAddress
        SM.Pair {} -> PrintAddress
        SM.RefType t -> printByType t
        _ -> error "Semantic check has failed."

    readByType t = \case
        SM.Int -> ReadInt
        SM.Char -> ReadChar t
        _ -> error "Semantic check has failed."

statements :: [SM.Statement] -> State FlattenerState [NoExpressionStatement]
statements xs = concat <$> traverse statement xs

block :: SM.Block -> State FlattenerState [NoExpressionStatement]
block (SM.Block xs) = do
    modify $ \s -> s { mappingStack = M.empty : mappingStack s }
    statements xs

class HasReference a where
    reference :: a -> S.Set Identifier

instance HasReference a => HasReference [a] where
    reference :: HasReference a => [a] -> S.Set Identifier
    reference = S.unions . map reference

instance HasReference Scalar where
    reference :: Scalar -> S.Set Identifier
    reference = \case
        (Variable var@(Identifier {})) -> S.singleton var
        (Variable var@(Parameter {})) -> S.singleton var
        (Variable var@(Temporary {})) -> S.singleton var
        (Reference var@(Identifier {})) -> S.singleton var
        (Reference var@(Parameter {})) -> S.singleton var
        (Reference var@(Temporary {})) -> S.singleton var
        _ -> S.empty

instance HasReference Expression where
    reference :: Expression -> S.Set Identifier
    reference = \case
        Scalar s -> reference s
        GetField _ s -> reference s
        Not a -> reference a
        Length a -> reference a
        Order a -> reference a
        Character a -> reference a
        NewStruct -> S.empty
        Multiply a b -> reference a `S.union` reference b
        Divide a b -> reference a `S.union` reference b
        Remainder a b -> reference a `S.union` reference b
        Add a b -> reference a `S.union` reference b
        Subtract a b -> reference a `S.union` reference b

        Greater _ a b -> reference a `S.union` reference b
        GreaterEqual _ a b -> reference a `S.union` reference b
        Less _ a b -> reference a `S.union` reference b
        LessEqual _ a b -> reference a `S.union` reference b

        Equal _ a b -> reference a `S.union` reference b
        NotEqual _ a b -> reference a `S.union` reference b

        And a b -> reference a `S.union` reference b
        Or a b -> reference a `S.union` reference b

        NewArray _ xs -> reference xs
        SeekArrayElement _ a i -> reference a `S.union` reference i

        NewPair _ (a, b) -> reference a `S.union` reference b
        SeekPairFirst p -> reference p
        SeekPairSecond p -> reference p

        Dereference _ s -> reference s

        Call _ _ args -> reference $ snd <$> args
        ReadInt -> S.empty
        ReadChar _ -> S.empty

instance HasReference SingleStatement where
    reference :: SingleStatement -> S.Set Identifier
    reference = \case
        Assign _ var@(Identifier {}) e -> var `S.insert` reference e
        Assign _ _ e -> reference e
        AssignIndirect _ address e -> address `S.insert` reference e
        Return _ e -> reference e
        Exit e -> reference e
        Free e -> reference e
        FreeArray e -> reference e
        FunctionReturnStruct _ var _ args ->
            var `S.insert` reference (snd <$> args)

        PrintBool e -> reference e
        PrintInt e -> reference e
        PrintChar e -> reference e
        PrintString e -> reference e
        PrintAddress e -> reference e
        PrintLineBreak -> S.empty

instance HasReference NoExpressionStatement where
    reference :: NoExpressionStatement -> S.Set Identifier
    reference = \case
        NE s -> reference s

        While (condition, evaluateCondition) body _ ->
            reference body `S.union`
            reference evaluateCondition `S.union`
            reference condition

        If condition thenClause elseClause ->
            S.unions
                [ reference thenClause
                , reference elseClause
                , reference condition]

function :: SM.Function ->
            State FlattenerState (Function NoExpressionStatement)
function (SM.Function t functionName params@(unzip -> (names, types)) b) = do
    oldState <- get
    size <- getSize' t
    (map fst -> identifiers) <- for params $ \(name, _) -> do
        identifier <- newParameter name
        return (identifier, getSize' t)

    modify $ \s -> s {
        mappingStack = M.fromList (names `zip` identifiers) : mappingStack s
    }

    b' <- block b
    ts' <- traverse getSize' types

    put oldState

    return $ Function
        size
        functionName
        (identifiers `zip` ts')
        b'

program :: S.Set SM.Function -> SM.Block
    -> State FlattenerState (Program NoExpressionStatement)
program fs main = do
    dataSegment <- gets dataSegment
    let allFunctions =
            SM.Function SM.Int "main" [] main :
            [SM.Function ret ("fn_" ++ name) param b
                | SM.Function ret name param b <- S.toList fs]
    functions' <- traverse function allFunctions
    return $ Program dataSegment functions'

flattenExpression :: (String `M.Map` Int, SM.Program) ->
                        Program NoExpressionStatement
flattenExpression (dataSegment, SM.Program structures fs main) =
    runIdentity $ evalStateT (program fs main)
        (initialState dataSegment structures)
