module Inference exposing (inferHole)

import Dict exposing (Dict)
import Elm.Docs exposing (Alias)
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Case, Expression(..), Function, FunctionImplementation)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as Src
import Elm.Type exposing (Type(..))
import Scheme exposing (Scheme(..))
import Set exposing (Set)
import Solver
import Src
import State exposing (State)
import Triple
import Type
import TypeEnv exposing (TypeEnv)


inferHole :
    { src : String
    , holeRange : Range
    , values : Dict String Type
    , typeAliases : List Alias
    }
    -> Maybe ( Type, Dict String Type )
inferHole params =
    let
        srcModule =
            "module Main exposing (..)\n" ++ params.src
    in
    case Elm.Parser.parse srcModule of
        Err error ->
            Nothing

        Ok rawFile ->
            let
                getFunction (Node _ declaration) =
                    case declaration of
                        FunctionDeclaration function ->
                            Just function

                        _ ->
                            Nothing
            in
            rawFile
                |> Elm.Processing.process Elm.Processing.init
                |> .declarations
                |> List.filterMap getFunction
                |> List.head
                |> Maybe.andThen
                    (getHole
                        params.typeAliases
                        params.values
                        params.holeRange
                    )


getHole :
    List Alias
    -> Dict String Type
    -> Range
    -> Function
    -> Maybe ( Type, Dict String Type )
getHole typeAliases values holeRange function =
    let
        ( constraints, holes, result ) =
            runInfer typeAliases
                initialEnv
                { start = incrementRow holeRange.start
                , end = incrementRow holeRange.end
                }
                function

        initialEnv =
            values
                |> Dict.toList
                |> TypeEnv.fromValues

        incrementRow location =
            { location | row = location.row + 1 }
    in
    case result of
        Err error ->
            Nothing

        Ok _ ->
            let
                solve ( _, tipe, env ) =
                    Solver.run constraints
                        |> Result.toMaybe
                        |> Maybe.map (substitute tipe env)

                substitute tipe env subst =
                    let
                        substEnv =
                            TypeEnv.apply subst env
                    in
                    ( Type.apply subst tipe
                    , TypeEnv.diff substEnv initialEnv
                        |> TypeEnv.toValues
                        |> Dict.fromList
                    )
            in
            holes
                |> List.head
                |> Maybe.andThen solve



---- INFER


type Infer a
    = Infer (TypeEnv -> State Int ( List Constraint, List Hole, Result Error a ))


type alias Hole =
    ( String, Type, TypeEnv )


storeHole : String -> Type -> Infer ()
storeHole name tipe =
    Infer <|
        \env ->
            State.state ( [], [ ( name, tipe, env ) ], Ok () )


type alias Constraint =
    ( Type, Type )


addConstraint : Constraint -> Infer ()
addConstraint constraint =
    Infer (\_ -> State.state ( [ constraint ], [], Ok () ))


type Error
    = UnboundVariable String
    | ParserError
    | SyntaxError
    | UnsupportedExpression Expression
    | UnsupportedPattern Pattern
    | UnsupportedUnification


runInfer :
    List Alias
    -> TypeEnv
    -> Range
    -> Function
    -> ( List Constraint, List Hole, Result Error Type )
runInfer typeAliases env holeRange expr =
    let
        (Infer run) =
            inferFunction typeAliases holeRange expr
    in
    State.finalValue 0 (run env)


inferFunction : List Alias -> Range -> Function -> Infer Type
inferFunction typeAliases holeRange function =
    let
        (Node _ declaration) =
            function.declaration

        inferHelp args argTypes schemes =
            case args of
                [] ->
                    inEnvs schemes
                        (infer holeRange declaration.expression
                            |> andThen
                                (\tipe ->
                                    let
                                        inferedType =
                                            returnType argTypes tipe
                                    in
                                    case function.signature of
                                        Nothing ->
                                            return inferedType

                                        Just (Node _ signature) ->
                                            addConstraint
                                                ( inferedType
                                                , signature.typeAnnotation
                                                    |> Type.fromTypeAnnotation
                                                    |> Type.normalize typeAliases
                                                )
                                                |> map (\_ -> inferedType)
                                )
                        )

                arg :: rest ->
                    inferPattern arg
                        |> andThen
                            (\( argType, newSchemes ) ->
                                inferHelp rest
                                    (argType :: argTypes)
                                    (newSchemes ++ schemes)
                            )

        returnType vars tipe =
            List.foldl Lambda tipe vars
    in
    inferHelp declaration.arguments [] []


infer : Range -> Node Expression -> Infer Type
infer holeRange (Node range expr) =
    case expr of
        UnitExpr ->
            return (Tuple [])

        Application allExprs ->
            case allExprs of
                [] ->
                    throwError ParserError

                function :: arguments ->
                    let
                        inferArguments functionType =
                            let
                                inferHelp types exprs =
                                    case exprs of
                                        [] ->
                                            fresh
                                                |> andThen
                                                    (\typeVar ->
                                                        addConstraint
                                                            ( functionType
                                                            , List.foldl Lambda typeVar types
                                                            )
                                                            |> map (\_ -> typeVar)
                                                    )

                                        firstExpr :: rest ->
                                            infer holeRange firstExpr
                                                |> andThen
                                                    (\tipe -> inferHelp (tipe :: types) rest)
                            in
                            inferHelp [] arguments
                    in
                    infer holeRange function
                        |> andThen inferArguments

        FunctionOrValue moduleName name ->
            if range == holeRange then
                storeHole name (Var name)
                    |> map (\_ -> Var name)

            else if name == "True" then
                return (Type "Bool" [])

            else if name == "False" then
                return (Type "Bool" [])

            else
                lookupEnv (Src.qualifiedName moduleName name)

        IfBlock exprCond exprIf exprElse ->
            let
                inferCond =
                    infer holeRange exprCond
                        |> andThen inferIf

                inferIf typeCond =
                    addConstraint ( typeCond, Type "Bool" [] )
                        |> andThen (\_ -> infer holeRange exprIf)
                        |> andThen (inferElse typeCond)

                inferElse typeCond typeIf =
                    infer holeRange exprElse
                        |> andThen (returnHelp typeCond typeIf)

                returnHelp typeCond typeIf typeElse =
                    addConstraint ( typeIf, typeElse )
                        |> map (\_ -> typeIf)
            in
            inferCond

        Integer _ ->
            return (Type "Int" [])

        Floatable _ ->
            return (Type "Float" [])

        Literal _ ->
            return (Type "String" [])

        CharLiteral _ ->
            return (Type "Char" [])

        TupledExpression allExprs ->
            let
                inferHelp exprs types =
                    case exprs of
                        [] ->
                            return (Tuple (List.reverse types))

                        firstExpr :: rest ->
                            infer holeRange firstExpr
                                |> andThen (\tipe -> inferHelp rest (tipe :: types))
            in
            inferHelp allExprs []

        ParenthesizedExpression parenthesizedExpr ->
            infer holeRange parenthesizedExpr

        CaseExpression caseBlock ->
            let
                inferCaseBranches exprTipe =
                    case caseBlock.cases of
                        [] ->
                            throwError SyntaxError

                        firstCase :: rest ->
                            let
                                inferRest cases firstType =
                                    case cases of
                                        [] ->
                                            return firstType

                                        nextCase :: nextRest ->
                                            inferCaseBranch holeRange exprTipe nextCase
                                                |> andThen
                                                    (Tuple.pair firstType >> addConstraint)
                                                |> andThen
                                                    (\_ -> inferRest nextRest firstType)
                            in
                            inferCaseBranch holeRange exprTipe firstCase
                                |> andThen (inferRest rest)
            in
            infer holeRange caseBlock.expression
                |> andThen inferCaseBranches

        LambdaExpression lambda ->
            let
                inferHelp args argTypes schemes =
                    case args of
                        [] ->
                            inEnvs schemes
                                (infer holeRange lambda.expression
                                    |> map (returnType argTypes)
                                )

                        arg :: rest ->
                            inferPattern arg
                                |> andThen
                                    (\( argType, newSchemes ) ->
                                        inferHelp rest
                                            (argType :: argTypes)
                                            (newSchemes ++ schemes)
                                    )

                returnType vars tipe =
                    List.foldl Lambda tipe vars
            in
            inferHelp lambda.args [] []

        RecordExpr recordSetters ->
            let
                inferSetter (Node _ ( Node _ name, valueExpr )) =
                    infer holeRange valueExpr
                        |> map (Tuple.pair name)
            in
            traverse inferSetter recordSetters
                |> map (\namedTypes -> Record namedTypes Nothing)

        ListExpr elementExprs ->
            case elementExprs of
                [] ->
                    fresh

                firstExpr :: rest ->
                    let
                        inferRest firstType =
                            rest
                                |> traverse (infer holeRange)
                                |> andThen (traverse (Tuple.pair firstType >> addConstraint))
                                |> map (\_ -> Type "List" [ firstType ])
                    in
                    infer holeRange firstExpr
                        |> andThen inferRest

        OperatorApplication name infixDirection exprA exprB ->
            throwError (UnsupportedExpression expr)

        PrefixOperator _ ->
            throwError (UnsupportedExpression expr)

        Operator _ ->
            throwError (UnsupportedExpression expr)

        Hex _ ->
            throwError (UnsupportedExpression expr)

        Negation _ ->
            throwError (UnsupportedExpression expr)

        LetExpression _ ->
            throwError (UnsupportedExpression expr)

        RecordAccess _ _ ->
            throwError (UnsupportedExpression expr)

        RecordAccessFunction _ ->
            throwError (UnsupportedExpression expr)

        RecordUpdateExpression (Node _ name) recordSetters ->
            let
                inferRecordSetter (Node _ ( Node _ fieldName, fieldExpr )) =
                    infer holeRange fieldExpr
                        |> map (Tuple.pair fieldName)

                inferRecordType fieldTypes =
                    lookupEnv name
                        |> andThen
                            (\recordType ->
                                fresh
                                    |> andThen
                                        (\var ->
                                            let
                                                varName =
                                                    case var of
                                                        Var n ->
                                                            Just n

                                                        _ ->
                                                            Nothing
                                            in
                                            addConstraint
                                                ( recordType
                                                , Record fieldTypes varName
                                                )
                                                |> map (\_ -> recordType)
                                        )
                            )
            in
            recordSetters
                |> traverse inferRecordSetter
                |> andThen inferRecordType

        GLSLExpression _ ->
            throwError (UnsupportedExpression expr)


lookupEnv : String -> Infer Type
lookupEnv name =
    Infer <|
        \env ->
            case TypeEnv.lookup name env of
                Nothing ->
                    State.state
                        ( []
                        , []
                        , Err (UnboundVariable name)
                        )

                Just scheme ->
                    let
                        (Infer run) =
                            instantiate scheme
                    in
                    run env


inferPattern : Node Pattern -> Infer ( Type, List ( String, Scheme ) )
inferPattern (Node _ pattern) =
    case pattern of
        AllPattern ->
            fresh
                |> map (\tipe -> ( tipe, [] ))

        UnitPattern ->
            return (Tuple [])
                |> map (\tipe -> ( tipe, [] ))

        CharPattern _ ->
            return (Type "Char" [])
                |> map (\tipe -> ( tipe, [] ))

        StringPattern _ ->
            return (Type "String" [])
                |> map (\tipe -> ( tipe, [] ))

        IntPattern _ ->
            return (Type "Int" [])
                |> map (\tipe -> ( tipe, [] ))

        HexPattern _ ->
            throwError (UnsupportedPattern pattern)

        FloatPattern _ ->
            return (Type "Float" [])
                |> map (\tipe -> ( tipe, [] ))

        TuplePattern patterns ->
            traverse inferPattern patterns
                |> map List.unzip
                |> map
                    (\( types, schemes ) ->
                        ( Tuple types
                        , List.concat schemes
                        )
                    )

        RecordPattern names ->
            throwError (UnsupportedPattern pattern)

        UnConsPattern firstPattern secondPattern ->
            throwError (UnsupportedPattern pattern)

        ListPattern patterns ->
            throwError (UnsupportedPattern pattern)

        VarPattern name ->
            fresh
                |> map
                    (\tipe ->
                        ( tipe
                        , [ ( name, ForAll [] tipe ) ]
                        )
                    )

        NamedPattern { moduleName, name } patterns ->
            fresh
                |> andThen
                    (\tipe ->
                        lookupEnv (Src.qualifiedName moduleName name)
                            |> andThen
                                (\constructorType ->
                                    traverse inferPattern patterns
                                        |> map List.unzip
                                        |> andThen
                                            (\( types, schemes ) ->
                                                addConstraint
                                                    ( constructorType
                                                    , List.foldl Lambda tipe types
                                                    )
                                                    |> map
                                                        (\_ ->
                                                            ( tipe
                                                            , List.concat schemes
                                                            )
                                                        )
                                            )
                                )
                    )

        AsPattern subPattern name ->
            throwError (UnsupportedPattern pattern)

        ParenthesizedPattern subPattern ->
            inferPattern subPattern


inferCaseBranch : Range -> Type -> Case -> Infer Type
inferCaseBranch holeRange exprType ( pattern, expr ) =
    inferPattern pattern
        |> andThen
            (\( tipe, schemes ) ->
                addConstraint ( exprType, tipe )
                    |> andThen
                        (\_ ->
                            inEnvs schemes
                                (infer holeRange expr)
                        )
            )


fresh : Infer Type
fresh =
    Infer <|
        \_ ->
            State.advance <|
                \count ->
                    ( ( [], [], Ok (Var ("a" ++ String.fromInt count)) )
                    , count + 1
                    )


inEnv : ( String, Scheme ) -> Infer a -> Infer a
inEnv ( var, scheme ) (Infer run) =
    Infer <|
        \env ->
            run (TypeEnv.extend var scheme env)


inEnvs : List ( String, Scheme ) -> Infer a -> Infer a
inEnvs newSchemes (Infer run) =
    Infer <|
        \env ->
            let
                extend ( name, scheme ) =
                    TypeEnv.extend name scheme
            in
            run (List.foldl extend env newSchemes)


return : a -> Infer a
return a =
    Infer (\_ -> State.state ( [], [], Ok a ))


throwError : Error -> Infer a
throwError error =
    Infer (\_ -> State.state ( [], [], Err error ))


map : (a -> b) -> Infer a -> Infer b
map f (Infer run) =
    Infer (\env -> State.map (Triple.mapThird (Result.map f)) (run env))


map2 : (a -> b -> c) -> Infer a -> Infer b -> Infer c
map2 f (Infer runA) (Infer runB) =
    Infer <|
        \env ->
            State.map2
                (\( logsA, holesA, resultA ) ( logsB, holesB, resultB ) ->
                    ( logsA ++ logsB
                    , holesA ++ holesB
                    , Result.map2 f resultA resultB
                    )
                )
                (runA env)
                (runB env)


andThen : (a -> Infer b) -> Infer a -> Infer b
andThen f (Infer runA) =
    Infer <|
        \env ->
            runA env
                |> State.andThen
                    (\( logsA, holesA, resultA ) ->
                        case resultA of
                            Err typeError ->
                                State.state ( logsA, holesA, Err typeError )

                            Ok a ->
                                let
                                    (Infer runB) =
                                        f a
                                in
                                runB env
                                    |> State.map
                                        (Triple.mapSecond (List.append holesA)
                                            >> Triple.mapFirst (List.append logsA)
                                        )
                    )


traverse : (a -> Infer b) -> List a -> Infer (List b)
traverse f listA =
    traverseHelp f listA (return [])


traverseHelp : (a -> Infer b) -> List a -> Infer (List b) -> Infer (List b)
traverseHelp f listA inferB =
    case listA of
        [] ->
            inferB

        a :: rest ->
            traverseHelp f rest <|
                map2 (::) (f a) inferB



---- INSTANTIATE AND GENERALIZE


instantiate : Scheme -> Infer Type
instantiate (ForAll vars tipe) =
    Infer <|
        \_ ->
            instantiateHelp vars Dict.empty
                |> State.map
                    (\subst ->
                        ( []
                        , []
                        , Ok (Type.apply subst tipe)
                        )
                    )


instantiateHelp : List String -> Dict String Type -> State Int (Dict String Type)
instantiateHelp vars subst =
    case vars of
        [] ->
            State.state subst

        var :: rest ->
            State.andThen (instantiateHelp rest) <|
                State.advance <|
                    \count ->
                        ( Dict.insert var
                            (Var ("a" ++ String.fromInt count))
                            subst
                        , count + 1
                        )


generalize : TypeEnv -> Type -> Scheme
generalize env tipe =
    let
        subst =
            Set.toList <|
                Set.diff
                    (Type.freeTypeVars tipe)
                    (TypeEnv.freeTypeVars env)
    in
    ForAll subst tipe
