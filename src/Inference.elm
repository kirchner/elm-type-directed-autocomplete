module Inference exposing (inferHole)

import Dict exposing (Dict)
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Case, Expression(..), Function, FunctionImplementation)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as Src
import Elm.Type exposing (Type(..))
import Set exposing (Set)
import State exposing (State)
import Type



---- SCHEME


type Scheme
    = ForAll (List String) Type



---- TYPE ENV


type TypeEnv
    = TypeEnv (Dict String Scheme)


extend : TypeEnv -> ( String, Scheme ) -> TypeEnv
extend (TypeEnv schemes) ( var, scheme ) =
    TypeEnv <|
        Dict.insert var scheme schemes


join : TypeEnv -> TypeEnv -> TypeEnv
join (TypeEnv schemesA) (TypeEnv schemesB) =
    TypeEnv (Dict.union schemesA schemesB)


emptyEnv : TypeEnv
emptyEnv =
    TypeEnv Dict.empty


diff : TypeEnv -> TypeEnv -> TypeEnv
diff (TypeEnv schemesA) (TypeEnv schemesB) =
    TypeEnv <|
        Dict.diff schemesA schemesB



---- SUBST


type alias Subst =
    Dict String Type


compose : Subst -> Subst -> Subst
compose substA substB =
    Dict.union (Dict.map (\_ -> apply substA) substB) substA


apply : Subst -> Type -> Type
apply subst tipe =
    case tipe of
        Type _ _ ->
            tipe

        Var var ->
            Dict.get var subst
                |> Maybe.withDefault tipe

        Lambda from to ->
            Lambda (apply subst from) (apply subst to)

        Tuple types ->
            Tuple (List.map (apply subst) types)

        Record fields var ->
            Record
                (List.map (Tuple.mapSecond (apply subst)) fields)
                var


freeTypeVars : Type -> Set String
freeTypeVars tipe =
    case tipe of
        Type _ _ ->
            Set.empty

        Var var ->
            Set.singleton var

        Lambda from to ->
            Set.union (freeTypeVars from) (freeTypeVars to)

        Tuple types ->
            List.foldl (freeTypeVars >> Set.union) Set.empty types

        Record fields _ ->
            Set.empty


applyToScheme : Subst -> Scheme -> Scheme
applyToScheme subst (ForAll vars tipe) =
    let
        freeSubst =
            List.foldl Dict.remove subst vars
    in
    ForAll vars <|
        apply freeSubst tipe


freeTypeVarsInScheme : Scheme -> Set String
freeTypeVarsInScheme (ForAll subst tipe) =
    Set.diff
        (freeTypeVars tipe)
        (Set.fromList subst)


applyToEnv : Subst -> TypeEnv -> TypeEnv
applyToEnv subst (TypeEnv schemes) =
    TypeEnv (Dict.map (\_ -> applyToScheme subst) schemes)


freeTypeVarsInEnv : TypeEnv -> Set String
freeTypeVarsInEnv (TypeEnv schemes) =
    Dict.foldl (\_ -> freeTypeVarsInScheme >> Set.union) Set.empty schemes



-- INSTANTIATE AND GENERALIZE


instantiate : Scheme -> Infer Type
instantiate (ForAll vars tipe) =
    Infer <|
        \_ ->
            instantiateHelp vars Dict.empty
                |> State.map
                    (\subst ->
                        ( []
                        , []
                        , Ok (apply subst tipe)
                        )
                    )


instantiateHelp : List String -> Dict String Type -> State Int (Dict String Type)
instantiateHelp vars subst =
    case vars of
        [] ->
            State.state subst

        var :: rest ->
            let
                increaseCount count =
                    State.put (count + 1)
                        |> State.andThen (addSubst count)

                addSubst count _ =
                    subst
                        |> Dict.insert var (Var (toVar count))
                        |> instantiateHelp rest

                toVar count =
                    "a" ++ String.fromInt count
            in
            State.get
                |> State.andThen increaseCount


generalize : TypeEnv -> Type -> Scheme
generalize env tipe =
    let
        subst =
            Set.toList <|
                Set.diff
                    (freeTypeVars tipe)
                    (freeTypeVarsInEnv env)
    in
    ForAll subst tipe



---- INFER


type Infer a
    = Infer (TypeEnv -> State Int ( List Constraint, List Hole, Result Error a ))


type alias Hole =
    ( String, Type, List ( String, Type ) )


type Error
    = UnboundVariable String
    | InfiniteType String Type
    | UnificationFail Type Type
    | UnificationMismatch (List Type) (List Type)
    | RecordUnificationMismatch (List ( String, Type )) (List ( String, Type ))
    | ParserError
    | SyntaxError


type alias Constraint =
    ( Type, Type )


runInfer :
    TypeEnv
    -> Range
    -> Node Expression
    -> ( List Constraint, List Hole, Result Error Type )
runInfer env holeRange expr =
    let
        (Infer run) =
            infer holeRange expr
    in
    State.finalValue 0 (run env)


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
                lookupEnv (qualifiedName moduleName name)

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
            Debug.todo "unhandled expression"

        PrefixOperator _ ->
            Debug.todo "unhandled expression"

        Operator _ ->
            Debug.todo "unhandled expression"

        Hex _ ->
            Debug.todo "unhandled expression"

        Negation _ ->
            Debug.todo "unhandled expression"

        LetExpression _ ->
            Debug.todo "unhandled expression"

        RecordAccess _ _ ->
            Debug.todo "unhandled expression"

        RecordAccessFunction _ ->
            Debug.todo "unhandled expression"

        RecordUpdateExpression _ _ ->
            Debug.todo "unhandled expression"

        GLSLExpression _ ->
            Debug.todo "unhandled expression"


lookupEnv : String -> Infer Type
lookupEnv name =
    Infer <|
        \((TypeEnv schemes) as env) ->
            case Dict.get name schemes of
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
            Debug.todo ""

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
            Debug.todo ""

        UnConsPattern firstPattern secondPattern ->
            Debug.todo ""

        ListPattern patterns ->
            Debug.todo ""

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
                        lookupEnv (qualifiedName moduleName name)
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
            Debug.todo ""

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


inEnv : ( String, Scheme ) -> Infer a -> Infer a
inEnv ( var, scheme ) (Infer run) =
    Infer <|
        \(TypeEnv schemes) ->
            run <|
                extend
                    (TypeEnv (Dict.remove var schemes))
                    ( var, scheme )


fresh : Infer Type
fresh =
    Infer <|
        \_ ->
            State.get
                |> State.andThen
                    (\count ->
                        State.put (count + 1)
                            |> State.map
                                (\_ ->
                                    ( []
                                    , []
                                    , Ok (Var ("a" ++ String.fromInt count))
                                    )
                                )
                    )


inEnvs : List ( String, Scheme ) -> Infer a -> Infer a
inEnvs newSchemes (Infer run) =
    Infer <|
        \(TypeEnv schemes) ->
            let
                actualSchemes =
                    List.foldl (\( name, _ ) -> Dict.remove name) schemes newSchemes
            in
            run <|
                TypeEnv <|
                    List.foldl (\( var, scheme ) -> Dict.insert var scheme)
                        actualSchemes
                        newSchemes


return : a -> Infer a
return a =
    Infer (\_ -> State.state ( [], [], Ok a ))


throwError : Error -> Infer a
throwError error =
    Infer (\_ -> State.state ( [], [], Err error ))


map : (a -> b) -> Infer a -> Infer b
map f (Infer run) =
    Infer (\env -> State.map (mapThird (Result.map f)) (run env))


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
                                        (mapSecond (List.append holesA)
                                            >> mapFirst (List.append logsA)
                                        )
                    )


mapFirst : (a -> d) -> ( a, b, c ) -> ( d, b, c )
mapFirst f ( a, b, c ) =
    ( f a, b, c )


mapSecond : (b -> d) -> ( a, b, c ) -> ( a, d, c )
mapSecond f ( a, b, c ) =
    ( a, f b, c )


mapThird : (c -> d) -> ( a, b, c ) -> ( a, b, d )
mapThird f ( a, b, c ) =
    ( a, b, f c )


storeHole : String -> Type -> Infer ()
storeHole name tipe =
    Infer <|
        \(TypeEnv env) ->
            State.state
                ( []
                , [ ( name
                    , tipe
                    , env
                        |> Dict.map (\_ (ForAll _ t) -> t)
                        |> Dict.toList
                    )
                  ]
                , Ok ()
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


addConstraint : Constraint -> Infer ()
addConstraint constraint =
    Infer (\_ -> State.state ( [ constraint ], [], Ok () ))



---- UNIFICATION


unifies : Type -> Type -> Result Error Subst
unifies typeA typeB =
    if typeA == typeB then
        Ok Dict.empty

    else
        case ( typeA, typeB ) of
            ( Var varA, _ ) ->
                bind varA typeB

            ( _, Var varB ) ->
                bind varB typeA

            ( Lambda fromA toA, Lambda fromB toB ) ->
                unifyMany [ fromA, toA ] [ fromB, toB ]

            ( Tuple typeAs, Tuple typeBs ) ->
                unifyMany typeAs typeBs

            ( Record fieldsA maybeVarA, Record fieldsB maybeVarB ) ->
                case ( maybeVarA, maybeVarB ) of
                    ( Nothing, Nothing ) ->
                        unifyFields
                            (List.sortBy Tuple.first fieldsA)
                            (List.sortBy Tuple.first fieldsB)

                    ( Just nameA, Nothing ) ->
                        Debug.todo "TODO"

                    ( Nothing, Just nameB ) ->
                        Debug.todo "TODO"

                    ( Just nameA, Just nameB ) ->
                        Debug.todo "TODO"

            _ ->
                Err (UnificationFail typeA typeB)


unifyFields : List ( String, Type ) -> List ( String, Type ) -> Result Error Subst
unifyFields fieldsA fieldsB =
    case ( fieldsA, fieldsB ) of
        ( ( nameA, typeA ) :: restA, ( nameB, typeB ) :: restB ) ->
            if nameA == nameB then
                unifies typeA typeB
                    |> Result.andThen
                        (\subst ->
                            unifyFields
                                (List.map (Tuple.mapSecond (apply subst)) restA)
                                (List.map (Tuple.mapSecond (apply subst)) restB)
                                |> Result.map
                                    (\newSubst ->
                                        compose newSubst subst
                                    )
                        )

            else
                Err (RecordUnificationMismatch fieldsA fieldsB)

        ( [], [] ) ->
            Ok Dict.empty

        _ ->
            Err (RecordUnificationMismatch fieldsA fieldsB)


unifyMany : List Type -> List Type -> Result Error Subst
unifyMany typeAs typeBs =
    case ( typeAs, typeBs ) of
        ( [], [] ) ->
            Ok Dict.empty

        ( typeA :: restA, typeB :: restB ) ->
            unifies typeA typeB
                |> Result.andThen
                    (\subst ->
                        unifyMany
                            (List.map (apply subst) restA)
                            (List.map (apply subst) restB)
                            |> Result.map
                                (\newSubst ->
                                    compose newSubst subst
                                )
                    )

        _ ->
            Err (UnificationMismatch typeAs typeBs)



---- SOLVE


runSolve : List Constraint -> Result Error Subst
runSolve constraints =
    solver
        ( Dict.empty, constraints )


solver : ( Subst, List Constraint ) -> Result Error Subst
solver ( subst, constraints ) =
    case constraints of
        [] ->
            Ok subst

        ( typeA, typeB ) :: rest ->
            unifies typeA typeB
                |> Result.andThen
                    (\newSubst ->
                        solver
                            ( compose newSubst subst
                            , List.map
                                (Tuple.mapFirst (apply newSubst)
                                    >> Tuple.mapSecond (apply newSubst)
                                )
                                rest
                            )
                    )


bind : String -> Type -> Result Error Subst
bind var tipe =
    let
        isVar =
            case tipe of
                Var otherVar ->
                    otherVar == var

                _ ->
                    False
    in
    if isVar then
        Ok Dict.empty

    else if Set.member var (freeTypeVars tipe) then
        Err (InfiniteType var tipe)

    else
        Ok (Dict.singleton var tipe)


inferHole :
    { src : String
    , holeRange : Range
    , values : Dict String Type
    }
    -> Maybe ( Type, Dict String Type )
inferHole ({ holeRange } as params) =
    let
        srcModule =
            "module Main exposing (..)\n" ++ params.src

        actualRange =
            { holeRange
                | start = incrementRow holeRange.start
                , end = incrementRow holeRange.end
            }

        incrementRow location =
            { location | row = location.row + 1 }

        initialEnv =
            params.values
                |> Dict.map (\_ -> ForAll [])
                |> TypeEnv
    in
    case Elm.Parser.parse srcModule of
        Err error ->
            Nothing

        Ok rawFile ->
            let
                file =
                    Elm.Processing.process Elm.Processing.init rawFile

                getFunction (Node _ declaration) =
                    case declaration of
                        FunctionDeclaration function ->
                            let
                                (Node _ functionImplementation) =
                                    function.declaration

                                (Node _ name) =
                                    functionImplementation.name
                            in
                            case function.signature of
                                Nothing ->
                                    Nothing

                                Just (Node _ signature) ->
                                    let
                                        functionType =
                                            typeFromTypeAnnotation
                                                signature.typeAnnotation

                                        arguments =
                                            argumentsFromPatterns
                                                functionImplementation.arguments
                                    in
                                    Just
                                        ( functionImplementation.expression
                                        , typeEnvFromArguments arguments functionType
                                        )

                        _ ->
                            Nothing
            in
            file.declarations
                |> List.filterMap getFunction
                |> List.head
                |> Maybe.andThen
                    (\( expression, ( typeEnv, returnType ) ) ->
                        let
                            ( constraints, holes, result ) =
                                runInfer (join typeEnv initialEnv) actualRange expression
                        in
                        case result of
                            Err error ->
                                Debug.todo "TODO"

                            Ok inferedReturnType ->
                                List.head holes
                                    |> Maybe.andThen
                                        (\( _, tipe, env ) ->
                                            (( inferedReturnType, returnType )
                                                :: constraints
                                            )
                                                |> runSolve
                                                |> Result.toMaybe
                                                |> Maybe.map
                                                    (\subst ->
                                                        let
                                                            substEnv =
                                                                env
                                                                    |> List.map
                                                                        (Tuple.mapSecond <|
                                                                            apply subst
                                                                                >> ForAll []
                                                                        )
                                                                    |> Dict.fromList
                                                                    |> TypeEnv

                                                            (TypeEnv newSchemes) =
                                                                diff substEnv initialEnv
                                                        in
                                                        ( apply subst tipe
                                                        , newSchemes
                                                            |> Dict.map
                                                                (\_ (ForAll _ t) -> t)
                                                        )
                                                    )
                                        )
                    )


typeEnvFromArguments : List String -> Type -> ( TypeEnv, Type )
typeEnvFromArguments names tipe =
    typeEnvFromArgumentsHelp Dict.empty names tipe


typeEnvFromArgumentsHelp :
    Dict String Scheme
    -> List String
    -> Type
    -> ( TypeEnv, Type )
typeEnvFromArgumentsHelp schemes names tipe =
    case ( names, tipe ) of
        ( [], returnType ) ->
            ( TypeEnv schemes
            , returnType
            )

        ( name :: restNames, Lambda from to ) ->
            typeEnvFromArgumentsHelp
                (Dict.insert name (ForAll [] from) schemes)
                restNames
                to

        _ ->
            Debug.todo "could not generate typeEnv from arguments"



---- HELPER


typeFromTypeAnnotation : Node Src.TypeAnnotation -> Type
typeFromTypeAnnotation (Node _ typeAnnotation) =
    case typeAnnotation of
        Src.GenericType var ->
            Var var

        Src.Typed (Node _ ( moduleName, name )) typeAnnotations ->
            Type
                (qualifiedName moduleName name)
                (List.map typeFromTypeAnnotation typeAnnotations)

        Src.Unit ->
            Tuple []

        Src.Tupled typeAnnotations ->
            Tuple (List.map typeFromTypeAnnotation typeAnnotations)

        Src.Record recordFields ->
            Record
                (List.map
                    (\(Node _ ( Node _ name, annotation )) ->
                        ( name, typeFromTypeAnnotation annotation )
                    )
                    recordFields
                )
                Nothing

        Src.GenericRecord (Node _ var) (Node _ recordFields) ->
            Record
                (List.map
                    (\(Node _ ( Node _ name, annotation )) ->
                        ( name, typeFromTypeAnnotation annotation )
                    )
                    recordFields
                )
                (Just var)

        Src.FunctionTypeAnnotation from to ->
            Lambda (typeFromTypeAnnotation from) (typeFromTypeAnnotation to)


qualifiedName : List String -> String -> String
qualifiedName moduleName name =
    String.join "." (moduleName ++ [ name ])


argumentsFromPatterns : List (Node Pattern) -> List String
argumentsFromPatterns patterns =
    let
        help (Node _ pattern) =
            case pattern of
                VarPattern name ->
                    name

                _ ->
                    "_"
    in
    List.map help patterns
