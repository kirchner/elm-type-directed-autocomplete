module Inference exposing
    ( Error(..)
    , errorToString
    , inferHole
    )

import Canonical exposing (Alias, Associativity(..), Binop, Imports, Union)
import Canonical.Annotation exposing (Annotation(..))
import Canonical.Type exposing (Type(..))
import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Infix exposing (Infix)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as Src
import Set exposing (Set)
import Solver
import Src
import State exposing (State)
import Triple


inferHole :
    { function : Elm.Syntax.Expression.Function
    , range : Range
    , moduleName : String
    , imports : Imports
    , binops : Dict String Binop
    , values : Dict String Annotation
    }
    -> Result Error ( Type, Dict String Type )
inferHole { moduleName, imports, binops, values, range, function } =
    let
        ( constraints, holes, result ) =
            runInfer env range function

        env =
            { moduleName = moduleName
            , imports = imports
            , binops = binops
            , values = values
            , qualifiedValues = Dict.empty
            }
    in
    case result of
        Err error ->
            Err error

        Ok _ ->
            let
                solve ( _, tipe, newEnv ) =
                    Solver.run constraints
                        |> Result.mapError CouldNotSolve
                        |> Result.map (substitute tipe newEnv)

                substitute tipe newEnv subst =
                    let
                        substEnv =
                            Dict.map
                                (\_ -> Canonical.Annotation.apply subst)
                                newEnv.values
                    in
                    ( Canonical.Type.apply subst tipe
                    , Dict.diff substEnv values
                        |> Dict.map (\_ (ForAll _ t) -> t)
                    )
            in
            holes
                |> List.head
                |> Result.fromMaybe NoHoleFound
                |> Result.andThen solve


type Error
    = UnboundVariable String
    | UnknownInfix String
    | ConflictingAssociativity
    | ParserError
    | CaseWithoutBranches
    | UnsupportedUnification
    | CouldNotSolve Solver.Error
    | NoHoleFound


errorToString : Error -> String
errorToString error =
    case error of
        UnboundVariable name ->
            "Unbound variable " ++ name

        UnknownInfix name ->
            "Unknown infix operator " ++ name

        ConflictingAssociativity ->
            "Found two operators with same precedence but different associativity"

        ParserError ->
            "Parser error"

        CaseWithoutBranches ->
            "There is a case without any branches"

        UnsupportedUnification ->
            "Unsuppoerted unification"

        CouldNotSolve solverError ->
            "Could not solve constraints: " ++ Solver.errorToString solverError

        NoHoleFound ->
            "No hole found at the specified range"



---- INFER


type Infer a
    = Infer (Env -> State Int ( List Constraint, List Hole, Result Error a ))


type alias Env =
    { moduleName : String
    , imports : Imports
    , binops : Dict String Binop
    , values : Dict String Annotation
    , qualifiedValues : Dict String (Dict String Annotation)
    }


type alias Hole =
    ( String, Type, Env )


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


runInfer :
    Env
    -> Range
    -> Elm.Syntax.Expression.Function
    -> ( List Constraint, List Hole, Result Error Type )
runInfer env range expr =
    let
        (Infer run) =
            inferFunction range expr
    in
    State.finalValue 0 (run env)



---- INFER TOP-LEVEL FUNCTIONS


inferFunction : Range -> Elm.Syntax.Expression.Function -> Infer Type
inferFunction range function =
    let
        (Node _ declaration) =
            function.declaration

        inferBody ( types, annotations ) =
            inEnvs (List.concat annotations)
                (infer range declaration.expression
                    |> andThen (returnType types)
                )

        returnType types tipe =
            let
                inferedType =
                    List.foldr Lambda tipe types
            in
            case function.signature of
                Nothing ->
                    return inferedType

                Just (Node _ signature) ->
                    instantiateTypeAnnotation signature.typeAnnotation
                        |> andThen (addConstraint << Tuple.pair inferedType)
                        |> map (\_ -> inferedType)
    in
    traverse inferPattern declaration.arguments
        |> andThen (List.unzip >> inferBody)



---- INFER EXPRESSIONS


infer : Range -> Node Expression -> Infer Type
infer range (Node currentRange expr) =
    case expr of
        FunctionOrValue moduleName name ->
            if range == currentRange then
                storeHole name (Var name)
                    |> map (\_ -> Var name)

            else if name == "True" then
                return Canonical.Type.bool

            else if name == "False" then
                return Canonical.Type.bool

            else
                findValue (Src.qualifiedName moduleName name)

        PrefixOperator _ ->
            -- TODO implement properly
            map2 Lambda
                (map Var freshVar)
                (map Var freshVar)

        Operator _ ->
            -- TODO implement properly
            map2 Lambda
                (map Var freshVar)
                (map2 Lambda
                    (map Var freshVar)
                    (map Var freshVar)
                )

        Literal _ ->
            return Canonical.Type.string

        CharLiteral _ ->
            return Canonical.Type.char

        Integer _ ->
            return Canonical.Type.int

        Hex _ ->
            return Canonical.Type.int

        Floatable _ ->
            return Canonical.Type.float

        UnitExpr ->
            return (Tuple [])

        ParenthesizedExpression parenthesizedExpr ->
            infer range parenthesizedExpr

        ListExpr elementExprs ->
            inferList range elementExprs

        TupledExpression allExprs ->
            inferTuple range allExprs

        Negation numberExpr ->
            infer range numberExpr

        OperatorApplication name _ exprA exprB ->
            inferBinops range name exprA exprB

        LambdaExpression lambda ->
            inferLambda range lambda

        Application allExprs ->
            inferCall range allExprs

        IfBlock exprCond exprIf exprElse ->
            inferIf range exprCond exprIf exprElse

        CaseExpression caseBlock ->
            inferCase range caseBlock

        LetExpression letBlock ->
            -- TODO add declarations to env
            infer range letBlock.expression

        RecordAccessFunction name ->
            inferAccessor range name

        RecordAccess recordExpr (Node _ name) ->
            inferAccess range recordExpr name

        RecordUpdateExpression (Node _ name) recordSetters ->
            inferUpdate range name recordSetters

        RecordExpr recordSetters ->
            inferRecord range recordSetters

        GLSLExpression _ ->
            -- TODO implement properly
            map Canonical.Type.shader <|
                traverse (map Var) [ freshVar, freshVar, freshVar ]



---- INFER LISTS


inferList : Range -> List (Node Expression) -> Infer Type
inferList range elementExprs =
    case elementExprs of
        [] ->
            map Var freshVar

        firstExpr :: rest ->
            let
                inferRest firstType =
                    traverse (infer range) rest
                        |> andThen (traverse (Tuple.pair firstType >> addConstraint))
                        |> map (\_ -> Canonical.Type.list firstType)
            in
            infer range firstExpr
                |> andThen inferRest



---- INFER TUPLES


inferTuple : Range -> List (Node Expression) -> Infer Type
inferTuple range exprs =
    map Tuple (traverse (infer range) exprs)



---- INFER BINOPS


inferBinops : Range -> String -> Node Expression -> Node Expression -> Infer Type
inferBinops range name exprA exprB =
    let
        ( binopApplications, finalExpr ) =
            collectOperatorApplications [] name exprA exprB
    in
    sortOperatorApplications binopApplications finalExpr
        |> andThen (inferBinop range)


collectOperatorApplications :
    List ( Node Expression, String )
    -> String
    -> Node Expression
    -> Node Expression
    -> ( List ( Node Expression, String ), Node Expression )
collectOperatorApplications collected name exprA exprB =
    case exprB of
        Node _ (OperatorApplication nextName _ nextExprA nextExprB) ->
            collectOperatorApplications (( exprA, name ) :: collected)
                nextName
                nextExprA
                nextExprB

        _ ->
            ( List.reverse (( exprA, name ) :: collected), exprB )


type BinopApp
    = BinopApp Type BinopApp BinopApp
    | Final (Node Expression)


sortOperatorApplications :
    List ( Node Expression, String )
    -> Node Expression
    -> Infer BinopApp
sortOperatorApplications binopApps finalExpr =
    case binopApps of
        [] ->
            return (Final finalExpr)

        ( expr, name ) :: rest ->
            let
                sort ( binop, infixType ) =
                    sortOperatorApplicationsHelp
                        (BinopApp infixType (Final expr))
                        binop.precedence
                        binop.associativity
                        rest
                        finalExpr
            in
            findInfix name
                |> andThen sort


sortOperatorApplicationsHelp :
    (BinopApp -> BinopApp)
    -> Int
    -> Canonical.Associativity
    -> List ( Node Expression, String )
    -> Node Expression
    -> Infer BinopApp
sortOperatorApplicationsHelp makeBinop rootPrecedence rootAssociativity middle finalExpr =
    case middle of
        [] ->
            return (makeBinop (Final finalExpr))

        ( expr, name ) :: rest ->
            let
                handlePrecedence ( binop, infixType ) =
                    if binop.precedence < rootPrecedence then
                        sortOperatorApplicationsHelp
                            (makeBinop << BinopApp infixType (Final expr))
                            binop.precedence
                            binop.associativity
                            rest
                            finalExpr

                    else if binop.precedence > rootPrecedence then
                        sortOperatorApplicationsHelp
                            (BinopApp infixType (makeBinop (Final expr)))
                            binop.precedence
                            binop.associativity
                            rest
                            finalExpr

                    else
                        case ( rootAssociativity, binop.associativity ) of
                            ( Left, Left ) ->
                                sortOperatorApplicationsHelp
                                    (BinopApp infixType (makeBinop (Final expr)))
                                    binop.precedence
                                    binop.associativity
                                    rest
                                    finalExpr

                            ( Right, Right ) ->
                                sortOperatorApplicationsHelp
                                    (makeBinop << BinopApp infixType (Final expr))
                                    binop.precedence
                                    binop.associativity
                                    rest
                                    finalExpr

                            _ ->
                                throwError ConflictingAssociativity
            in
            findInfix name
                |> andThen handlePrecedence


inferBinop : Range -> BinopApp -> Infer Type
inferBinop range binop =
    case binop of
        Final expr ->
            infer range expr

        BinopApp infixType binopA binopB ->
            let
                inferBinopB typeA =
                    inferBinop range binopB
                        |> andThen (getReturnVar typeA)

                getReturnVar typeA typeB =
                    freshVar
                        |> andThen (returnType typeA typeB)

                returnType typeA typeB returnVar =
                    addConstraint
                        ( infixType
                        , Lambda typeA (Lambda typeB (Var returnVar))
                        )
                        |> map (\_ -> Var returnVar)
            in
            inferBinop range binopA
                |> andThen inferBinopB



---- INFER LAMBDA


inferLambda : Range -> Elm.Syntax.Expression.Lambda -> Infer Type
inferLambda range lambda =
    let
        inferBody ( types, annotations ) =
            inEnvs (List.concat annotations)
                (infer range lambda.expression
                    |> map (returnType types)
                )

        returnType vars tipe =
            List.foldl Lambda tipe vars
    in
    traverse inferPattern lambda.args
        |> andThen (List.unzip >> inferBody)



---- INFER CALL


inferCall : Range -> List (Node Expression) -> Infer Type
inferCall range exprs =
    case exprs of
        [] ->
            throwError ParserError

        function :: arguments ->
            let
                inferArguments functionType =
                    traverse (infer range) arguments
                        |> andThen (getReturnVar functionType)

                getReturnVar functionType types =
                    freshVar
                        |> andThen (returnType functionType types)

                returnType functionType types var =
                    addConstraint
                        ( functionType
                        , List.foldr Lambda
                            (Var var)
                            types
                        )
                        |> map (\_ -> Var var)
            in
            infer range function
                |> andThen inferArguments



---- INFER IF EXPRESSIONS


inferIf : Range -> Node Expression -> Node Expression -> Node Expression -> Infer Type
inferIf range exprCond exprFirst exprSecond =
    let
        inferCond =
            infer range exprCond
                |> andThen inferFirst

        inferFirst typeCond =
            addConstraint ( typeCond, Canonical.Type.bool )
                |> andThen (\_ -> infer range exprFirst)
                |> andThen (inferSecond typeCond)

        inferSecond typeCond typeFirst =
            infer range exprSecond
                |> andThen (returnHelp typeCond typeFirst)

        returnHelp typeCond typeFirst typeSecond =
            addConstraint ( typeFirst, typeSecond )
                |> map (\_ -> typeFirst)
    in
    inferCond



---- INFER CASE EXPRESSIONS


inferCase : Range -> Elm.Syntax.Expression.CaseBlock -> Infer Type
inferCase range caseBlock =
    let
        inferCaseBranches exprTipe =
            traverse (inferCaseBranch range exprTipe) caseBlock.cases
                |> andThen addConstraints

        addConstraints types =
            case types of
                [] ->
                    throwError CaseWithoutBranches

                firstType :: rest ->
                    traverse (Tuple.pair firstType >> addConstraint) rest
                        |> map (\_ -> firstType)
    in
    infer range caseBlock.expression
        |> andThen inferCaseBranches


inferCaseBranch : Range -> Type -> Elm.Syntax.Expression.Case -> Infer Type
inferCaseBranch range exprType ( pattern, expr ) =
    let
        addConstraintHelp ( tipe, annotations ) =
            addConstraint ( exprType, tipe )
                |> andThen (inferBody annotations)

        inferBody annotations _ =
            inEnvs annotations
                (infer range expr)
    in
    inferPattern pattern
        |> andThen addConstraintHelp



---- INFER RECORD ACCESSORS


inferAccessor : Range -> String -> Infer Type
inferAccessor range name =
    let
        getFieldVar recordVar =
            freshVar
                |> andThen (returnType recordVar)

        returnType recordVar fieldVar =
            return <|
                Lambda
                    (Record [ ( String.dropLeft 1 name, Var fieldVar ) ]
                        (Just recordVar)
                    )
                    (Var fieldVar)
    in
    freshVar
        |> andThen getFieldVar



---- INFER RECORD FIELD ACCESS


inferAccess : Range -> Node Expression -> String -> Infer Type
inferAccess range recordExpr name =
    let
        getRecordVar recordType =
            freshVar
                |> andThen (getFieldVar recordType)

        getFieldVar recordType recordVar =
            freshVar
                |> andThen (returnType recordType recordVar)

        returnType recordType recordVar fieldVar =
            addConstraint
                ( recordType
                , Record [ ( name, Var fieldVar ) ] (Just recordVar)
                )
                |> map (\_ -> Var fieldVar)
    in
    infer range recordExpr
        |> andThen getRecordVar



---- INFER RECORD UPDATES


inferUpdate : Range -> String -> List (Node Elm.Syntax.Expression.RecordSetter) -> Infer Type
inferUpdate range name recordSetters =
    let
        inferRecordSetter (Node _ ( Node _ fieldName, fieldExpr )) =
            infer range fieldExpr
                |> map (Tuple.pair fieldName)

        inferRecordType fieldTypes =
            findValue name
                |> andThen (getFreshVar fieldTypes)

        getFreshVar fieldTypes recordType =
            freshVar
                |> andThen (returnType fieldTypes recordType)

        returnType fieldTypes recordType var =
            addConstraint
                ( recordType
                , Record fieldTypes (Just var)
                )
                |> map (\_ -> recordType)
    in
    traverse inferRecordSetter recordSetters
        |> andThen inferRecordType



---- INFER RECORD CONSTRUCTIONS


inferRecord : Range -> List (Node Elm.Syntax.Expression.RecordSetter) -> Infer Type
inferRecord range recordSetters =
    let
        inferSetter (Node _ ( Node _ name, valueExpr )) =
            infer range valueExpr
                |> map (Tuple.pair name)
    in
    traverse inferSetter recordSetters
        |> map (\namedTypes -> Record namedTypes Nothing)



---- INFER PATTERNS


inferPattern : Node Pattern -> Infer ( Type, List ( String, Annotation ) )
inferPattern (Node _ pattern) =
    case pattern of
        AllPattern ->
            freshVar
                |> map (\var -> ( Var var, [] ))

        UnitPattern ->
            return ( Tuple [], [] )

        CharPattern _ ->
            return ( Canonical.Type.char, [] )

        StringPattern _ ->
            return ( Canonical.Type.string, [] )

        IntPattern _ ->
            return ( Canonical.Type.int, [] )

        HexPattern _ ->
            return ( Canonical.Type.int, [] )

        FloatPattern _ ->
            return ( Canonical.Type.float, [] )

        TuplePattern patterns ->
            traverse inferPattern patterns
                |> map
                    (List.unzip
                        >> Tuple.mapFirst Tuple
                        >> Tuple.mapSecond List.concat
                    )

        RecordPattern names ->
            let
                toField (Node _ name) =
                    map (Tuple.pair name << Var) freshVar
            in
            map2
                (\fields rest ->
                    ( Record fields (Just rest)
                    , List.map (Tuple.mapSecond (ForAll [])) fields
                    )
                )
                (traverse toField names)
                freshVar

        UnConsPattern firstPattern secondPattern ->
            let
                inferSecondPattern ( firstType, firstAnnotation ) =
                    inferPattern secondPattern
                        |> andThen (returnType firstType firstAnnotation)

                returnType firstType firstAnnotation ( secondType, secondAnnotation ) =
                    addConstraint
                        ( secondType
                        , Canonical.Type.list firstType
                        )
                        |> map
                            (\_ ->
                                ( Canonical.Type.list firstType
                                , firstAnnotation ++ secondAnnotation
                                )
                            )
            in
            inferPattern firstPattern
                |> andThen inferSecondPattern

        ListPattern patterns ->
            let
                returnType ( types, annotations ) =
                    case types of
                        [] ->
                            freshVar
                                |> andThen
                                    (\var ->
                                        return
                                            ( Canonical.Type.list (Var var)
                                            , []
                                            )
                                    )

                        tipe :: _ ->
                            return
                                ( Canonical.Type.list tipe
                                , List.concat annotations
                                )
            in
            traverse inferPattern patterns
                |> andThen (List.unzip >> returnType)

        VarPattern name ->
            freshVar
                |> map
                    (\var ->
                        ( Var var
                        , [ ( name, ForAll [] (Var var) ) ]
                        )
                    )

        NamedPattern { moduleName, name } patterns ->
            freshVar
                |> andThen
                    (\var ->
                        findValue (Src.qualifiedName moduleName name)
                            |> andThen
                                (\constructorType ->
                                    traverse inferPattern patterns
                                        |> map List.unzip
                                        |> andThen
                                            (\( types, annotations ) ->
                                                addConstraint
                                                    ( constructorType
                                                    , List.foldl Lambda (Var var) types
                                                    )
                                                    |> map
                                                        (\_ ->
                                                            ( Var var
                                                            , List.concat annotations
                                                            )
                                                        )
                                            )
                                )
                    )

        AsPattern subPattern (Node _ name) ->
            inferPattern subPattern
                |> map
                    (\( tipe, annotations ) ->
                        ( tipe
                        , ( name, ForAll [] tipe ) :: annotations
                        )
                    )

        ParenthesizedPattern subPattern ->
            inferPattern subPattern



---- HELPER


findValue : String -> Infer Type
findValue name =
    Infer <|
        \env ->
            case Dict.get name env.values of
                Nothing ->
                    State.state
                        ( [], [], Err (UnboundVariable name) )

                Just annotation ->
                    let
                        (Infer run) =
                            instantiate annotation
                    in
                    run env


findInfix : String -> Infer ( Binop, Type )
findInfix name =
    Infer <|
        \env ->
            case Dict.get name env.binops of
                Nothing ->
                    State.state
                        ( [], [], Err (UnknownInfix name) )

                Just binop ->
                    let
                        (Infer run) =
                            instantiate binop.tipe
                    in
                    run env
                        |> State.map (Triple.mapThird (Result.map (Tuple.pair binop)))


freshVar : Infer String
freshVar =
    Infer <|
        \_ ->
            State.advance <|
                \count ->
                    ( ( [], [], Ok ("a" ++ String.fromInt count) )
                    , count + 1
                    )


inEnvs : List ( String, Annotation ) -> Infer a -> Infer a
inEnvs newAnnotations (Infer run) =
    Infer <|
        \env ->
            let
                extend ( name, annotation ) =
                    Dict.insert name annotation
            in
            run
                { env | values = List.foldl extend env.values newAnnotations }



---- INSTANTIATE AND GENERALIZE


instantiateTypeAnnotation : Node Src.TypeAnnotation -> Infer Type
instantiateTypeAnnotation typeAnnotation =
    let
        annotation =
            Infer <|
                \env ->
                    State.state
                        ( []
                        , []
                        , typeAnnotation
                            |> Canonical.canonicalizeTypeAnnotation
                                env.imports
                                env.moduleName
                            |> Canonical.Annotation.fromType
                            |> Ok
                        )
    in
    annotation
        |> andThen instantiate


instantiate : Annotation -> Infer Type
instantiate (ForAll vars tipe) =
    Infer <|
        \_ ->
            State.map
                (\subst -> ( [], [], Ok (Canonical.Type.apply subst tipe) ))
                (freshVars vars Dict.empty)


freshVars : List String -> Dict String Type -> State Int (Dict String Type)
freshVars vars subst =
    case vars of
        [] ->
            State.state subst

        var :: rest ->
            State.andThen (freshVars rest) <|
                State.advance <|
                    \count ->
                        ( Dict.insert var
                            (Var ("a" ++ String.fromInt count))
                            subst
                        , count + 1
                        )


generalize : Env -> Type -> Annotation
generalize env tipe =
    let
        newVars =
            Set.toList <|
                Set.diff
                    (Canonical.Type.freeTypeVars tipe)
                    envFreeTypeVars

        envFreeTypeVars =
            Set.union
                (Dict.foldl
                    (\_ -> Canonical.Annotation.freeTypeVars >> Set.union)
                    Set.empty
                    env.values
                )
                (Dict.foldl
                    (\_ values freeVars ->
                        Dict.foldl
                            (\_ -> Canonical.Annotation.freeTypeVars >> Set.union)
                            freeVars
                            values
                    )
                    Set.empty
                    env.qualifiedValues
                )
    in
    ForAll newVars tipe



---- GENERAL HELPER


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
            map List.reverse inferB

        a :: rest ->
            traverseHelp f rest <|
                map2 (::) (f a) inferB
