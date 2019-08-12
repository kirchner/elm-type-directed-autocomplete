module Inference exposing
    ( Error(..)
    , errorToString
    , inferHole
    )

import Dict exposing (Dict)
import Elm.Docs exposing (Alias)
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Infix exposing (Infix, InfixDirection(..))
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
    { function : Elm.Syntax.Expression.Function
    , range : Range
    , binops : Dict String ( Infix, Type )
    , values : Dict String Type
    , aliases : List Alias
    }
    -> Result Error ( Type, Dict String Type )
inferHole { binops, aliases, values, range, function } =
    let
        ( constraints, holes, result ) =
            runInfer aliases binops initialEnv range function

        initialEnv =
            values
                |> Dict.toList
                |> TypeEnv.fromValues
    in
    case result of
        Err error ->
            Err error

        Ok _ ->
            let
                solve ( _, tipe, env ) =
                    Solver.run constraints
                        |> Result.mapError CouldNotSolve
                        |> Result.map (substitute tipe env)

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
    = Infer (Binops -> TypeEnv -> State Int ( List Constraint, List Hole, Result Error a ))


type alias Binops =
    Dict String ( Infix, Type )


type alias Hole =
    ( String, Type, TypeEnv )


storeHole : String -> Type -> Infer ()
storeHole name tipe =
    Infer <|
        \_ env ->
            State.state ( [], [ ( name, tipe, env ) ], Ok () )


type alias Constraint =
    ( Type, Type )


addConstraint : Constraint -> Infer ()
addConstraint constraint =
    Infer (\_ _ -> State.state ( [ constraint ], [], Ok () ))


runInfer :
    List Alias
    -> Binops
    -> TypeEnv
    -> Range
    -> Elm.Syntax.Expression.Function
    -> ( List Constraint, List Hole, Result Error Type )
runInfer typeAliases binops env range expr =
    let
        (Infer run) =
            inferFunction typeAliases range expr
    in
    State.finalValue 0 (run binops env)



---- INFER TOP-LEVEL FUNCTIONS


inferFunction : List Alias -> Range -> Elm.Syntax.Expression.Function -> Infer Type
inferFunction typeAliases range function =
    let
        (Node _ declaration) =
            function.declaration

        inferHelp args argTypes schemes =
            case args of
                [] ->
                    inEnvs schemes
                        (infer range declaration.expression
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



---- INFER EXPRESSIONS


infer : Range -> Node Expression -> Infer Type
infer range (Node currentRange expr) =
    case expr of
        FunctionOrValue moduleName name ->
            if range == currentRange then
                storeHole name (Var name)
                    |> map (\_ -> Var name)

            else if name == "True" then
                return (Type "Bool" [])

            else if name == "False" then
                return (Type "Bool" [])

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
            return (Type "String" [])

        CharLiteral _ ->
            return (Type "Char" [])

        Integer _ ->
            return (Type "Int" [])

        Hex _ ->
            return (Type "Int" [])

        Floatable _ ->
            return (Type "Float" [])

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
            map (Type "Shader") <|
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
                    rest
                        |> traverse (infer range)
                        |> andThen (traverse (Tuple.pair firstType >> addConstraint))
                        |> map (\_ -> Type "List" [ firstType ])
            in
            infer range firstExpr
                |> andThen inferRest



---- INFER TUPLES


inferTuple : Range -> List (Node Expression) -> Infer Type
inferTuple range allExprs =
    let
        inferHelp exprs types =
            case exprs of
                [] ->
                    return (Tuple (List.reverse types))

                firstExpr :: rest ->
                    infer range firstExpr
                        |> andThen (\tipe -> inferHelp rest (tipe :: types))
    in
    inferHelp allExprs []



---- INFER BINOPS


inferBinops : Range -> String -> Node Expression -> Node Expression -> Infer Type
inferBinops range name exprA exprB =
    let
        ( binops, finalExpr ) =
            collectOperatorApplications [] name exprA exprB
    in
    sortOperatorApplications binops finalExpr
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


type Binop
    = Binop Type Binop Binop
    | Final (Node Expression)


sortOperatorApplications :
    List ( Node Expression, String )
    -> Node Expression
    -> Infer Binop
sortOperatorApplications binops finalExpr =
    case binops of
        [] ->
            return (Final finalExpr)

        ( expr, name ) :: rest ->
            findInfix name
                |> andThen
                    (\( infix, infixType ) ->
                        let
                            (Node _ precedence) =
                                infix.precedence

                            (Node _ associativity) =
                                infix.direction
                        in
                        sortOperatorApplicationsHelp
                            (Binop infixType (Final expr))
                            precedence
                            associativity
                            rest
                            finalExpr
                    )


sortOperatorApplicationsHelp :
    (Binop -> Binop)
    -> Int
    -> InfixDirection
    -> List ( Node Expression, String )
    -> Node Expression
    -> Infer Binop
sortOperatorApplicationsHelp makeBinop rootPrecedence rootAssociativity middle finalExpr =
    case middle of
        [] ->
            return (makeBinop (Final finalExpr))

        ( expr, name ) :: rest ->
            let
                handlePrecedence ( infix, infixType ) =
                    let
                        (Node _ precedence) =
                            infix.precedence

                        (Node _ associativity) =
                            infix.direction
                    in
                    if precedence < rootPrecedence then
                        sortOperatorApplicationsHelp
                            (makeBinop << Binop infixType (Final expr))
                            precedence
                            associativity
                            rest
                            finalExpr

                    else if precedence > rootPrecedence then
                        sortOperatorApplicationsHelp
                            (Binop infixType (makeBinop (Final expr)))
                            precedence
                            associativity
                            rest
                            finalExpr

                    else
                        case ( rootAssociativity, associativity ) of
                            ( Left, Left ) ->
                                sortOperatorApplicationsHelp
                                    (\right ->
                                        Binop infixType (makeBinop (Final expr)) right
                                    )
                                    precedence
                                    associativity
                                    rest
                                    finalExpr

                            ( Right, Right ) ->
                                sortOperatorApplicationsHelp
                                    (\right ->
                                        makeBinop (Binop infixType (Final expr) right)
                                    )
                                    precedence
                                    associativity
                                    rest
                                    finalExpr

                            _ ->
                                throwError ConflictingAssociativity
            in
            findInfix name
                |> andThen handlePrecedence


inferBinop : Range -> Binop -> Infer Type
inferBinop range binop =
    case binop of
        Final expr ->
            infer range expr

        Binop infixType binopA binopB ->
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
        inferBody ( types, schemes ) =
            inEnvs (List.concat schemes)
                (infer range lambda.expression
                    |> map (returnType types)
                )

        returnType vars tipe =
            List.foldl Lambda tipe vars
    in
    traverse inferPattern lambda.args
        |> map List.unzip
        |> andThen inferBody



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
                        , List.foldl Lambda
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
            addConstraint ( typeCond, Type "Bool" [] )
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
        addConstraintHelp ( tipe, schemes ) =
            addConstraint ( exprType, tipe )
                |> andThen (inferBody schemes)

        inferBody schemes _ =
            inEnvs schemes
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
    recordSetters
        |> traverse inferRecordSetter
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


inferPattern : Node Pattern -> Infer ( Type, List ( String, Scheme ) )
inferPattern (Node _ pattern) =
    case pattern of
        AllPattern ->
            freshVar
                |> map (\var -> ( Var var, [] ))

        UnitPattern ->
            return ( Tuple [], [] )

        CharPattern _ ->
            return ( Type "Char" [], [] )

        StringPattern _ ->
            return ( Type "String" [], [] )

        IntPattern _ ->
            return ( Type "Int" [], [] )

        HexPattern _ ->
            return ( Type "Int" [], [] )

        FloatPattern _ ->
            return ( Type "Float" [], [] )

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
            let
                toField (Node _ name) =
                    map (Tuple.pair name << Var) freshVar
            in
            map2
                (\fields rest ->
                    ( Record fields (Just rest)
                    , List.map
                        (Tuple.mapSecond (ForAll []))
                        fields
                    )
                )
                (traverse toField names)
                freshVar

        UnConsPattern firstPattern secondPattern ->
            inferPattern firstPattern
                |> andThen
                    (\( firstType, firstScheme ) ->
                        inferPattern secondPattern
                            |> andThen
                                (\( secondType, secondScheme ) ->
                                    addConstraint
                                        ( secondType
                                        , Type "List" [ firstType ]
                                        )
                                        |> map
                                            (\_ ->
                                                ( Type "List" [ firstType ]
                                                , firstScheme ++ secondScheme
                                                )
                                            )
                                )
                    )

        ListPattern patterns ->
            traverse inferPattern patterns
                |> map List.unzip
                |> andThen
                    (\( types, schemes ) ->
                        case types of
                            [] ->
                                freshVar
                                    |> andThen
                                        (\var ->
                                            return
                                                ( Type "List" [ Var var ]
                                                , []
                                                )
                                        )

                            tipe :: _ ->
                                return
                                    ( Type "List" [ tipe ]
                                    , List.concat schemes
                                    )
                    )

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
                                            (\( types, schemes ) ->
                                                addConstraint
                                                    ( constructorType
                                                    , List.foldl Lambda (Var var) types
                                                    )
                                                    |> map
                                                        (\_ ->
                                                            ( Var var
                                                            , List.concat schemes
                                                            )
                                                        )
                                            )
                                )
                    )

        AsPattern subPattern (Node _ name) ->
            inferPattern subPattern
                |> map
                    (\( tipe, schemes ) ->
                        ( tipe
                        , ( name, ForAll [] tipe ) :: schemes
                        )
                    )

        ParenthesizedPattern subPattern ->
            inferPattern subPattern



---- HELPER


findValue : String -> Infer Type
findValue name =
    Infer <|
        \binops env ->
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
                    run binops env


findInfix : String -> Infer ( Infix, Type )
findInfix name =
    Infer <|
        \binops env ->
            case Dict.get name binops of
                Nothing ->
                    State.state
                        ( []
                        , []
                        , Err (UnknownInfix name)
                        )

                Just ( infix, infixType ) ->
                    let
                        (Infer run) =
                            instantiate <|
                                ForAll (Set.toList (Type.freeTypeVars infixType)) infixType

                        mapThird f ( a, b, c ) =
                            ( a, b, f c )
                    in
                    run binops env
                        |> State.map (mapThird (Result.map (Tuple.pair infix)))


freshVar : Infer String
freshVar =
    Infer <|
        \_ _ ->
            State.advance <|
                \count ->
                    ( ( [], [], Ok ("a" ++ String.fromInt count) )
                    , count + 1
                    )


inEnv : ( String, Scheme ) -> Infer a -> Infer a
inEnv ( var, scheme ) (Infer run) =
    Infer <|
        \binops env ->
            run binops (TypeEnv.extend var scheme env)


inEnvs : List ( String, Scheme ) -> Infer a -> Infer a
inEnvs newSchemes (Infer run) =
    Infer <|
        \binops env ->
            let
                extend ( name, scheme ) =
                    TypeEnv.extend name scheme
            in
            run binops (List.foldl extend env newSchemes)


return : a -> Infer a
return a =
    Infer (\_ _ -> State.state ( [], [], Ok a ))


throwError : Error -> Infer a
throwError error =
    Infer (\_ _ -> State.state ( [], [], Err error ))


map : (a -> b) -> Infer a -> Infer b
map f (Infer run) =
    Infer (\binops env -> State.map (Triple.mapThird (Result.map f)) (run binops env))


map2 : (a -> b -> c) -> Infer a -> Infer b -> Infer c
map2 f (Infer runA) (Infer runB) =
    Infer <|
        \binops env ->
            State.map2
                (\( logsA, holesA, resultA ) ( logsB, holesB, resultB ) ->
                    ( logsA ++ logsB
                    , holesA ++ holesB
                    , Result.map2 f resultA resultB
                    )
                )
                (runA binops env)
                (runB binops env)


andThen : (a -> Infer b) -> Infer a -> Infer b
andThen f (Infer runA) =
    Infer <|
        \binops env ->
            runA binops env
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
                                runB binops env
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
        \_ _ ->
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
