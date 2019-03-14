module Suggest exposing
    ( Expr
    , Generator
    , addUnions
    , addValues
    , all
    , call
    , cases
    , exprToString
    , for
    , recordUpdate
    , tuple
    , value
    )

import Dict exposing (Dict)
import Elm.Docs exposing (Alias, Module, Union)
import Elm.Type exposing (Type(..))
import State exposing (State)
import String.Extra as String
import Type exposing (Substitutions)


type Expr
    = Call String (List Expr)
    | UpdateRecord String (List ( String, Expr ))
    | CreateTuple Expr Expr
    | Case Expr (List ( String, Expr ))


exprToString : Expr -> String
exprToString expr =
    exprToStringHelp False expr


exprToStringHelp : Bool -> Expr -> String
exprToStringHelp isArgument expr =
    case expr of
        Call name args ->
            let
                callString =
                    String.concat
                        [ String.join " " (name :: List.map (exprToStringHelp True) args) ]
            in
            if isArgument && List.length args >= 1 then
                String.concat
                    [ "("
                    , callString
                    , ")"
                    ]

            else
                callString

        UpdateRecord name values ->
            if List.isEmpty values then
                name

            else
                let
                    valueToString ( fieldName, tipe ) =
                        fieldName ++ " = " ++ exprToStringHelp False tipe
                in
                String.concat
                    [ "{ "
                    , name
                    , " | "
                    , String.join " , " <|
                        List.map valueToString values
                    , " }"
                    ]

        CreateTuple exprA exprB ->
            String.concat
                [ "( "
                , exprToStringHelp False exprA
                , "\n, "
                , exprToStringHelp False exprB
                , "\n)"
                ]

        Case matchedExpr branches ->
            let
                branchToString ( branch, branchExpr ) =
                    String.concat
                        [ branch
                        , " ->\n"
                        , indent (exprToStringHelp False branchExpr)
                        ]
            in
            String.concat
                [ "case "
                , exprToStringHelp False matchedExpr
                , " of\n"
                , indent <|
                    String.join "\n\n" <|
                        List.map branchToString branches
                ]


indent : String -> String
indent text =
    text
        |> String.lines
        |> List.map (\line -> "    " ++ line)
        |> String.join "\n"



---- GENERATOR


type Generator
    = Generator
        (Type
         -> List Union
         -> Dict String Type
         -> Substitutions
         -> List ( Expr, Substitutions )
        )


addUnions : List Union -> Generator -> Generator
addUnions newUnions (Generator generator) =
    Generator <|
        \targetType unions values ->
            generator targetType (newUnions ++ unions) values


addValues : Dict String Type -> Generator -> Generator
addValues newValues (Generator generator) =
    Generator <|
        \targetType unions values ->
            generator targetType unions (Dict.union newValues values)


for : Type -> Generator -> List Expr
for targetType (Generator generator) =
    generator targetType [] Dict.empty Type.noSubstitutions
        |> List.map Tuple.first


all : List Generator -> Generator
all generators =
    Generator <|
        \targetType unions values substitutions ->
            List.concatMap
                (\(Generator generator) ->
                    generator targetType unions values substitutions
                )
                generators


value : Generator
value =
    Generator <|
        \targetType unions values substitutions ->
            let
                ofTargetType name tipe collected =
                    Type.unifiable tipe targetType
                        |> State.run substitutions
                        |> collect name collected

                collect name collected ( isUnifiable, nextSubstitutions ) =
                    if isUnifiable then
                        ( Call name [], nextSubstitutions )
                            :: collected

                    else
                        collected
            in
            values
                |> Dict.foldl ofTargetType []


call : List Generator -> Generator
call generators =
    Generator <|
        \targetType unions values substitutions ->
            let
                ofTargetType name tipe collected =
                    case ofTargetTypeHelp [] generators tipe of
                        Nothing ->
                            collected

                        Just ( froms, to ) ->
                            ( name, froms, to ) :: collected

                ofTargetTypeHelp froms args tipe =
                    case ( args, tipe ) of
                        ( [], _ ) ->
                            let
                                ( isUnifiable, nextSubstitutions ) =
                                    Type.unifiable tipe targetType
                                        |> State.run substitutions
                            in
                            if isUnifiable then
                                Just ( froms, nextSubstitutions )

                            else
                                Nothing

                        ( firstArg :: restArgs, Lambda from to ) ->
                            ofTargetTypeHelp
                                (( from, firstArg ) :: froms)
                                restArgs
                                to

                        _ ->
                            Nothing

                toCall ( name, froms, nextSubstitutions ) =
                    suggestArguments froms nextSubstitutions
                        |> List.filterMap
                            (\( arguments, finalSubstitutions ) ->
                                if
                                    List.length arguments
                                        == List.length generators
                                then
                                    Just
                                        ( Call name (List.reverse arguments)
                                        , finalSubstitutions
                                        )

                                else
                                    Nothing
                            )

                suggestArguments froms nextSubstitutions =
                    suggestArgumentsHelp froms nextSubstitutions []

                suggestArgumentsHelp froms nextSubstitutions revArguments =
                    case froms of
                        [] ->
                            [ ( [], Type.noSubstitutions ) ]

                        ( firstFrom, Generator fromGenerator ) :: restFroms ->
                            fromGenerator firstFrom unions values nextSubstitutions
                                |> List.concatMap
                                    (\( firstArgumentExpr, nextNextSubstitutions ) ->
                                        suggestArgumentsHelp restFroms
                                            nextNextSubstitutions
                                            (firstArgumentExpr :: revArguments)
                                            |> List.map
                                                (\( restArgumentExprs, finalSubstitutions ) ->
                                                    ( firstArgumentExpr :: restArgumentExprs
                                                    , finalSubstitutions
                                                    )
                                                )
                                    )
            in
            values
                |> Dict.foldl ofTargetType []
                |> List.concatMap toCall


tuple : { first : Generator, second : Generator } -> Generator
tuple generator =
    Generator <|
        \targetType unions values substitutions ->
            case targetType of
                Tuple (typeA :: typeB :: []) ->
                    let
                        (Generator firstGenerator) =
                            generator.first

                        (Generator secondGenerator) =
                            generator.second

                        toTuple ( exprA, nextSubstitutions ) =
                            secondGenerator typeB unions values nextSubstitutions
                                |> List.map (Tuple.mapFirst (CreateTuple exprA))
                    in
                    firstGenerator typeA unions values substitutions
                        |> List.concatMap toTuple

                _ ->
                    []


recordUpdate : Generator -> Generator
recordUpdate generator =
    Generator <|
        \targetType unions values substitutions ->
            case targetType of
                Record fields var ->
                    let
                        (Generator fieldGenerator) =
                            generator

                        (Generator initialRecordGenerator) =
                            value

                        ofTargetType name tipe collected =
                            Type.unifiable tipe targetType
                                |> State.run substitutions
                                |> collect name collected

                        collect name collected ( isUnifiable, nextSubstitutions ) =
                            if isUnifiable then
                                ( name, nextSubstitutions ) :: collected

                            else
                                collected

                        toRecordUpdate ( name, nextSubstitutions ) =
                            fields
                                |> List.foldl (updateField nextSubstitutions) []
                                |> List.map
                                    (\( field, ( tipe, finalSubstitutions ) ) ->
                                        ( UpdateRecord name [ ( field, tipe ) ]
                                        , finalSubstitutions
                                        )
                                    )

                        updateField nextSubstitutions ( field, tipe ) collected =
                            fieldGenerator tipe unions values nextSubstitutions
                                |> List.map (Tuple.pair field)
                                |> List.append collected
                    in
                    values
                        |> Dict.foldl ofTargetType []
                        |> List.concatMap toRecordUpdate

                _ ->
                    []


cases :
    { matched : Generator
    , branch : Dict String Type -> Generator
    }
    -> Generator
cases generator =
    Generator <|
        \targetType unions values substitutions ->
            let
                (Generator matchedGenerator) =
                    generator.matched

                matchedValues =
                    unions
                        |> List.concatMap suggestMatched
                        |> List.concatMap suggestCase
                        |> List.map (\expr -> ( expr, Type.noSubstitutions ))

                suggestMatched union =
                    matchedGenerator (Type union.name (List.map Var union.args))
                        unions
                        values
                        Type.noSubstitutions
                        |> List.map (Tuple.first >> Tuple.pair union.tags)

                suggestCase ( tags, matched ) =
                    tags
                        |> List.map suggestBranch
                        |> namedCombinations
                        |> List.map (Case matched)

                suggestBranch ( name, subTypes ) =
                    let
                        (Generator branchGenerator) =
                            generator.branch (toNewValues subTypes)

                        branch =
                            if List.isEmpty subTypes then
                                name

                            else
                                String.join " "
                                    (name
                                        :: List.map String.decapitalize
                                            (List.map typeName subTypes)
                                    )
                    in
                    ( branch
                    , branchGenerator targetType unions values Type.noSubstitutions
                        |> List.map Tuple.first
                    )

                typeName tipe =
                    case tipe of
                        Type name _ ->
                            name

                        _ ->
                            "a"

                toNewValues types =
                    types
                        |> List.map toNewValue
                        |> Dict.fromList

                toNewValue tipe =
                    ( String.decapitalize (typeName tipe)
                    , tipe
                    )
            in
            case targetType of
                Type _ _ ->
                    matchedValues

                Tuple _ ->
                    matchedValues

                Record _ _ ->
                    matchedValues

                _ ->
                    []



---- HELPER


namedCombinations : List ( String, List a ) -> List (List ( String, a ))
namedCombinations lists =
    case lists of
        [] ->
            [ [] ]

        ( name, first ) :: rest ->
            first
                |> List.concatMap
                    (\a ->
                        namedCombinations rest
                            |> List.map
                                (\combination ->
                                    ( name, a ) :: combination
                                )
                    )
