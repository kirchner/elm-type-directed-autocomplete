module Suggest exposing
    ( Expr
    , Generator
    , addUnions
    , addValues
    , all
    , cases
    , exprToString
    , for
    , recordUpdate
    , tuple
    , value
    , withArgument
    , withArguments
    )

import Dict exposing (Dict)
import Elm.Docs exposing (Alias, Module, Union)
import Elm.Type exposing (Type(..))
import State
import String.Extra as String
import Type exposing (Substitutions)


type Expr
    = Call String (List Expr)
    | UpdateRecord String (List ( String, Expr ))
    | CreateTuple Expr Expr
    | Case Expr (List ( String, Expr ))


exprToString : Expr -> String
exprToString expr =
    case expr of
        Call name args ->
            String.concat
                [ String.join " " (name :: List.map exprToString args) ]

        UpdateRecord name values ->
            if List.isEmpty values then
                name

            else
                let
                    valueToString ( fieldName, tipe ) =
                        fieldName ++ " = " ++ exprToString tipe
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
                , exprToString exprA
                , "\n, "
                , exprToString exprB
                , "\n)"
                ]

        Case matchedExpr branches ->
            let
                branchToString ( branch, branchExpr ) =
                    String.concat
                        [ branch
                        , " ->\n"
                        , indent (exprToString branchExpr)
                        ]
            in
            String.concat
                [ "case "
                , exprToString matchedExpr
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


type Generator
    = Generator (Type -> List Union -> Dict String Type -> List Expr)


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
    generator targetType [] Dict.empty


all : List Generator -> Generator
all generators =
    Generator <|
        \targetType unions values ->
            List.concatMap
                (\(Generator generator) ->
                    generator targetType unions values
                )
                generators


value : Generator
value =
    Generator <|
        \targetType unions values ->
            let
                ofTargetType name tipe collected =
                    case Type.unifier tipe targetType of
                        Nothing ->
                            collected

                        Just _ ->
                            name :: collected

                toCall name =
                    Call name []
            in
            values
                |> Dict.foldl ofTargetType []
                |> List.map toCall


withArgument : { first : Generator } -> Generator
withArgument generator =
    Generator <|
        \targetType unions values ->
            case targetType of
                Lambda from to ->
                    let
                        ofToType name tipe collected =
                            Type.unifiable targetType to
                                |> State.andThen suggestArgument
                                |> State.finalValue Type.noSubstitutions
                                |> List.map (toCall name)
                                |> List.append collected

                        suggestArgument isUnifiable =
                            if isUnifiable then
                                State.get
                                    |> State.map
                                        (\substitutions ->
                                            allSpecializationsOf values
                                                substitutions
                                                from
                                        )

                            else
                                State.state []

                        toCall name argument =
                            Call name [ argument ]
                    in
                    Dict.foldl ofToType [] values

                _ ->
                    []


withArguments : { first : Generator, second : Generator } -> Generator
withArguments generator =
    Generator <|
        \targetType unions values ->
            case targetType of
                Lambda fromA (Lambda fromB to) ->
                    let
                        suggestValue name tipe collected =
                            Type.unifiable targetType to
                                |> State.andThen (suggestArgumentFor fromA)
                                |> State.andThen
                                    (\namesA ->
                                        State.get
                                            |> State.map
                                                (\substitutions ->
                                                    allSpecializationsOf values
                                                        substitutions
                                                        fromB
                                                )
                                            |> State.map (Tuple.pair namesA)
                                    )
                                |> State.finalValue Type.noSubstitutions
                                |> toCalls name
                                |> List.append collected

                        suggestArgumentFor from isUnifiable =
                            if isUnifiable then
                                State.get
                                    |> State.map
                                        (\substitutions ->
                                            allSpecializationsOf values
                                                substitutions
                                                from
                                        )

                            else
                                State.state []

                        toCalls name ( argumentsA, argumentsB ) =
                            List.map
                                (\argumentA ->
                                    List.map
                                        (\argumentB ->
                                            Call name [ argumentA, argumentB ]
                                        )
                                        argumentsB
                                )
                                argumentsA
                                |> List.concat
                    in
                    Dict.foldl suggestValue [] values

                _ ->
                    []


allSpecializationsOf : Dict String Type -> Substitutions -> Type -> List Expr
allSpecializationsOf knownValues substitutions targetType =
    let
        checkKnownValue name knownType names =
            Type.unifiable knownType targetType
                |> State.map
                    (\isUnifiable ->
                        if isUnifiable then
                            Call name [] :: names

                        else
                            names
                    )
                |> State.finalValue substitutions
    in
    Dict.foldl checkKnownValue [] knownValues


recordUpdate : { field : Generator } -> Generator
recordUpdate generator =
    Generator <|
        \targetType unions values ->
            case targetType of
                Record fields var ->
                    let
                        initialRecords =
                            values
                                |> Dict.toList
                                |> List.filterMap (ofType targetType)

                        updateField ( field, tipe ) collected =
                            fieldGenerator tipe unions values
                                |> List.map (Tuple.pair field)
                                |> List.append collected

                        ofType tipe ( name, otherType ) =
                            case Type.unifier otherType tipe of
                                Nothing ->
                                    Nothing

                                Just _ ->
                                    Just name

                        toRecordUpdate initialRecord ( field, tipe ) =
                            UpdateRecord initialRecord [ ( field, tipe ) ]

                        (Generator fieldGenerator) =
                            generator.field
                    in
                    initialRecords
                        |> List.concatMap
                            (\initialRecord ->
                                fields
                                    |> List.foldl updateField []
                                    |> List.map (toRecordUpdate initialRecord)
                            )

                _ ->
                    []


tuple : { first : Generator, second : Generator } -> Generator
tuple generator =
    Generator <|
        \targetType unions values ->
            case targetType of
                Tuple (typeA :: typeB :: []) ->
                    let
                        toTuple exprA =
                            firstGenerator typeB unions values
                                |> List.map (CreateTuple exprA)

                        (Generator firstGenerator) =
                            generator.first

                        (Generator secondGenerator) =
                            generator.second
                    in
                    secondGenerator typeA unions values
                        |> List.concatMap toTuple

                _ ->
                    []


cases :
    { matched : Generator
    , branch : Dict String Type -> Generator
    }
    -> Generator
cases generator =
    Generator <|
        \targetType unions values ->
            let
                suggestCase union =
                    valuesOfUnion union
                        |> List.map
                            (\( name, _ ) ->
                                List.map suggestBranch union.tags
                                    |> Case (Call name [])
                            )

                valuesOfUnion union =
                    List.filter (valueOfUnion union) (Dict.toList values)

                valueOfUnion union ( _, tipe ) =
                    case tipe of
                        Type name _ ->
                            union.name == name

                        _ ->
                            False

                suggestBranch ( name, subTypes ) =
                    ( if List.isEmpty subTypes then
                        name

                      else
                        String.join " "
                            (name
                                :: List.map String.decapitalize
                                    (List.map typeName subTypes)
                            )
                    , generator.branch (toNewValues subTypes)
                        |> for targetType
                        |> List.head
                        |> Maybe.withDefault
                            (Call "Debug.todo" [ Call "\"implement\"" [] ])
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
                    List.concatMap suggestCase unions

                Tuple _ ->
                    List.concatMap suggestCase unions

                Record _ _ ->
                    List.concatMap suggestCase unions

                _ ->
                    []
