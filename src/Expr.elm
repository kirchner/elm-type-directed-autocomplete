module Expr exposing
    ( Expr
    , suggest
    , toString
    )

import Dict exposing (Dict)
import Elm.Docs exposing (Module)
import Elm.Type exposing (Type(..))
import List.Extra as List
import Set
import State exposing (State)
import Type


usefulConstants : Dict String Type
usefulConstants =
    Dict.fromList
        [ ( "0", Type "Int" [] )
        , ( "\"\"", Type "String" [] )
        ]


type Expr
    = Call String (List String)
    | UpdateRecord String (List ( String, String ))
    | CreateTuple Expr Expr


toString : Expr -> String
toString expr =
    case expr of
        Call name args ->
            String.concat
                [ String.join " " (name :: args) ]

        UpdateRecord name values ->
            if List.isEmpty values then
                name

            else
                let
                    valueToString ( fieldName, value ) =
                        fieldName ++ " = " ++ value
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
                , toString exprA
                , "\n, "
                , toString exprB
                , "\n)"
                ]


suggest : Dict String Type -> Type -> List Expr
suggest knownValues targetType =
    let
        usedKnownValues =
            knownValues
                |> Dict.filter
                    (\name _ ->
                        not
                            (List.member name
                                [ "Basics.identity"
                                , "Basics.always"
                                , "Debug.todo"
                                , "Debug.toString"
                                , "Debug.log"
                                ]
                            )
                    )
                |> Dict.map (always removeScope)
                |> Dict.union usefulConstants
    in
    List.concat
        [ suggestHelp usedKnownValues targetType
        ]


suggestHelp : Dict String Type -> Type -> List Expr
suggestHelp knownValues targetType =
    List.concat
        [ suggestDirect knownValues targetType
        , suggestWithArgument knownValues targetType
        , suggestWithTwoArguments knownValues targetType
        , suggestRecordUpdate knownValues targetType
        , suggestCreateTuple knownValues targetType
        ]


suggestDirect : Dict String Type -> Type -> List Expr
suggestDirect knownValues targetType =
    allGeneralizationsOf knownValues Dict.empty targetType
        |> List.map (\name -> Call name [])


suggestWithArgument : Dict String Type -> Type -> List Expr
suggestWithArgument knownValues targetType =
    let
        checkKnownValue name knownType exprs =
            case knownType of
                Lambda from to ->
                    to
                        |> Type.isGeneralizationOf targetType
                        |> State.andThen
                            (\toIsGeneralization ->
                                if toIsGeneralization then
                                    State.get
                                        |> State.map
                                            (\setVars ->
                                                allSpecializationsOf knownValues
                                                    setVars
                                                    from
                                            )

                                else
                                    State.state []
                            )
                        |> State.finalValue Dict.empty
                        |> List.map (\calledName -> Call name [ calledName ])
                        |> List.append exprs

                _ ->
                    exprs
    in
    Dict.foldl checkKnownValue [] knownValues


suggestWithTwoArguments : Dict String Type -> Type -> List Expr
suggestWithTwoArguments knownValues targetType =
    let
        checkKnownValue name knownType exprs =
            case knownType of
                Lambda fromA (Lambda fromB to) ->
                    let
                        ( namesA, namesB ) =
                            to
                                |> Type.isGeneralizationOf targetType
                                |> State.andThen
                                    (\toIsGeneralization ->
                                        if toIsGeneralization then
                                            State.get
                                                |> State.map
                                                    (\setVars ->
                                                        allSpecializationsOf knownValues
                                                            setVars
                                                            fromA
                                                    )

                                        else
                                            State.state []
                                    )
                                |> State.andThen
                                    (\namesA_ ->
                                        State.get
                                            |> State.map
                                                (\setVars ->
                                                    allSpecializationsOf knownValues
                                                        setVars
                                                        fromB
                                                )
                                            |> State.map (Tuple.pair namesA_)
                                    )
                                |> State.finalValue Dict.empty
                    in
                    List.map
                        (\nameA ->
                            List.map
                                (\nameB ->
                                    Call name [ nameA, nameB ]
                                )
                                namesB
                        )
                        namesA
                        |> List.concat
                        |> List.append exprs

                _ ->
                    exprs
    in
    Dict.foldl checkKnownValue [] knownValues


suggestRecordUpdate : Dict String Type -> Type -> List Expr
suggestRecordUpdate knownValues targetType =
    case targetType of
        Record values var ->
            knownValues
                |> Dict.toList
                |> List.filterMap
                    (\( name, knownValue ) ->
                        if knownValue == targetType then
                            Just name

                        else
                            Nothing
                    )
                |> List.map
                    (\initialValue ->
                        values
                            |> List.foldl
                                (\( value, type_ ) updates ->
                                    knownValues
                                        |> Dict.toList
                                        |> List.filterMap
                                            (\( name, knownType ) ->
                                                if knownType == type_ then
                                                    Just ( value, name )

                                                else
                                                    Nothing
                                            )
                                        |> List.append updates
                                )
                                []
                            |> List.map
                                (\( value, name ) ->
                                    UpdateRecord initialValue [ ( value, name ) ]
                                )
                    )
                |> List.concat

        _ ->
            []


suggestCreateTuple : Dict String Type -> Type -> List Expr
suggestCreateTuple knownValues targetType =
    case targetType of
        Tuple (typeA :: typeB :: []) ->
            List.concatMap
                (\exprA ->
                    List.map (CreateTuple exprA)
                        (suggestHelp knownValues typeB)
                )
                (suggestHelp knownValues typeA)

        _ ->
            []



---- HELPER


allGeneralizationsOf : Dict String Type -> Dict String Type -> Type -> List String
allGeneralizationsOf knownValues setVars targetType =
    let
        checkKnownValue name knownType names =
            if
                knownType
                    |> Type.isGeneralizationOf targetType
                    |> State.finalValue setVars
            then
                name :: names

            else
                names
    in
    Dict.foldl checkKnownValue [] knownValues


allSpecializationsOf : Dict String Type -> Dict String Type -> Type -> List String
allSpecializationsOf knownValues setVars targetType =
    let
        checkKnownValue name knownType names =
            targetType
                |> Type.isGeneralizationOf knownType
                |> State.map
                    (\otherKnownTypeIsGeneralization ->
                        if otherKnownTypeIsGeneralization then
                            name :: names

                        else
                            names
                    )
                |> State.finalValue setVars
    in
    Dict.foldl checkKnownValue [] knownValues


removeScope : Type -> Type
removeScope scopedType =
    case scopedType of
        Var name ->
            Var name

        Type name subTypes ->
            Type
                (String.split "." name
                    |> List.reverse
                    |> List.head
                    |> Maybe.withDefault name
                )
                (List.map removeScope subTypes)

        Lambda typeA typeB ->
            Lambda (removeScope typeA) (removeScope typeB)

        Tuple types ->
            Tuple (List.map removeScope types)

        Record values var ->
            Record
                (List.map (Tuple.mapSecond removeScope) values)
                var
