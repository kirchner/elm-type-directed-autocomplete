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


toString : Expr -> String
toString expr =
    case expr of
        Call name args ->
            String.concat
                [ String.join " " (name :: args) ]


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
        [ suggestDirect usedKnownValues targetType
        , suggestWithArgument usedKnownValues targetType
        , suggestWithTwoArguments usedKnownValues targetType
        ]


suggestDirect : Dict String Type -> Type -> List Expr
suggestDirect knownValues targetType =
    let
        checkKnownValue name knownType exprs =
            if
                knownType
                    |> Type.isGeneralizationOf targetType
                    |> State.finalValue Dict.empty
            then
                Call name [] :: exprs

            else
                exprs
    in
    Dict.foldl checkKnownValue [] knownValues


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
                                                from
                                                    |> lessGeneralValues knownValues setVars
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
                                                        fromA
                                                            |> lessGeneralValues knownValues setVars
                                                    )

                                        else
                                            State.state []
                                    )
                                |> State.andThen
                                    (\namesA_ ->
                                        State.get
                                            |> State.map
                                                (\setVars ->
                                                    fromB
                                                        |> lessGeneralValues knownValues setVars
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


lessGeneralValues : Dict String Type -> Dict String Type -> Type -> List String
lessGeneralValues knownValues setVars targetType =
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



---- HELPER


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
