module Expr exposing
    ( Expr
    , suggestCreateCase
    , suggestCreateTuple
    , suggestDirect
    , suggestRecordUpdate
    , suggestWithArgument
    , suggestWithTwoArguments
    , toString
    )

{-

   Copyright 2019 Fabian Kirchner

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

-}

import Dict exposing (Dict)
import Elm.Docs exposing (Alias, Module, Union)
import Elm.Type exposing (Type(..))
import List.Extra as List
import Set
import State exposing (State)
import String.Extra as String
import Type exposing (Substitutions)


type Expr
    = Call String (List String)
    | UpdateRecord String (List ( String, String ))
    | CreateTuple Expr Expr
    | Case Expr (List ( String, Expr ))


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

        Case matchedExpr branches ->
            let
                branchToString ( branch, branchExpr ) =
                    String.concat
                        [ branch
                        , " ->\n"
                        , indent (toString branchExpr)
                        ]
            in
            String.concat
                [ "case "
                , toString matchedExpr
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


suggestDirect : Dict String Type -> Type -> List Expr
suggestDirect knownValues targetType =
    let
        checkKnownValue name knownType names =
            case Type.unifier knownType targetType of
                Nothing ->
                    names

                Just _ ->
                    name :: names
    in
    Dict.foldl checkKnownValue [] knownValues
        |> List.map (\name -> Call name [])


suggestWithArgument : Dict String Type -> Type -> List Expr
suggestWithArgument knownValues targetType =
    let
        checkKnownValue name knownType exprs =
            case knownType of
                Lambda from to ->
                    Type.unifiable targetType to
                        |> State.andThen
                            (\isUnifiable ->
                                if isUnifiable then
                                    State.get
                                        |> State.map
                                            (\substitutions ->
                                                allSpecializationsOf knownValues
                                                    substitutions
                                                    from
                                            )

                                else
                                    State.state []
                            )
                        |> State.finalValue Type.noSubstitutions
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
                            Type.unifiable targetType to
                                |> State.andThen
                                    (\isUnifiable ->
                                        if isUnifiable then
                                            State.get
                                                |> State.map
                                                    (\substitutions ->
                                                        allSpecializationsOf knownValues
                                                            substitutions
                                                            fromA
                                                    )

                                        else
                                            State.state []
                                    )
                                |> State.andThen
                                    (\namesA_ ->
                                        State.get
                                            |> State.map
                                                (\substitutions ->
                                                    allSpecializationsOf knownValues
                                                        substitutions
                                                        fromB
                                                )
                                            |> State.map (Tuple.pair namesA_)
                                    )
                                |> State.finalValue Type.noSubstitutions
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


suggestCreateTuple : (Type -> List Expr) -> Dict String Type -> Type -> List Expr
suggestCreateTuple suggest knownValues targetType =
    case targetType of
        Tuple (typeA :: typeB :: []) ->
            List.concatMap
                (\exprA ->
                    List.map (CreateTuple exprA)
                        (suggest typeB)
                )
                (suggest typeA)

        _ ->
            []


suggestCreateCase :
    (Dict String Type -> Type -> List Expr)
    -> Dict String Type
    -> List Union
    -> Type
    -> List Expr
suggestCreateCase suggest values unions targetType =
    let
        suggestCase union =
            valuesOfUnion union
                |> List.map
                    (\( value, _ ) ->
                        List.map suggestBranch union.tags
                            |> Case (Call value [])
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
            , suggest (toNewValues subTypes) targetType
                |> List.head
                |> Maybe.withDefault (Call "Debug.todo" [ "\"implement\"" ])
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



---- HELPER


allSpecializationsOf : Dict String Type -> Substitutions -> Type -> List String
allSpecializationsOf knownValues substitutions targetType =
    let
        checkKnownValue name knownType names =
            Type.unifiable knownType targetType
                |> State.map
                    (\isUnifiable ->
                        if isUnifiable then
                            name :: names

                        else
                            names
                    )
                |> State.finalValue substitutions
    in
    Dict.foldl checkKnownValue [] knownValues
