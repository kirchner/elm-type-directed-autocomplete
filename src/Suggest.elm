module Suggest exposing
    ( Expr
    , isGeneralizationOf
    , printExpr
    , printType
    , suggest
    )

import Dict exposing (Dict)
import Elm.Docs exposing (Module)
import Elm.Type exposing (Type(..))
import List.Extra as List
import Set
import State exposing (State)


type Expr
    = Call String (List String) Type


printExpr : Expr -> String
printExpr expr =
    case expr of
        Call name args type_ ->
            String.concat
                [ String.join " " (name :: args)
                , " : "
                , printType (removeScope type_)
                ]


printType : Type -> String
printType type_ =
    case type_ of
        Var var ->
            var

        Type name subTypes ->
            if List.isEmpty subTypes then
                name

            else
                name ++ " " ++ String.join " " (List.map printType subTypes)

        Lambda from to ->
            printType from ++ " -> " ++ printType to

        Tuple subTypes ->
            if List.isEmpty subTypes then
                "()"

            else
                "( "
                    ++ String.join ", " (List.map printType subTypes)
                    ++ " )"

        Record _ _ ->
            "TODO"


suggest : Dict String Type -> Type -> List Expr
suggest knownValues targetType =
    let
        toExpr ( name, knownType ) =
            Call name [] knownType
    in
    [ exactMatch knownValues targetType
    , generalizations knownValues targetType
    ]
        |> List.concat
        |> List.uniqueBy Tuple.first
        |> List.map toExpr


exactMatch : Dict String Type -> Type -> List ( String, Type )
exactMatch knownValues targetType =
    knownValues
        |> Dict.filter
            (\name knownType ->
                removeScope knownType == removeScope targetType
            )
        |> Dict.toList


generalizations : Dict String Type -> Type -> List ( String, Type )
generalizations knownValues targetType =
    knownValues
        |> Dict.filter
            (\name knownType ->
                let
                    isGeneralization =
                        removeScope knownType
                            |> isGeneralizationOf targetType
                            |> State.finalValue Dict.empty
                in
                isGeneralization
            )
        |> Dict.toList



---- COMPARE


{-| Check if the second `Type` is a generalization of the first `Type`
-}
isGeneralizationOf : Type -> Type -> State (Dict String Type) Bool
isGeneralizationOf typeA typeB =
    case ( typeA, typeB ) of
        ( Type nameA subTypesA, Type nameB subTypesB ) ->
            State.map2 (&&)
                (State.state (nameA == nameB))
                (zipTraverse subTypesA subTypesB)

        ( Lambda fromA toA, Lambda fromB toB ) ->
            State.map2 (&&)
                (fromB |> isGeneralizationOf fromA)
                (toB |> isGeneralizationOf toA)

        ( Tuple subTypesA, Tuple subTypesB ) ->
            zipTraverse subTypesA subTypesB

        ( Record valuesA varA, Record valuesB varB ) ->
            State.state False

        ( _, Var varB ) ->
            State.get
                |> State.andThen
                    (\vars ->
                        case Dict.get varB vars of
                            Nothing ->
                                if
                                    (varB /= "number")
                                        || ((varB == "number")
                                                && ((typeA == Type "Int" [])
                                                        || (typeA == Type "Float" [])
                                                   )
                                           )
                                then
                                    State.put (Dict.insert varB typeA vars)
                                        |> State.map (always True)

                                else
                                    State.state False

                            Just setTypeA ->
                                case setTypeA of
                                    Var varA ->
                                        State.state True

                                    _ ->
                                        setTypeA
                                            |> isGeneralizationOf typeA
                    )

        _ ->
            State.state False


zipTraverse : List Type -> List Type -> State (Dict String Type) Bool
zipTraverse typeAs typeBs =
    State.map (List.all identity) <|
        State.traverse
            (\( typeA, typeB ) ->
                typeB
                    |> isGeneralizationOf typeA
            )
            (zip typeAs typeBs)


zip : List a -> List b -> List ( a, b )
zip =
    zipHelp []


zipHelp : List ( a, b ) -> List a -> List b -> List ( a, b )
zipHelp listAB listA listB =
    case ( listA, listB ) of
        ( a :: restA, b :: restB ) ->
            zipHelp (( a, b ) :: listAB) restA restB

        _ ->
            List.reverse listAB



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
                subTypes

        Lambda typeA typeB ->
            Lambda (removeScope typeA) (removeScope typeB)

        Tuple types ->
            Tuple (List.map removeScope types)

        Record values var ->
            Record
                (List.map (Tuple.mapSecond removeScope) values)
                var
