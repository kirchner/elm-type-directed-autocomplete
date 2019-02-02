module Suggest exposing
    ( Expr(..)
    , isGeneralizationOf
    , suggest
    )

import Dict exposing (Dict)
import Elm.Docs exposing (Module)
import Elm.Type exposing (Type(..))
import Set
import State exposing (State)


type Expr
    = Call String (List String)


suggest : Dict String Type -> Type -> List Expr
suggest knownValues targetType =
    let
        toExpr name =
            Call name []

        makeUnique =
            Set.fromList >> Set.toList
    in
    [ exactMatch knownValues targetType
    , generalizations knownValues targetType
    ]
        |> List.concat
        |> makeUnique
        |> List.map toExpr


exactMatch : Dict String Type -> Type -> List String
exactMatch knownValues targetType =
    knownValues
        |> Dict.filter
            (\name knownType ->
                removeScope knownType == removeScope targetType
            )
        |> Dict.keys


generalizations : Dict String Type -> Type -> List String
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
        |> Dict.keys



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
