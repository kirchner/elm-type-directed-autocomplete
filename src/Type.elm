module Type exposing
    ( isGeneralizationOf
    , toString
    )

import Dict exposing (Dict)
import Elm.Type exposing (Type(..))
import State exposing (State)


toString : Type -> String
toString type_ =
    case type_ of
        Var var ->
            var

        Type name subTypes ->
            if List.isEmpty subTypes then
                name

            else
                name ++ " " ++ String.join " " (List.map toString subTypes)

        Lambda from to ->
            toString from ++ " -> " ++ toString to

        Tuple subTypes ->
            if List.isEmpty subTypes then
                "()"

            else
                "( "
                    ++ String.join ", " (List.map toString subTypes)
                    ++ " )"

        Record _ _ ->
            "TODO"


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



---- HELPER


zipTraverse : List Type -> List Type -> State (Dict String Type) Bool
zipTraverse typeAs typeBs =
    if List.length typeAs /= List.length typeBs then
        State.state False

    else
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
