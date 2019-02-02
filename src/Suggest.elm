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
    = Call String (List String)


printExpr : Expr -> String
printExpr expr =
    case expr of
        Call name args ->
            String.concat
                [ String.join " " (name :: args) ]


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
    targetType
        |> findExprs (Dict.map (\_ knownValue -> removeScope knownValue) knownValues)
        |> State.finalValue Dict.empty


computeGeneralizations : Dict String Type -> Type -> List Expr
computeGeneralizations knownValues targetType =
    knownValues
        |> Dict.foldl
            (\name knownType exprs ->
                knownType
                    |> isGeneralizationOf targetType
                    |> State.andThen
                        (\isGeneralization ->
                            if isGeneralization then
                                State.state (Call name [] :: exprs)

                            else
                                case knownType of
                                    Lambda from to ->
                                        to
                                            |> isGeneralizationOf targetType
                                            |> State.andThen
                                                (\toIsGeneralization ->
                                                    if toIsGeneralization then
                                                        State.map (List.append exprs)
                                                            (computeGeneralizationsHelp knownValues name from)

                                                    else
                                                        State.state exprs
                                                )

                                    _ ->
                                        State.state exprs
                        )
                    |> State.finalValue Dict.empty
            )
            []


computeGeneralizationsHelp :
    Dict String Type
    -> String
    -> Type
    -> State (Dict String Type) (List Expr)
computeGeneralizationsHelp knownValues name targetType =
    knownValues
        |> Dict.foldl
            (\calledName knownType exprs ->
                targetType
                    |> isGeneralizationOf knownType
                    |> State.andThen
                        (\isGeneralization ->
                            if isGeneralization then
                                State.map ((::) (Call name [ calledName ]))
                                    exprs

                            else
                                exprs
                        )
            )
            (State.state [])



---- COMPARE


findExprs : Dict String Type -> Type -> State (Dict String Type) (List Expr)
findExprs knownValues targetType =
    let
        directExprs =
            targetType
                |> computeGeneralizations knownValues
                |> State.state

        toExpr ( name, knownType ) =
            Call name []
    in
    directExprs


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
