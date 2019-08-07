module Solver exposing (Constraint, Error(..), errorToString, run)

import Dict exposing (Dict)
import Elm.Type exposing (Type(..))
import Set
import Type


type alias Constraint =
    ( Type, Type )


type Error
    = InfiniteType String Type
    | UnificationFail Type Type
    | UnificationMismatch (List Type) (List Type)
    | RecordUnificationMismatch (List ( String, Type )) (List ( String, Type ))


errorToString : Error -> String
errorToString error =
    case error of
        InfiniteType name tipe ->
            "Infinite type: " ++ name ++ " : " ++ Type.toString tipe

        UnificationFail typeA typeB ->
            "Cannot unify " ++ Type.toString typeA ++ " and " ++ Type.toString typeB

        UnificationMismatch typesA typesB ->
            String.concat
                [ "Cannot unify [ "
                , String.join ", " (List.map Type.toString typesA)
                , " ] and [ "
                , String.join ", " (List.map Type.toString typesB)
                , " ]"
                ]

        RecordUnificationMismatch fieldsA fieldsB ->
            let
                fieldToString ( fieldName, fieldType ) =
                    fieldName ++ " : " ++ Type.toString fieldType
            in
            String.concat
                [ "Cannot unify record types { "
                , String.join ", " (List.map fieldToString fieldsA)
                , " } and { "
                , String.join ", " (List.map fieldToString fieldsB)
                , " }"
                ]


run : List Constraint -> Result Error (Dict String Type)
run constraints =
    solver
        ( Dict.empty, constraints )


solver : ( Dict String Type, List Constraint ) -> Result Error (Dict String Type)
solver ( subst, constraints ) =
    case constraints of
        [] ->
            Ok subst

        ( typeA, typeB ) :: rest ->
            unifyOne typeA typeB
                |> Result.andThen
                    (\newSubst ->
                        solver
                            ( compose newSubst subst
                            , List.map
                                (Tuple.mapFirst (Type.apply newSubst)
                                    >> Tuple.mapSecond (Type.apply newSubst)
                                )
                                rest
                            )
                    )



---- UNIFICATION


unifyOne : Type -> Type -> Result Error (Dict String Type)
unifyOne typeA typeB =
    if typeA == typeB then
        Ok Dict.empty

    else
        case ( typeA, typeB ) of
            ( Var varA, _ ) ->
                bind varA typeB

            ( _, Var varB ) ->
                bind varB typeA

            ( Type nameA subTypesA, Type nameB subTypesB ) ->
                if nameA == nameB then
                    unifyMany subTypesA subTypesB

                else
                    Err (UnificationFail typeA typeB)

            ( Lambda fromA toA, Lambda fromB toB ) ->
                unifyMany [ fromA, toA ] [ fromB, toB ]

            ( Tuple typeAs, Tuple typeBs ) ->
                unifyMany typeAs typeBs

            ( Record fieldsA maybeVarA, Record fieldsB maybeVarB ) ->
                unifyFields []
                    maybeVarA
                    maybeVarB
                    (List.sortBy Tuple.first fieldsA)
                    (List.sortBy Tuple.first fieldsB)

            _ ->
                Err (UnificationFail typeA typeB)


unifyMany : List Type -> List Type -> Result Error (Dict String Type)
unifyMany typeAs typeBs =
    case ( typeAs, typeBs ) of
        ( [], [] ) ->
            Ok Dict.empty

        ( typeA :: restA, typeB :: restB ) ->
            unifyOne typeA typeB
                |> Result.andThen
                    (\subst ->
                        unifyMany
                            (List.map (Type.apply subst) restA)
                            (List.map (Type.apply subst) restB)
                            |> Result.map
                                (\newSubst ->
                                    compose newSubst subst
                                )
                    )

        _ ->
            Err (UnificationMismatch typeAs typeBs)


unifyFields :
    List ( String, Type )
    -> Maybe String
    -> Maybe String
    -> List ( String, Type )
    -> List ( String, Type )
    -> Result Error (Dict String Type)
unifyFields collectedFields maybeVarA maybeVarB fieldsA fieldsB =
    case fieldsA of
        ( nameA, typeA ) :: restA ->
            let
                ( nameAFields, otherFields ) =
                    List.partition
                        (\( nameB, _ ) -> nameB == nameA)
                        fieldsB
            in
            case nameAFields of
                [] ->
                    case maybeVarB of
                        Nothing ->
                            Err (RecordUnificationMismatch fieldsA fieldsB)

                        Just varB ->
                            if maybeVarB /= maybeVarA then
                                unifyFields
                                    (( nameA, typeA ) :: collectedFields)
                                    maybeVarA
                                    maybeVarB
                                    restA
                                    fieldsB

                            else
                                Err (RecordUnificationMismatch fieldsA fieldsB)

                ( _, typeB ) :: [] ->
                    unifyOne typeA typeB
                        |> Result.andThen
                            (\subst ->
                                unifyFields
                                    collectedFields
                                    maybeVarA
                                    maybeVarB
                                    restA
                                    otherFields
                                    |> Result.map
                                        (\newSubst ->
                                            compose newSubst subst
                                        )
                            )

                _ ->
                    Err (RecordUnificationMismatch fieldsA fieldsB)

        [] ->
            case fieldsB of
                [] ->
                    if List.isEmpty collectedFields then
                        case maybeVarA of
                            Nothing ->
                                case maybeVarB of
                                    Nothing ->
                                        Ok Dict.empty

                                    Just varB ->
                                        Ok (Dict.singleton varB (Record [] Nothing))

                            Just varA ->
                                Ok (Dict.singleton varA (Record [] maybeVarB))

                    else
                        case maybeVarB of
                            Nothing ->
                                Err (RecordUnificationMismatch fieldsA fieldsB)

                            Just varB ->
                                Ok <|
                                    Dict.singleton varB
                                        (Record collectedFields Nothing)

                _ ->
                    case maybeVarA of
                        Nothing ->
                            Err (RecordUnificationMismatch fieldsA fieldsB)

                        Just varA ->
                            Ok (Dict.singleton varA (Record fieldsB maybeVarB))


bind : String -> Type -> Result Error (Dict String Type)
bind var tipe =
    let
        isVar =
            case tipe of
                Var otherVar ->
                    otherVar == var

                _ ->
                    False
    in
    if isVar then
        Ok Dict.empty

    else if Set.member var (Type.freeTypeVars tipe) then
        Err (InfiniteType var tipe)

    else
        Ok (Dict.singleton var tipe)



---- HELPER


compose : Dict String Type -> Dict String Type -> Dict String Type
compose substA substB =
    Dict.union (Dict.map (\_ -> Type.apply substA) substB) substA
