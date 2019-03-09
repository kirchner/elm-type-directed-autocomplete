module Type exposing
    ( Substitutions
    , isGeneralizationOf
    , substitute
    , toString
    , typeVariables
    , unifier
    )

import Dict exposing (Dict)
import Elm.Type exposing (Type(..))
import Set exposing (Set)
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

        Record values maybeVar ->
            let
                valueToString ( name, subType ) =
                    name ++ " : " ++ toString subType
            in
            String.concat
                [ "{ "
                , case maybeVar of
                    Nothing ->
                        ""

                    Just var ->
                        var ++ " | "
                , values
                    |> List.map valueToString
                    |> String.join ", "
                , " }"
                ]


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
                                    ((varB /= "number")
                                        && (varB /= "comparable")
                                    )
                                        || ((varB == "number")
                                                && ((typeA == Type "Int" [])
                                                        || (typeA == Type "Float" [])
                                                   )
                                           )
                                        || ((varB == "comparable")
                                                && ((typeA == Type "Int" [])
                                                        || (typeA == Type "Float" [])
                                                        || (typeA == Type "String" [])
                                                        || (typeA == Type "Bool" [])
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



---- UNIFICATION


type alias Substitutions =
    { bindTypeVariables : Dict String Type
    , bindRecordVariables : Dict String ( List ( String, Type ), Maybe String )
    }


noSubstitutions : Substitutions
noSubstitutions =
    { bindTypeVariables = Dict.empty
    , bindRecordVariables = Dict.empty
    }


mergeSubstitutions : Substitutions -> Substitutions -> Substitutions
mergeSubstitutions substitutionsA substitutionsB =
    { bindTypeVariables =
        Dict.union
            substitutionsA.bindTypeVariables
            substitutionsB.bindTypeVariables
    , bindRecordVariables =
        Dict.union
            substitutionsA.bindRecordVariables
            substitutionsB.bindRecordVariables
    }


{-| Substitute all type variables.
-}
substitute : Substitutions -> Type -> Type
substitute substitutions tipe =
    case tipe of
        Var name ->
            Dict.get name substitutions.bindTypeVariables
                |> Maybe.withDefault tipe

        Lambda typeA typeB ->
            Lambda
                (substitute substitutions typeA)
                (substitute substitutions typeB)

        Tuple types ->
            Tuple (List.map (substitute substitutions) types)

        Type name vars ->
            Type name (List.map (substitute substitutions) vars)

        Record fields maybeVar ->
            let
                ( actualFields, actualMaybeVar ) =
                    case maybeVar of
                        Nothing ->
                            ( fields, maybeVar )

                        Just name ->
                            Dict.get name substitutions.bindRecordVariables
                                |> Maybe.map subsituteRecordVariable
                                |> Maybe.withDefault ( fields, maybeVar )

                subsituteRecordVariable ( newFields, maybeNewVar ) =
                    ( -- TODO: Do we have to check for duplicate fields?
                      fields ++ newFields
                    , case maybeNewVar of
                        Nothing ->
                            maybeVar

                        _ ->
                            maybeNewVar
                    )
            in
            Record
                (List.map (Tuple.mapSecond (substitute substitutions)) actualFields)
                actualMaybeVar


{-| Returns all type variables.
-}
typeVariables : Type -> Set String
typeVariables tipe =
    case tipe of
        Var name ->
            Set.singleton name

        Lambda typeA typeB ->
            Set.union
                (typeVariables typeA)
                (typeVariables typeB)

        Tuple types ->
            types
                |> List.map typeVariables
                |> List.foldl Set.union Set.empty

        Type name vars ->
            vars
                |> List.map typeVariables
                |> List.foldl Set.union Set.empty

        Record fields maybeVar ->
            fields
                |> List.map (Tuple.second >> typeVariables)
                |> List.foldl Set.union Set.empty


{-| Return substitutions if both types can be unified.
-}
unifier : Type -> Type -> Maybe Substitutions
unifier typeA typeB =
    let
        bindTypeVariables bindings =
            { bindTypeVariables = bindings
            , bindRecordVariables = Dict.empty
            }
    in
    case ( typeA, typeB ) of
        ( Var nameA, _ ) ->
            bindTypeVariable nameA typeB
                |> Maybe.map bindTypeVariables

        ( _, Var nameB ) ->
            bindTypeVariable nameB typeA
                |> Maybe.map bindTypeVariables

        ( Lambda fromA toA, Lambda fromB toB ) ->
            unifierMany [ fromA, toA ] [ fromB, toB ]

        ( Tuple typesA, Tuple typesB ) ->
            unifierMany typesA typesB

        ( Type nameA varsA, Type nameB varsB ) ->
            if nameA == nameB then
                unifierMany varsA varsB

            else
                Nothing

        ( Record fieldsA maybeVarA, Record fieldsB maybeVarB ) ->
            case ( maybeVarA, maybeVarB ) of
                ( Nothing, Nothing ) ->
                    unifierFields fieldsA fieldsB

                ( Just nameA, Nothing ) ->
                    let
                        inFieldsA ( fieldNameB, _ ) =
                            List.any (hasName fieldNameB) fieldsA

                        hasName thisName ( otherName, _ ) =
                            thisName == otherName

                        bindRecordVariable substitutions =
                            { substitutions
                                | bindRecordVariables =
                                    Dict.insert nameA
                                        ( List.filter (not << inFieldsA) fieldsB, Nothing )
                                        substitutions.bindRecordVariables
                            }
                    in
                    unifierFields
                        fieldsA
                        (List.filter inFieldsA fieldsB)
                        |> Maybe.map bindRecordVariable

                ( Nothing, Just nameB ) ->
                    let
                        inFieldsB ( fieldNameA, _ ) =
                            List.any (hasName fieldNameA) fieldsB

                        hasName thisName ( otherName, _ ) =
                            thisName == otherName

                        bindRecordVariable substitutions =
                            { substitutions
                                | bindRecordVariables =
                                    Dict.insert nameB
                                        ( List.filter (not << inFieldsB) fieldsA, Nothing )
                                        substitutions.bindRecordVariables
                            }
                    in
                    unifierFields
                        (List.filter inFieldsB fieldsA)
                        fieldsB
                        |> Maybe.map bindRecordVariable

                ( Just nameA, Just nameB ) ->
                    if nameA == nameB then
                        unifierFields fieldsA fieldsB

                    else
                        let
                            bindRecordVariable substitutions =
                                { substitutions
                                    | bindRecordVariables =
                                        Dict.insert nameB
                                            ( [], Just nameA )
                                            substitutions.bindRecordVariables
                                }
                        in
                        unifierFields fieldsA fieldsB
                            |> Maybe.map bindRecordVariable

        _ ->
            Nothing


unifierMany : List Type -> List Type -> Maybe Substitutions
unifierMany typesA typesB =
    unifierManyHelp
        typesA
        typesB
        noSubstitutions


unifierManyHelp : List Type -> List Type -> Substitutions -> Maybe Substitutions
unifierManyHelp typesA typesB substitutions =
    case ( typesA, typesB ) of
        ( firstA :: restA, firstB :: restB ) ->
            unifier firstA firstB
                |> Maybe.map (mergeSubstitutions substitutions)
                |> Maybe.andThen (unifierManyHelp restA restB)

        ( [], [] ) ->
            Just substitutions

        _ ->
            Nothing


unifierFields : List ( String, Type ) -> List ( String, Type ) -> Maybe Substitutions
unifierFields fieldsA fieldsB =
    unifierFieldsHelp
        (List.sortBy Tuple.first fieldsA)
        (List.sortBy Tuple.first fieldsB)
        noSubstitutions


unifierFieldsHelp :
    List ( String, Type )
    -> List ( String, Type )
    -> Substitutions
    -> Maybe Substitutions
unifierFieldsHelp fieldsA fieldsB substitutions =
    case ( fieldsA, fieldsB ) of
        ( ( nameA, typeA ) :: restA, ( nameB, typeB ) :: restB ) ->
            if nameA == nameB then
                unifier typeA typeB
                    |> Maybe.map (mergeSubstitutions substitutions)
                    |> Maybe.andThen (unifierFieldsHelp restA restB)

            else
                Nothing

        ( [], [] ) ->
            Just substitutions

        _ ->
            Nothing


bindTypeVariable : String -> Type -> Maybe (Dict String Type)
bindTypeVariable name tipe =
    let
        nameOccursInType =
            Set.member name (typeVariables tipe)
    in
    case tipe of
        Var otherName ->
            if otherName == name then
                Just Dict.empty

            else if nameOccursInType then
                Nothing

            else
                Just (Dict.singleton name tipe)

        _ ->
            if nameOccursInType then
                Nothing

            else
                Just (Dict.singleton name tipe)
