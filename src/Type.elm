module Type exposing
    ( Substitutions
    , noSubstitutions
    , normalize
    , substitute
    , toString
    , typeVariables
    , unifiable
    , unifier
    )

import Dict exposing (Dict)
import Elm.Docs exposing (Alias)
import Elm.Type exposing (Type(..))
import List.Extra as List
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



---- NORMALIZATION


{-| Normalize a type by recursivly replacing all type aliases.
-}
normalize : List Alias -> Type -> Type
normalize aliases tipe =
    let
        getAlias name =
            List.find (.name >> (==) name) aliases
    in
    case tipe of
        Var _ ->
            tipe

        Type name subTypes ->
            case getAlias name of
                Nothing ->
                    Type name
                        (List.map (normalize aliases) subTypes)

                Just alias_ ->
                    let
                        bindings =
                            bindingsHelp Dict.empty subTypes alias_.args

                        bindingsHelp dict types args =
                            case ( types, args ) of
                                ( nextTipe :: restTypes, arg :: restArgs ) ->
                                    bindingsHelp
                                        (Dict.insert arg nextTipe dict)
                                        restTypes
                                        restArgs

                                _ ->
                                    dict
                    in
                    alias_.args
                        |> List.foldl
                            (\arg nextTipe ->
                                substitute
                                    { bindTypeVariables = bindings
                                    , bindRecordVariables = Dict.empty
                                    }
                                    nextTipe
                            )
                            alias_.tipe

        Lambda from to ->
            Lambda
                (normalize aliases from)
                (normalize aliases to)

        Tuple subTypes ->
            Tuple (List.map (normalize aliases) subTypes)

        Record values maybeVar ->
            Record
                (List.map (Tuple.mapSecond (normalize aliases)) values)
                maybeVar



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


{-| Return substitutions if both types can be unified.
-}
unifier : Type -> Type -> Maybe Substitutions
unifier typeA typeB =
    case State.run noSubstitutions (unifiable typeA typeB) of
        ( False, _ ) ->
            Nothing

        ( True, substitutions ) ->
            Just substitutions


unifiable : Type -> Type -> State Substitutions Bool
unifiable typeA typeB =
    case ( typeA, typeB ) of
        ( Var nameA, _ ) ->
            bindTypeVariable nameA typeB

        ( _, Var nameB ) ->
            bindTypeVariable nameB typeA

        ( Lambda fromA toA, Lambda fromB toB ) ->
            unifiableMany [ fromA, toA ] [ fromB, toB ]

        ( Tuple typesA, Tuple typesB ) ->
            unifiableMany typesA typesB

        ( Type nameA varsA, Type nameB varsB ) ->
            if nameA == nameB then
                unifiableMany varsA varsB

            else
                State.state False

        ( Record fieldsA maybeVarA, Record fieldsB maybeVarB ) ->
            case ( maybeVarA, maybeVarB ) of
                ( Nothing, Nothing ) ->
                    unifiableFields fieldsA fieldsB

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
                    State.modify bindRecordVariable
                        |> State.andThen
                            (\_ ->
                                unifiableFields
                                    fieldsA
                                    (List.filter inFieldsA fieldsB)
                            )

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
                    State.modify bindRecordVariable
                        |> State.andThen
                            (\_ ->
                                unifiableFields
                                    (List.filter inFieldsB fieldsA)
                                    fieldsB
                            )

                ( Just nameA, Just nameB ) ->
                    if nameA == nameB then
                        unifiableFields fieldsA fieldsB

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
                        State.modify bindRecordVariable
                            |> State.andThen
                                (\_ ->
                                    unifiableFields fieldsA fieldsB
                                )

        _ ->
            State.state False


unifiableMany : List Type -> List Type -> State Substitutions Bool
unifiableMany typesA typesB =
    unifiableManyHelp
        typesA
        typesB


unifiableManyHelp : List Type -> List Type -> State Substitutions Bool
unifiableManyHelp typesA typesB =
    case ( typesA, typesB ) of
        ( firstA :: restA, firstB :: restB ) ->
            State.map2 (&&)
                (unifiable firstA firstB)
                (unifiableManyHelp restA restB)

        ( [], [] ) ->
            State.state True

        _ ->
            State.state False


unifiableFields : List ( String, Type ) -> List ( String, Type ) -> State Substitutions Bool
unifiableFields fieldsA fieldsB =
    unifiableFieldsHelp
        (List.sortBy Tuple.first fieldsA)
        (List.sortBy Tuple.first fieldsB)


unifiableFieldsHelp :
    List ( String, Type )
    -> List ( String, Type )
    -> State Substitutions Bool
unifiableFieldsHelp fieldsA fieldsB =
    case ( fieldsA, fieldsB ) of
        ( ( nameA, typeA ) :: restA, ( nameB, typeB ) :: restB ) ->
            if nameA == nameB then
                State.map2 (&&)
                    (unifiable typeA typeB)
                    (unifiableFieldsHelp restA restB)

            else
                State.state False

        ( [], [] ) ->
            State.state True

        _ ->
            State.state False


bindTypeVariable : String -> Type -> State Substitutions Bool
bindTypeVariable name tipe =
    let
        nameOccursInType =
            Set.member name (typeVariables tipe)

        bindIfNew substitutions =
            case Dict.get name substitutions.bindTypeVariables of
                Nothing ->
                    ( True
                    , { substitutions
                        | bindTypeVariables =
                            Dict.insert name
                                tipe
                                substitutions.bindTypeVariables
                      }
                    )

                Just boundType ->
                    if boundType == tipe then
                        ( True, substitutions )

                    else
                        ( False, substitutions )
    in
    case tipe of
        Var otherName ->
            if otherName == name then
                State.state True

            else if nameOccursInType then
                State.state False

            else
                State.advance bindIfNew

        _ ->
            if
                nameOccursInType
                    || (String.startsWith "number" name && not (isNumber tipe))
                    || (String.startsWith "comparable" name && not (isComparable tipe))
            then
                State.state False

            else
                State.advance bindIfNew


isNumber : Type -> Bool
isNumber tipe =
    case tipe of
        Var name ->
            String.startsWith "number" name

        Type "Int" [] ->
            True

        Type "Float" [] ->
            True

        _ ->
            False


isComparable : Type -> Bool
isComparable tipe =
    if isNumber tipe then
        True

    else
        case tipe of
            Var name ->
                String.startsWith "comparable" name

            Type "Char" [] ->
                True

            Type "String" [] ->
                True

            Type "List" [ elementType ] ->
                isComparable elementType

            Tuple types ->
                List.all isComparable types

            _ ->
                False


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
