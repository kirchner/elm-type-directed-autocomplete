module Type exposing
    ( Comparability(..)
    , Substitutions
    , Unifiability(..)
    , apply
    , freeTypeVars
    , fromTypeAnnotation
    , noSubstitutions
    , normalize
    , substitute
    , unifiability
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
import Elm.Docs exposing (Alias)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as Src
import Elm.Type exposing (Type(..))
import List.Extra as List
import Set exposing (Set)
import Src
import State exposing (State)



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
                        |> normalize aliases

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


type Unifiability
    = NotUnifiable
    | Unifiable Comparability Substitutions


type Comparability
    = NotComparable
    | TypesAreEqual
    | TypeAIsMoreGeneral
    | TypeBIsMoreGeneral


unifiability : { typeA : Type, typeB : Type } -> Unifiability
unifiability { typeA, typeB } =
    let
        ( internalUnifiability, substitutions ) =
            State.run noSubstitutions <|
                unifiabilityHelp typeA typeB
    in
    case internalUnifiability of
        INotUnifiable ->
            NotUnifiable

        IUnifiable comparability ->
            Unifiable comparability substitutions


type IUnifiability
    = INotUnifiable
    | IUnifiable Comparability


unifiabilityHelp : Type -> Type -> State Substitutions IUnifiability
unifiabilityHelp typeA typeB =
    case ( typeA, typeB ) of
        ( Var nameA, _ ) ->
            bindTypeVariable nameA typeB
                |> State.map
                    (\isUnifiable ->
                        if isUnifiable then
                            IUnifiable TypeAIsMoreGeneral

                        else
                            INotUnifiable
                    )

        ( _, Var nameB ) ->
            bindTypeVariable nameB typeA
                |> State.map
                    (\isUnifiable ->
                        if isUnifiable then
                            IUnifiable TypeBIsMoreGeneral

                        else
                            INotUnifiable
                    )

        ( Lambda fromA toA, Lambda fromB toB ) ->
            unifiabilityMany [ fromA, toA ] [ fromB, toB ]

        ( Tuple typesA, Tuple typesB ) ->
            unifiabilityMany typesA typesB

        ( Type nameA varsA, Type nameB varsB ) ->
            if nameA == nameB then
                unifiabilityMany varsA varsB

            else
                State.state INotUnifiable

        ( Record fieldsA maybeVarA, Record fieldsB maybeVarB ) ->
            case ( maybeVarA, maybeVarB ) of
                ( Nothing, Nothing ) ->
                    unifiabilityFields fieldsA fieldsB

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
                                unifiabilityFields
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
                                unifiabilityFields
                                    (List.filter inFieldsB fieldsA)
                                    fieldsB
                            )

                ( Just nameA, Just nameB ) ->
                    if nameA == nameB then
                        unifiabilityFields fieldsA fieldsB

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
                                    unifiabilityFields fieldsA fieldsB
                                )

        _ ->
            State.state INotUnifiable


unifiabilityMany : List Type -> List Type -> State Substitutions IUnifiability
unifiabilityMany typesA typesB =
    case ( typesA, typesB ) of
        ( firstA :: restA, firstB :: restB ) ->
            State.map2 mergeUnifiability
                (unifiabilityHelp firstA firstB)
                (unifiabilityMany restA restB)

        ( [], [] ) ->
            State.state
                (IUnifiable TypesAreEqual)

        _ ->
            State.state INotUnifiable


mergeUnifiability firstUnifiability secondUnifiability =
    case ( firstUnifiability, secondUnifiability ) of
        ( INotUnifiable, _ ) ->
            INotUnifiable

        ( _, INotUnifiable ) ->
            INotUnifiable

        ( IUnifiable TypesAreEqual, IUnifiable TypesAreEqual ) ->
            firstUnifiability

        ( IUnifiable _, IUnifiable TypesAreEqual ) ->
            firstUnifiability

        ( IUnifiable TypesAreEqual, IUnifiable _ ) ->
            secondUnifiability

        ( IUnifiable firstComparability, IUnifiable secondComparability ) ->
            if firstComparability == secondComparability then
                firstUnifiability

            else
                IUnifiable NotComparable


unifiabilityFields :
    List ( String, Type )
    -> List ( String, Type )
    -> State Substitutions IUnifiability
unifiabilityFields fieldsA fieldsB =
    unifiabilityFieldsHelp
        (List.sortBy Tuple.first fieldsA)
        (List.sortBy Tuple.first fieldsB)


unifiabilityFieldsHelp :
    List ( String, Type )
    -> List ( String, Type )
    -> State Substitutions IUnifiability
unifiabilityFieldsHelp fieldsA fieldsB =
    case ( fieldsA, fieldsB ) of
        ( ( nameA, typeA ) :: restA, ( nameB, typeB ) :: restB ) ->
            if nameA == nameB then
                State.map2 mergeUnifiability
                    (unifiabilityHelp typeA typeB)
                    (unifiabilityFieldsHelp restA restB)

            else
                State.state INotUnifiable

        ( [], [] ) ->
            State.state
                (IUnifiable TypesAreEqual)

        _ ->
            State.state INotUnifiable


bindTypeVariable : String -> Type -> State Substitutions Bool
bindTypeVariable name tipe =
    let
        nameOccursInType =
            Set.member name (freeTypeVars tipe)

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


{-| Returns all free type variables.
-}
freeTypeVars : Type -> Set String
freeTypeVars tipe =
    case tipe of
        Var name ->
            Set.singleton name

        Lambda typeA typeB ->
            Set.union
                (freeTypeVars typeA)
                (freeTypeVars typeB)

        Tuple types ->
            types
                |> List.map freeTypeVars
                |> List.foldl Set.union Set.empty

        Type name vars ->
            vars
                |> List.map freeTypeVars
                |> List.foldl Set.union Set.empty

        Record fields maybeVar ->
            fields
                |> List.map (Tuple.second >> freeTypeVars)
                |> List.foldl Set.union Set.empty


apply : Dict String Type -> Type -> Type
apply subst tipe =
    case tipe of
        Type _ _ ->
            tipe

        Var var ->
            Dict.get var subst
                |> Maybe.withDefault tipe

        Lambda from to ->
            Lambda (apply subst from) (apply subst to)

        Tuple types ->
            Tuple (List.map (apply subst) types)

        Record fields maybeVar ->
            case maybeVar of
                Nothing ->
                    Record
                        (List.map (Tuple.mapSecond (apply subst)) fields)
                        maybeVar

                Just var ->
                    case Dict.get var subst of
                        Just (Record otherFields maybeOtherVar) ->
                            Record
                                (List.map (Tuple.mapSecond (apply subst)) fields
                                    ++ List.map (Tuple.mapSecond (apply subst)) otherFields
                                )
                                maybeOtherVar

                        _ ->
                            Record
                                (List.map (Tuple.mapSecond (apply subst)) fields)
                                maybeVar


fromTypeAnnotation : Node Src.TypeAnnotation -> Type
fromTypeAnnotation (Node _ typeAnnotation) =
    case typeAnnotation of
        Src.GenericType var ->
            Var var

        Src.Typed (Node _ ( moduleName, name )) typeAnnotations ->
            Type
                (Src.qualifiedName moduleName name)
                (List.map fromTypeAnnotation typeAnnotations)

        Src.Unit ->
            Tuple []

        Src.Tupled typeAnnotations ->
            Tuple (List.map fromTypeAnnotation typeAnnotations)

        Src.Record recordFields ->
            Record
                (List.map
                    (\(Node _ ( Node _ name, annotation )) ->
                        ( name, fromTypeAnnotation annotation )
                    )
                    recordFields
                )
                Nothing

        Src.GenericRecord (Node _ var) (Node _ recordFields) ->
            Record
                (List.map
                    (\(Node _ ( Node _ name, annotation )) ->
                        ( name, fromTypeAnnotation annotation )
                    )
                    recordFields
                )
                (Just var)

        Src.FunctionTypeAnnotation from to ->
            Lambda (fromTypeAnnotation from) (fromTypeAnnotation to)
