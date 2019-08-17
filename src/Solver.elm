module Solver exposing
    ( Comparability(..)
    , Constraint
    , Error(..)
    , Unifiability(..)
    , errorToString
    , run
    , unifiability
    )

import Canonical.Type exposing (Type(..))
import Dict exposing (Dict)
import Set


type alias Constraint =
    ( Type, Type )


type Error
    = InfiniteType String Type
    | NotNumberType Type
    | NotComparableType Type
    | UnificationFail Type Type
    | UnificationMismatch (List Type) (List Type)
    | RecordUnificationMismatch (List ( String, Type )) (List ( String, Type ))


errorToString : Error -> String
errorToString error =
    case error of
        InfiniteType name tipe ->
            String.concat
                [ "Infinite type: "
                , name
                , " : "
                , Canonical.Type.toString tipe
                ]

        NotNumberType tipe ->
            String.concat
                [ "Not a number type: "
                , Canonical.Type.toString tipe
                ]

        NotComparableType tipe ->
            String.concat
                [ "Not a comparable type: "
                , Canonical.Type.toString tipe
                ]

        UnificationFail typeA typeB ->
            String.concat
                [ "Cannot unify "
                , Canonical.Type.toString typeA
                , " and "
                , Canonical.Type.toString typeB
                ]

        UnificationMismatch typesA typesB ->
            String.concat
                [ "Cannot unify [ "
                , String.join ", " (List.map Canonical.Type.toString typesA)
                , " ] and [ "
                , String.join ", " (List.map Canonical.Type.toString typesB)
                , " ]"
                ]

        RecordUnificationMismatch fieldsA fieldsB ->
            let
                fieldToString ( fieldName, fieldType ) =
                    fieldName ++ " : " ++ Canonical.Type.toString fieldType
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
            case unifyOne typeA typeB of
                NotUnifiable error ->
                    Err error

                Unifiable _ newSubst ->
                    solver
                        ( compose newSubst subst
                        , List.map
                            (Tuple.mapFirst (Canonical.Type.apply newSubst)
                                >> Tuple.mapSecond (Canonical.Type.apply newSubst)
                            )
                            rest
                        )



---- UNIFICATION


type Unifiability
    = NotUnifiable Error
    | Unifiable Comparability (Dict String Type)


type Comparability
    = NotComparable
    | TypesAreEqual
    | TypeAIsMoreGeneral
    | TypeBIsMoreGeneral


unifiability : { typeA : Type, typeB : Type } -> Unifiability
unifiability { typeA, typeB } =
    unifyOne typeA typeB


unifyOne : Type -> Type -> Unifiability
unifyOne typeA typeB =
    if typeA == typeB then
        Unifiable TypesAreEqual Dict.empty

    else
        case ( typeA, typeB ) of
            ( Var varA, _ ) ->
                case bind varA typeB of
                    Err error ->
                        NotUnifiable error

                    Ok substitutions ->
                        Unifiable TypeAIsMoreGeneral substitutions

            ( _, Var varB ) ->
                case bind varB typeA of
                    Err error ->
                        NotUnifiable error

                    Ok substitutions ->
                        Unifiable TypeBIsMoreGeneral substitutions

            ( Type qualifierA nameA subTypesA, Type qualifierB nameB subTypesB ) ->
                if (qualifierA == qualifierB) && (nameA == nameB) then
                    unifyMany subTypesA subTypesB

                else
                    NotUnifiable (UnificationFail typeA typeB)

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
                NotUnifiable (UnificationFail typeA typeB)


unifyMany : List Type -> List Type -> Unifiability
unifyMany typeAs typeBs =
    case ( typeAs, typeBs ) of
        ( [], [] ) ->
            Unifiable TypesAreEqual Dict.empty

        ( typeA :: restA, typeB :: restB ) ->
            case unifyOne typeA typeB of
                NotUnifiable error ->
                    NotUnifiable error

                Unifiable comparability subst ->
                    case
                        unifyMany
                            (List.map (Canonical.Type.apply subst) restA)
                            (List.map (Canonical.Type.apply subst) restB)
                    of
                        NotUnifiable restError ->
                            NotUnifiable restError

                        Unifiable restComparability restSubst ->
                            case ( comparability, restComparability ) of
                                ( TypesAreEqual, TypesAreEqual ) ->
                                    Unifiable comparability (compose restSubst subst)

                                ( _, TypesAreEqual ) ->
                                    Unifiable comparability (compose restSubst subst)

                                ( TypesAreEqual, _ ) ->
                                    Unifiable restComparability (compose restSubst subst)

                                _ ->
                                    if comparability == restComparability then
                                        Unifiable comparability (compose restSubst subst)

                                    else
                                        Unifiable NotComparable (compose restSubst subst)

        _ ->
            NotUnifiable (UnificationMismatch typeAs typeBs)


unifyFields :
    List ( String, Type )
    -> Maybe String
    -> Maybe String
    -> List ( String, Type )
    -> List ( String, Type )
    -> Unifiability
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
                            NotUnifiable (RecordUnificationMismatch fieldsA fieldsB)

                        Just varB ->
                            if maybeVarB /= maybeVarA then
                                unifyFields
                                    (( nameA, typeA ) :: collectedFields)
                                    maybeVarA
                                    maybeVarB
                                    restA
                                    fieldsB

                            else
                                NotUnifiable (RecordUnificationMismatch fieldsA fieldsB)

                ( _, typeB ) :: [] ->
                    case unifyOne typeA typeB of
                        NotUnifiable error ->
                            NotUnifiable error

                        Unifiable comparability subst ->
                            case
                                unifyFields
                                    collectedFields
                                    maybeVarA
                                    maybeVarB
                                    restA
                                    otherFields
                            of
                                NotUnifiable restError ->
                                    NotUnifiable restError

                                Unifiable restComparability restSubst ->
                                    case ( comparability, restComparability ) of
                                        ( TypesAreEqual, TypesAreEqual ) ->
                                            Unifiable comparability
                                                (compose restSubst subst)

                                        ( _, TypesAreEqual ) ->
                                            Unifiable comparability
                                                (compose restSubst subst)

                                        ( TypesAreEqual, _ ) ->
                                            Unifiable restComparability
                                                (compose restSubst subst)

                                        _ ->
                                            if comparability == restComparability then
                                                Unifiable comparability
                                                    (compose restSubst subst)

                                            else
                                                Unifiable NotComparable
                                                    (compose restSubst subst)

                _ ->
                    NotUnifiable (RecordUnificationMismatch fieldsA fieldsB)

        [] ->
            case fieldsB of
                [] ->
                    if List.isEmpty collectedFields then
                        case maybeVarA of
                            Nothing ->
                                case maybeVarB of
                                    Nothing ->
                                        Unifiable TypesAreEqual Dict.empty

                                    Just varB ->
                                        Unifiable TypeBIsMoreGeneral
                                            (Dict.singleton varB (Record [] Nothing))

                            Just varA ->
                                case maybeVarB of
                                    Nothing ->
                                        Unifiable TypeAIsMoreGeneral
                                            (Dict.singleton varA (Record [] Nothing))

                                    Just varB ->
                                        Unifiable NotComparable
                                            (Dict.singleton varA (Record [] maybeVarB))

                    else
                        case maybeVarB of
                            Nothing ->
                                NotUnifiable (RecordUnificationMismatch fieldsA fieldsB)

                            Just varB ->
                                Unifiable TypeBIsMoreGeneral
                                    (Dict.singleton varB (Record collectedFields Nothing))

                _ ->
                    case maybeVarA of
                        Nothing ->
                            NotUnifiable (RecordUnificationMismatch fieldsA fieldsB)

                        Just varA ->
                            Unifiable TypeAIsMoreGeneral
                                (Dict.singleton varA (Record fieldsB maybeVarB))


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

    else if Set.member var (Canonical.Type.freeTypeVars tipe) then
        Err (InfiniteType var tipe)

    else if String.startsWith "number" var && not (isNumber tipe) then
        Err (NotNumberType tipe)

    else if String.startsWith "comparable" var && not (isComparable tipe) then
        Err (NotComparableType tipe)

    else
        Ok (Dict.singleton var tipe)


isNumber : Type -> Bool
isNumber tipe =
    case tipe of
        Var name ->
            String.startsWith "number" name

        Type "Basics" "Int" [] ->
            True

        Type "Basics" "Float" [] ->
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

            Type "Char" "Char" [] ->
                True

            Type "String" "String" [] ->
                True

            Type "List" "List" [ elementType ] ->
                isComparable elementType

            Tuple types ->
                List.all isComparable types

            _ ->
                False



---- HELPER


compose : Dict String Type -> Dict String Type -> Dict String Type
compose substA substB =
    Dict.union substA (Dict.map (\_ -> Canonical.Type.apply substA) substB)
