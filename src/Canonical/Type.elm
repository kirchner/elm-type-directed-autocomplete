module Canonical.Type exposing
    ( Type(..)
    , apply
    , bool
    , char
    , float
    , freeTypeVars
    , fromTypeAnnotation
    , int
    , list
    , maybe
    , result
    , set
    , shader
    , string
    , toString
    )

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as Src
import Set exposing (Set)


type Type
    = Lambda Type Type
    | Var String
    | Type ModuleName String (List Type)
    | Record (List ( String, Type )) (Maybe String)
    | Unit
    | Tuple (List Type)


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

        Unit ->
            Set.empty

        Tuple types ->
            types
                |> List.map freeTypeVars
                |> List.foldl Set.union Set.empty

        Type _ _ vars ->
            vars
                |> List.map freeTypeVars
                |> List.foldl Set.union Set.empty

        Record fields maybeVar ->
            case maybeVar of
                Nothing ->
                    fields
                        |> List.map (Tuple.second >> freeTypeVars)
                        |> List.foldl Set.union Set.empty

                Just var ->
                    Set.insert var
                        (fields
                            |> List.map (Tuple.second >> freeTypeVars)
                            |> List.foldl Set.union Set.empty
                        )


apply : Dict String Type -> Type -> Type
apply subst tipe =
    case tipe of
        Type qualifier name subTypes ->
            Type qualifier name (List.map (apply subst) subTypes)

        Var var ->
            Dict.get var subst
                |> Maybe.withDefault tipe

        Lambda from to ->
            Lambda (apply subst from) (apply subst to)

        Unit ->
            tipe

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
fromTypeAnnotation typeAnnotation =
    case Node.value typeAnnotation of
        Src.GenericType var ->
            Var var

        Src.Typed qualifiedName typeAnnotations ->
            let
                ( moduleName, name ) =
                    Node.value qualifiedName
            in
            Type moduleName name (List.map fromTypeAnnotation typeAnnotations)

        Src.Unit ->
            Unit

        Src.Tupled typeAnnotations ->
            Tuple (List.map fromTypeAnnotation typeAnnotations)

        Src.Record recordFields ->
            Record
                (List.map
                    (\recordField ->
                        let
                            ( name, annotation ) =
                                Node.value recordField
                        in
                        ( Node.value name, fromTypeAnnotation annotation )
                    )
                    recordFields
                )
                Nothing

        Src.GenericRecord var recordFields ->
            Record
                (List.map
                    (\recordField ->
                        let
                            ( name, annotation ) =
                                Node.value recordField
                        in
                        ( Node.value name, fromTypeAnnotation annotation )
                    )
                    (Node.value recordFields)
                )
                (Just (Node.value var))

        Src.FunctionTypeAnnotation from to ->
            Lambda (fromTypeAnnotation from) (fromTypeAnnotation to)


toString : Type -> String
toString tipe =
    toStringHelp False tipe


toStringHelp : Bool -> Type -> String
toStringHelp needsParantheses tipe =
    case tipe of
        Var var ->
            var

        Lambda from to ->
            if needsParantheses then
                String.concat
                    [ "("
                    , toStringHelp True from
                    , " -> "
                    , toStringHelp False to
                    , ")"
                    ]

            else
                String.concat
                    [ toStringHelp True from
                    , " -> "
                    , toStringHelp False to
                    ]

        Unit ->
            "()"

        Tuple tipes ->
            String.concat
                [ "( "
                , String.join ", " (List.map (toStringHelp False) tipes)
                , " )"
                ]

        Type moduleName name tipes ->
            let
                qualifiedName =
                    String.join "." (moduleName ++ [ name ])
            in
            if List.isEmpty tipes then
                qualifiedName

            else if needsParantheses then
                String.concat
                    [ "("
                    , String.join " "
                        (qualifiedName :: List.map (toStringHelp True) tipes)
                    , ")"
                    ]

            else
                String.join " "
                    (qualifiedName :: List.map (toStringHelp True) tipes)

        Record fields maybeVar ->
            let
                fieldToString ( fieldName, fieldType ) =
                    fieldName ++ " : " ++ toStringHelp False fieldType
            in
            String.concat
                [ case maybeVar of
                    Nothing ->
                        "{ "

                    Just var ->
                        "{ " ++ var ++ " | "
                , String.join ", " (List.map fieldToString fields)
                , " }"
                ]



---- STANDARD TYPES


bool : Type
bool =
    Type [ "Basics" ] "Bool" []


int : Type
int =
    Type [ "Basics" ] "Int" []


float : Type
float =
    Type [ "Basics" ] "Float" []


char : Type
char =
    Type [ "Char" ] "Char" []


string : Type
string =
    Type [ "String" ] "String" []


list : Type -> Type
list a =
    Type [ "List" ] "List" [ a ]


set : Type -> Type
set a =
    Type [ "Set" ] "Set" [ a ]


result : Type -> Type -> Type
result err ok =
    Type [ "Result" ] "Result" [ err, ok ]


maybe : Type -> Type
maybe a =
    Type [ "Maybe" ] "Maybe" [ a ]


shader : List Type -> Type
shader =
    Type [ "WebGL" ] "Shader"
