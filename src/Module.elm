module Module exposing
    ( Module
    , fromFile
    , functionDeclarationAt
    )

import Dict exposing (Dict)
import Elm.Docs exposing (Alias, Union)
import Elm.Interface exposing (Exposed(..), Interface)
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Infix exposing (Infix)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type
import Elm.Type exposing (Type(..))
import Type


{-| -}
type alias Module =
    { binops : Dict String Infix
    , values : Dict String Type
    , constructors : Dict String ( String, Type )
    , aliases : List Alias
    , unions : List Union
    , exposedValues : Dict String Type
    , exposedConstructors : Dict String ( String, Type )
    , exposedAliases : List Alias
    , exposedUnions : List Union
    }


emptyModule : Module
emptyModule =
    { binops = Dict.empty
    , values = Dict.empty
    , constructors = Dict.empty
    , aliases = []
    , unions = []
    , exposedValues = Dict.empty
    , exposedConstructors = Dict.empty
    , exposedAliases = []
    , exposedUnions = []
    }


fromFile : File -> Interface -> Module
fromFile file interface =
    collect emptyModule interface file.declarations


{-| -}
functionDeclarationAt : Range -> File -> Maybe Function
functionDeclarationAt holeRange file =
    functionDeclarationAtHelp holeRange file.declarations


functionDeclarationAtHelp : Range -> List (Node Declaration) -> Maybe Function
functionDeclarationAtHelp holeRange declarations =
    case declarations of
        [] ->
            Nothing

        (Node range (FunctionDeclaration function)) :: rest ->
            let
                containsHole =
                    ((range.start.row < holeRange.start.row)
                        || ((range.start.row == holeRange.start.row)
                                && (range.start.column <= holeRange.start.column)
                           )
                    )
                        && ((range.end.row > holeRange.end.row)
                                || ((range.end.row == holeRange.end.row)
                                        && (range.end.column >= holeRange.end.column)
                                   )
                           )
            in
            if containsHole then
                Just function

            else
                functionDeclarationAtHelp holeRange rest

        _ :: rest ->
            functionDeclarationAtHelp holeRange rest



---- COLLECT


collect : Module -> Interface -> List (Node Declaration) -> Module
collect exposings interface declarations =
    case declarations of
        [] ->
            exposings

        (Node _ next) :: rest ->
            case next of
                FunctionDeclaration function ->
                    let
                        (Node _ functionImplementation) =
                            function.declaration

                        (Node _ name) =
                            functionImplementation.name
                    in
                    case function.signature of
                        Nothing ->
                            -- TODO insert general type signature?
                            collect exposings interface rest

                        Just (Node _ signature) ->
                            let
                                tipe =
                                    Type.fromTypeAnnotation signature.typeAnnotation
                            in
                            if Elm.Interface.exposesFunction name interface then
                                collect
                                    { exposings
                                        | values = Dict.insert name tipe exposings.values
                                        , exposedValues =
                                            Dict.insert name tipe exposings.exposedValues
                                    }
                                    interface
                                    rest

                            else
                                collect
                                    { exposings
                                        | values = Dict.insert name tipe exposings.values
                                    }
                                    interface
                                    rest

                AliasDeclaration typeAlias ->
                    let
                        (Node _ name) =
                            typeAlias.name

                        toArg (Node _ generic) =
                            generic

                        newAlias =
                            { name = name
                            , comment = ""
                            , args = List.map toArg typeAlias.generics
                            , tipe = Type.fromTypeAnnotation typeAlias.typeAnnotation
                            }
                    in
                    if Elm.Interface.exposesAlias name interface then
                        collect
                            { exposings
                                | aliases = newAlias :: exposings.aliases
                                , exposedAliases = newAlias :: exposings.exposedAliases
                            }
                            interface
                            rest

                    else
                        collect
                            { exposings | aliases = newAlias :: exposings.aliases }
                            interface
                            rest

                CustomTypeDeclaration tipe ->
                    let
                        (Node _ name) =
                            tipe.name

                        args =
                            List.map toArg tipe.generics

                        toArg (Node _ generic) =
                            generic

                        newUnion =
                            { name = name
                            , comment = ""
                            , args = args
                            , tags = []
                            }

                        isExposed exposed =
                            case exposed of
                                CustomType ( exposedName, exposedConstructors ) ->
                                    if exposedName == name then
                                        Just exposedConstructors

                                    else
                                        Nothing

                                _ ->
                                    Nothing

                        toTag (Node _ valueConstructor) =
                            let
                                (Node _ constructorName) =
                                    valueConstructor.name
                            in
                            Just
                                ( constructorName
                                , List.map Type.fromTypeAnnotation
                                    valueConstructor.arguments
                                )

                        values =
                            List.map toValue tipe.constructors

                        toValue (Node _ valueConstructor) =
                            let
                                (Node _ constructorName) =
                                    valueConstructor.name
                            in
                            ( constructorName
                            , ( name
                              , List.foldr (Lambda << Type.fromTypeAnnotation)
                                    targetType
                                    valueConstructor.arguments
                              )
                            )

                        targetType =
                            Type name (List.map Var args)
                    in
                    case List.head (List.filterMap isExposed interface) of
                        Nothing ->
                            collect
                                { exposings
                                    | unions = newUnion :: exposings.unions
                                    , constructors =
                                        List.foldl
                                            (\( constructor, value ) ->
                                                Dict.insert constructor value
                                            )
                                            exposings.constructors
                                            values
                                }
                                interface
                                rest

                        Just [] ->
                            collect
                                { exposings
                                    | unions = newUnion :: exposings.unions
                                    , constructors =
                                        List.foldl
                                            (\( constructor, value ) ->
                                                Dict.insert constructor value
                                            )
                                            exposings.constructors
                                            values
                                    , exposedUnions = newUnion :: exposings.exposedUnions
                                }
                                interface
                                rest

                        Just _ ->
                            collect
                                { exposings
                                    | unions = newUnion :: exposings.unions
                                    , constructors =
                                        List.foldl
                                            (\( constructor, value ) ->
                                                Dict.insert constructor value
                                            )
                                            exposings.constructors
                                            values
                                    , exposedUnions = newUnion :: exposings.exposedUnions
                                    , exposedConstructors =
                                        List.foldl
                                            (\( constructor, value ) ->
                                                Dict.insert constructor value
                                            )
                                            exposings.exposedConstructors
                                            values
                                }
                                interface
                                rest

                PortDeclaration signature ->
                    collect exposings interface rest

                InfixDeclaration infix ->
                    let
                        (Node _ name) =
                            infix.operator
                    in
                    collect
                        { exposings
                            | binops = Dict.insert name infix exposings.binops
                        }
                        interface
                        rest

                Destructuring _ _ ->
                    collect exposings interface rest
