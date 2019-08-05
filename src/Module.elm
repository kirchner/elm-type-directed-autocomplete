module Module exposing
    ( Module
    , exposed
    , functionDeclarationAt
    , internal
    )

import Dict exposing (Dict)
import Elm.Docs exposing (Alias, Union)
import Elm.Interface exposing (Exposed(..), Interface)
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type
import Elm.Type exposing (Type(..))
import Type


{-| -}
type alias Module =
    { values : Dict String Type
    , aliases : List Alias
    , unions : List Union
    }


emptyModule : Module
emptyModule =
    { values = Dict.empty
    , aliases = []
    , unions = []
    }


{-| -}
exposed : File -> Interface -> Module
exposed file interface =
    collect emptyModule (Just interface) file.declarations


{-| -}
internal : File -> Module
internal file =
    collect emptyModule Nothing file.declarations


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


collect : Module -> Maybe Interface -> List (Node Declaration) -> Module
collect exposings maybeInterface declarations =
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
                    if relevantFunction name maybeInterface then
                        case function.signature of
                            Nothing ->
                                -- TODO insert general type signature?
                                collect exposings maybeInterface rest

                            Just (Node _ signature) ->
                                let
                                    tipe =
                                        Type.fromTypeAnnotation signature.typeAnnotation
                                in
                                collect
                                    { exposings
                                        | values = Dict.insert name tipe exposings.values
                                    }
                                    maybeInterface
                                    rest

                    else
                        collect exposings maybeInterface rest

                AliasDeclaration typeAlias ->
                    let
                        (Node _ name) =
                            typeAlias.name
                    in
                    if relevantAlias name maybeInterface then
                        let
                            toArg (Node _ generic) =
                                generic
                        in
                        collect
                            { exposings
                                | aliases =
                                    { name = name
                                    , comment = ""
                                    , args = List.map toArg typeAlias.generics
                                    , tipe = Type.fromTypeAnnotation typeAlias.typeAnnotation
                                    }
                                        :: exposings.aliases
                            }
                            maybeInterface
                            rest

                    else
                        collect exposings maybeInterface rest

                CustomTypeDeclaration tipe ->
                    let
                        (Node _ name) =
                            tipe.name

                        args =
                            List.map toArg tipe.generics

                        toArg (Node _ generic) =
                            generic
                    in
                    case relevantUnion name maybeInterface of
                        Unexposed ->
                            collect exposings maybeInterface rest

                        ConstructorsUnexposed ->
                            collect
                                { exposings
                                    | unions =
                                        { name = name
                                        , comment = ""
                                        , args = args
                                        , tags = []
                                        }
                                            :: exposings.unions
                                }
                                maybeInterface
                                rest

                        ConstructorsExposed ->
                            let
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
                                    , List.foldr (Lambda << Type.fromTypeAnnotation)
                                        targetType
                                        valueConstructor.arguments
                                    )

                                targetType =
                                    Type name (List.map Var args)
                            in
                            collect
                                { exposings
                                    | unions =
                                        { name = name
                                        , comment = ""
                                        , args = List.map toArg tipe.generics
                                        , tags = List.filterMap toTag tipe.constructors
                                        }
                                            :: exposings.unions
                                    , values =
                                        List.foldl
                                            (\( constructor, value ) ->
                                                Dict.insert constructor value
                                            )
                                            exposings.values
                                            values
                                }
                                maybeInterface
                                rest

                PortDeclaration signature ->
                    collect exposings maybeInterface rest

                InfixDeclaration infix ->
                    collect exposings maybeInterface rest

                Destructuring _ _ ->
                    collect exposings maybeInterface rest


relevantFunction : String -> Maybe Interface -> Bool
relevantFunction name maybeInterface =
    case maybeInterface of
        Nothing ->
            True

        Just interface ->
            Elm.Interface.exposesFunction name interface


type ExposedUnion
    = Unexposed
    | ConstructorsExposed
    | ConstructorsUnexposed


relevantUnion : String -> Maybe Interface -> ExposedUnion
relevantUnion name maybeInterface =
    case maybeInterface of
        Nothing ->
            ConstructorsExposed

        Just interface ->
            let
                isType exposed_ =
                    case exposed_ of
                        CustomType ( exposedName, exposedConstructors ) ->
                            if exposedName == name then
                                Just exposedConstructors

                            else
                                Nothing

                        _ ->
                            Nothing
            in
            case List.head (List.filterMap isType interface) of
                Nothing ->
                    Unexposed

                Just [] ->
                    ConstructorsUnexposed

                Just _ ->
                    ConstructorsExposed


relevantAlias : String -> Maybe Interface -> Bool
relevantAlias name maybeInterface =
    case maybeInterface of
        Nothing ->
            True

        Just interface ->
            Elm.Interface.exposesAlias name interface
