port module Cli exposing (main)

import Declaration exposing (Declaration(..))
import Dict exposing (Dict)
import Elm.Docs exposing (Module)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Type exposing (Type(..))
import Generator
import Inference
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Type


port requestSuggestions : (Value -> msg) -> Sub msg


port receiveSuggestions : Value -> Cmd msg


main : Program {} Model Msg
main =
    Platform.worker
        { init = \_ -> ( {}, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    {}


type Msg
    = SuggestionsRequested Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SuggestionsRequested rawParams ->
            case Decode.decodeValue paramsDecoder rawParams of
                Err decodeError ->
                    ( model, Cmd.none )

                Ok params ->
                    ( model
                    , suggestions params
                        |> Encode.list Encode.string
                        |> receiveSuggestions
                    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    requestSuggestions SuggestionsRequested


type alias Params =
    { rawUnions : List String
    , rawValues : List (List RawValue)
    , src : String
    , holeRange : Range
    }


type alias RawValue =
    { name : String
    , rawType : String
    }


rawValueDecoder : Decoder RawValue
rawValueDecoder =
    Decode.succeed RawValue
        |> Decode.required "name" Decode.string
        |> Decode.required "rawType" Decode.string


paramsDecoder : Decoder Params
paramsDecoder =
    Decode.succeed Params
        |> Decode.required "rawUnions" (Decode.list Decode.string)
        |> Decode.required "rawValues" (Decode.list (Decode.list rawValueDecoder))
        |> Decode.required "src" Decode.string
        |> Decode.required "holeRange" Range.decoder


suggestions :
    { rawUnions : List String
    , rawValues : List (List RawValue)
    , src : String
    , holeRange : Range
    }
    -> List String
suggestions { rawUnions, rawValues, src, holeRange } =
    let
        localAliases =
            declarations
                |> List.filterMap
                    (\declaration ->
                        case declaration of
                            TypeAlias name args tipe_ ->
                                Just
                                    { name = name
                                    , comment = ""
                                    , args = args
                                    , tipe = tipe_
                                    }

                            _ ->
                                Nothing
                    )

        declarations =
            rawUnions
                |> unions

        localUnions =
            declarations
                |> List.filterMap
                    (\declaration ->
                        case declaration of
                            CustomType name args tags ->
                                Just
                                    { name = name
                                    , comment = ""
                                    , args = args
                                    , tags = tags
                                    }

                            _ ->
                                Nothing
                    )

        localEnv =
            declarations
                |> List.filterMap
                    (\declaration ->
                        case declaration of
                            CustomType name args tags ->
                                tags
                                    |> List.map
                                        (\( constructor, types ) ->
                                            ( constructor
                                            , List.foldl Lambda
                                                (Type name [])
                                                types
                                            )
                                        )
                                    |> Just

                            _ ->
                                Nothing
                    )
    in
    case
        Inference.inferHole
            { src = src
            , holeRange = holeRange
            , values =
                rawValues
                    |> List.map (values >> Dict.map (\_ -> Type.normalize localAliases))
                    |> List.append (List.map Dict.fromList localEnv)
                    |> List.foldl Dict.union Dict.empty
            }
    of
        Nothing ->
            []

        Just ( tipe, localValues ) ->
            List.foldr Generator.addValues
                Generator.default
                (rawValues
                    |> List.map values
                    |> List.map (Dict.map (\_ -> Type.normalize localAliases))
                )
                |> Generator.addValues
                    (localValues
                        |> Dict.map (\_ -> Type.normalize localAliases)
                    )
                |> Generator.addUnions localUnions
                |> Generator.for (Type.normalize localAliases tipe)
                |> List.map Generator.exprToText


extractArgs : List String -> Type -> ( Dict String Type, Type )
extractArgs args tipe =
    extractArgsHelp Dict.empty args tipe


extractArgsHelp : Dict String Type -> List String -> Type -> ( Dict String Type, Type )
extractArgsHelp extractedValues args tipe =
    case ( args, tipe ) of
        ( arg :: restArgs, Lambda from to ) ->
            extractArgsHelp
                (Dict.insert arg from extractedValues)
                restArgs
                to

        _ ->
            ( extractedValues, tipe )


decodeTargetType : String -> Maybe Type
decodeTargetType targetType =
    if targetType == "" then
        Nothing

    else
        Decode.decodeString Elm.Type.decoder ("\"" ++ targetType ++ "\"")
            |> Result.toMaybe


unions : List String -> List Declaration
unions =
    List.filterMap union


union : String -> Maybe Declaration
union rawUnion =
    Declaration.parse rawUnion


values : List RawValue -> Dict String Type
values =
    List.filterMap value
        >> Dict.fromList
        >> Dict.filter
            (\name _ ->
                not
                    (List.any (\excluded -> String.contains excluded name)
                        [ "identity"
                        , "always"
                        , "todo"
                        , "toString"
                        , "log"
                        ]
                    )
            )


value : RawValue -> Maybe ( String, Type )
value rawValue =
    decodeTargetType rawValue.rawType
        |> Maybe.map (Tuple.pair rawValue.name)
