port module Worker exposing (main)

import Dict exposing (Dict)
import Elm.Docs exposing (Alias, Union)
import Elm.Interface as Interface exposing (Interface)
import Elm.Parser as Parser
import Elm.Processing as Processing
import Elm.RawFile as RawFile exposing (RawFile)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Type exposing (Type(..))
import Generator exposing (Expr)
import Inference
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Module exposing (Module)
import Parser
import Set exposing (Set)
import Type


type alias InFile =
    { package : Maybe PackageIdentifier
    , fileName : String
    , content : String
    }


type alias CachedFile =
    { package : Maybe PackageIdentifier
    , fileName : String
    , data : Value
    }


type alias PackageIdentifier =
    { name : String
    , version : String
    }


type alias ModuleData =
    { name : ModuleName
    , file : File
    , exposed : Module
    }


parse : InFile -> Result String ( ModuleData, Value )
parse file =
    Parser.parse file.content
        |> Result.map
            (\rawFile ->
                ( toModuleData file.package rawFile
                , RawFile.encode rawFile
                )
            )
        |> Result.mapError Parser.deadEndsToString


parseCached : CachedFile -> Result String ModuleData
parseCached cached =
    Decode.decodeValue RawFile.decoder cached.data
        |> Result.map (toModuleData cached.package)
        |> Result.mapError Decode.errorToString


toModuleData : Maybe PackageIdentifier -> RawFile -> ModuleData
toModuleData package rawFile =
    let
        interface =
            Interface.build rawFile

        file =
            Processing.process Processing.init rawFile
    in
    { name = RawFile.moduleName rawFile
    , file = file
    , exposed = Module.exposed file interface
    }



-- Program


port toElm : (InFile -> msg) -> Sub msg


port restore : (CachedFile -> msg) -> Sub msg


port toJS : Value -> Cmd msg


port storeFile :
    { content : String
    , data : Value
    }
    -> Cmd msg


port completionsFor : (CompletionRequest -> msg) -> Sub msg


type alias CompletionRequest =
    { fileName : String
    , src : String
    , column : Int
    , row : Int
    }


port completions : List String -> Cmd msg


store : InFile -> Value -> Cmd msg
store file data =
    storeFile { content = file.content, data = data }


type alias Model =
    { modules : Dict String ModuleData }


type Msg
    = Parse InFile
    | Restore CachedFile
    | For CompletionRequest


init : flags -> ( Model, Cmd msg )
init _ =
    ( { modules = Dict.empty }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse file ->
            case parse file of
                Ok ( mod, data ) ->
                    ( { model | modules = Dict.insert file.fileName mod model.modules }
                    , Cmd.batch
                        [ store file data
                        , toJS (Encode.string ("reparsed " ++ file.fileName))
                        ]
                    )

                Err e ->
                    ( model
                    , toJS <| Encode.string <| file.fileName ++ ": " ++ e
                    )

        Restore cached ->
            case parseCached cached of
                Ok mod ->
                    ( { model | modules = Dict.insert cached.fileName mod model.modules }
                    , toJS (Encode.string ("restored " ++ cached.fileName))
                    )

                Err e ->
                    ( model
                    , Cmd.batch
                        [ toJS <| Encode.string e
                        , toJS <| Encode.string cached.fileName
                        ]
                    )

        For completionRequest ->
            case Parser.parse completionRequest.src of
                Err e ->
                    ( model
                    , Cmd.batch
                        [ toJS (Encode.string "could not parse file")
                        , completions []
                        ]
                    )

                Ok rawFile ->
                    let
                        file =
                            Processing.process Processing.init rawFile

                        range =
                            { start =
                                { column = completionRequest.column - wordLength
                                , row = completionRequest.row
                                }
                            , end =
                                { column = completionRequest.column
                                , row = completionRequest.row
                                }
                            }

                        wordLength =
                            completionRequest.src
                                |> String.lines
                                |> List.getAt (completionRequest.row - 1)
                                |> Maybe.andThen
                                    (String.left (completionRequest.column - 1)
                                        >> String.words
                                        >> List.last
                                    )
                                |> Maybe.map String.length
                                |> Maybe.withDefault 1
                    in
                    ( model
                    , case Module.functionDeclarationAt range file of
                        Nothing ->
                            Cmd.batch
                                [ toJS (Encode.string "no function declaration found")
                                , completions []
                                ]

                        Just function ->
                            let
                                internal =
                                    Module.internal file

                                values =
                                    internal.values

                                aliases =
                                    internal.aliases

                                unions =
                                    internal.unions
                            in
                            case
                                Inference.inferHole
                                    { function = function
                                    , range = range
                                    , values = values
                                    , aliases = aliases
                                    }
                                    |> Result.mapError
                                        (inferenceErrorToString
                                            values
                                            aliases
                                            unions
                                            range
                                            function
                                        )
                                    |> Result.map
                                        (generateCompletions range values aliases unions)
                            of
                                Err error ->
                                    Cmd.batch
                                        [ toJS (Encode.string error)
                                        , completions []
                                        ]

                                Ok cmd ->
                                    cmd
                    )


inferenceErrorToString :
    Dict String Type
    -> List Alias
    -> List Union
    -> Range
    -> Function
    -> Inference.Error
    -> String
inferenceErrorToString values aliases unions range function error =
    let
        unionToString union =
            String.concat
                [ String.join " " (union.name :: union.args)
                , " = "
                , List.map tagToString union.tags
                    |> String.join " | "
                ]

        tagToString ( tagName, tagTypes ) =
            tagName
                :: List.map Type.toString tagTypes
                |> String.join " "

        valueToString ( fieldName, fieldType ) =
            String.concat
                [ fieldName
                , " : "
                , Type.toString fieldType
                ]
    in
    String.concat
        [ "Could not infer at"
        , " "
        , String.fromInt range.start.column
        , " "
        , String.fromInt range.start.row
        , " "
        , String.fromInt range.end.column
        , " "
        , String.fromInt range.end.row
        , "\n\nThe error was:\n  "
        , Inference.errorToString error
        , "."
        , "\n\nUnions:\n"
        , List.map unionToString unions
            |> List.map (\line -> "  " ++ line)
            |> String.join "\n"
        , "\n\nValues:\n"
        , List.map valueToString (Dict.toList values)
            |> List.map (\line -> "  " ++ line)
            |> String.join "\n"
        ]


generateCompletions :
    Range
    -> Dict String Type
    -> List Alias
    -> List Union
    -> ( Type, Dict String Type )
    -> Cmd Msg
generateCompletions range values aliases unions ( tipe, localValues ) =
    Generator.default
        |> Generator.addValues (Dict.map (\_ -> Type.normalize aliases) values)
        |> Generator.addValues (Dict.map (\_ -> Type.normalize aliases) localValues)
        |> Generator.addUnions unions
        |> Generator.for (Type.normalize aliases tipe)
        |> List.map (completionToString range)
        |> completions


completionToString : Range -> Expr -> String
completionToString range completion =
    case String.lines (Generator.exprToText completion) of
        [] ->
            ""

        first :: [] ->
            first

        first :: rest ->
            String.concat
                [ first
                , "\n"
                , String.join "\n" <|
                    List.map (\line -> String.repeat (range.start.column - 1) " " ++ line)
                        rest
                ]


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ toElm Parse
        , restore Restore
        , completionsFor For
        ]


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = always subscriptions
        }
