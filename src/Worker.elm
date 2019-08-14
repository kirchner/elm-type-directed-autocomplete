port module Worker exposing (main)

import Canonical exposing (ModuleData, Store)
import Canonical.Annotation exposing (Annotation)
import Canonical.Type exposing (Type)
import Dict exposing (Dict)
import Elm.Docs exposing (Alias, Union)
import Elm.Interface as Interface exposing (Interface)
import Elm.Parser as Parser
import Elm.Processing as Processing
import Elm.RawFile as RawFile exposing (RawFile)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (Infix)
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range, emptyRange)
import Generator exposing (Expr)
import Inference
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Module
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


parse : InFile -> Result String ( ModuleData, Value )
parse file =
    Parser.parse file.content
        |> Result.map
            (\rawFile ->
                ( toModuleData file.fileName rawFile
                , RawFile.encode rawFile
                )
            )
        |> Result.mapError Parser.deadEndsToString


parseCached : CachedFile -> Result String ModuleData
parseCached cached =
    Decode.decodeValue RawFile.decoder cached.data
        |> Result.map (toModuleData cached.fileName)
        |> Result.mapError Decode.errorToString


toModuleData : String -> RawFile -> ModuleData
toModuleData fileName rawFile =
    let
        interface =
            Interface.build rawFile

        file =
            Processing.process Processing.init rawFile
    in
    { name = String.join "." (RawFile.moduleName rawFile)
    , fileName = fileName
    , file = file
    , imports = List.map Node.value file.imports
    , interface = interface
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
    { store : Store }


type Msg
    = Parse InFile
    | Restore CachedFile
    | For CompletionRequest


init : flags -> ( Model, Cmd msg )
init _ =
    ( { store = Canonical.emptyStore }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse file ->
            case parse file of
                Ok ( moduleData, data ) ->
                    ( { model | store = Canonical.add moduleData model.store }
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
                Ok moduleData ->
                    ( { model | store = Canonical.add moduleData model.store }
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
                        currentModuleData =
                            toModuleData completionRequest.fileName rawFile

                        newStore =
                            Canonical.replace currentModuleData model.store

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
                    ( { model | store = newStore }
                    , case Module.functionDeclarationAt range currentModuleData.file of
                        Nothing ->
                            Cmd.batch
                                [ toJS (Encode.string "no function declaration found")
                                , completions []
                                ]

                        Just function ->
                            let
                                values =
                                    Dict.get currentModuleData.name model.store.done
                                        |> Maybe.map .values
                                        |> Maybe.withDefault Dict.empty
                                        |> Dict.map (\_ -> Canonical.Annotation.fromType)

                                imports =
                                    Dict.get currentModuleData.name model.store.done
                                        |> Maybe.map .imports
                                        |> Maybe.withDefault Dict.empty
                            in
                            case
                                Inference.inferHole
                                    { function = function
                                    , range = range
                                    , moduleName = currentModuleData.name
                                    , imports = imports
                                    , binops = Dict.empty
                                    , values = values
                                    }
                                    |> Result.mapError
                                        (inferenceErrorToString
                                            Dict.empty
                                            []
                                            []
                                            range
                                            function
                                        )
                                    |> Result.map (generateCompletions range [] [] [])
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
    Dict String Annotation
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

        valueToString ( fieldName, fieldAnnotation ) =
            String.concat
                [ fieldName
                , " : "
                , Canonical.Annotation.toString fieldAnnotation
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
    -> List (Dict String Annotation)
    -> List Alias
    -> List Union
    -> ( Type, Dict String Type )
    -> Cmd Msg
generateCompletions range globalValues aliases unions ( tipe, localValues ) =
    let
        addValues generator =
            --List.foldl
            --    (\values ->
            --        Generator.addValues (Dict.map (\_ -> Type.normalize aliases) values)
            --    )
            --    generator
            --    globalValues
            generator
    in
    --Generator.default
    --    |> addValues
    --    --|> Generator.addValues localValues
    --    |> Generator.addUnions unions
    --    |> Generator.for (Type.normalize aliases tipe)
    --    |> List.map (completionToString range)
    --    |> completions
    Cmd.none


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
