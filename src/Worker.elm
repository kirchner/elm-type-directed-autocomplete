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
import Elm.Type exposing (Type)
import Generator
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
    , data : Encode.Value
    }


type alias PackageIdentifier =
    { name : String
    , version : String
    }


type alias ModuleData =
    { name : ModuleName
    , fileName : String
    , file : File
    , exposed : Module
    , internal : Module
    }


parse : InFile -> Result String ( ModuleData, Encode.Value )
parse file =
    Parser.parse file.content
        |> Result.map
            (\rawFile ->
                ( toModuleData file.package file.fileName rawFile
                , RawFile.encode rawFile
                )
            )
        |> Result.mapError Parser.deadEndsToString


parseCached : CachedFile -> Result String ModuleData
parseCached cached =
    Decode.decodeValue RawFile.decoder cached.data
        |> Result.map (toModuleData cached.package cached.fileName)
        |> Result.mapError Decode.errorToString


toModuleData : Maybe PackageIdentifier -> String -> RawFile -> ModuleData
toModuleData package fileName rawFile =
    let
        interface =
            Interface.build rawFile

        file =
            Processing.process Processing.init rawFile
    in
    { name = RawFile.moduleName rawFile
    , fileName = fileName
    , file = file
    , exposed = Module.exposed file interface
    , internal = Module.internal file
    }



-- Program


port toElm : (InFile -> msg) -> Sub msg


port restore : (CachedFile -> msg) -> Sub msg


port toJS : Encode.Value -> Cmd msg


port storeFile :
    { content : String
    , data : Encode.Value
    }
    -> Cmd msg


port completionsFor : (Value -> msg) -> Sub msg


type alias Params =
    { fileName : String
    , range : Range
    }


paramsDecoder : Decoder Params
paramsDecoder =
    Decode.succeed Params
        |> Decode.required "fileName" Decode.string
        |> Decode.required "range" Elm.Syntax.Range.decoder


port completions : List String -> Cmd msg


store : InFile -> Encode.Value -> Cmd msg
store file data =
    storeFile { content = file.content, data = data }


type alias Model =
    { modules : List ModuleData }


type Msg
    = Parse InFile
    | Restore CachedFile
    | For Value


init : flags -> ( Model, Cmd msg )
init _ =
    ( { modules = [] }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse file ->
            case parse file of
                Ok ( mod, data ) ->
                    ( { model | modules = mod :: model.modules }
                    , store file data
                    )

                Err e ->
                    ( model
                    , toJS <| Encode.string <| file.fileName ++ ": " ++ e
                    )

        Restore cached ->
            case parseCached cached of
                Ok mod ->
                    ( { model | modules = mod :: model.modules }
                    , Cmd.none
                    )

                Err e ->
                    ( model
                    , Cmd.batch
                        [ toJS <| Encode.string e
                        , toJS <| Encode.string cached.fileName
                        ]
                    )

        For rawParams ->
            case Decode.decodeValue paramsDecoder rawParams of
                Err e ->
                    ( model
                    , toJS <| Encode.string (Decode.errorToString e)
                    )

                Ok params ->
                    let
                        isFile { fileName } =
                            fileName == params.fileName
                    in
                    ( model
                    , case List.find isFile model.modules of
                        Nothing ->
                            toJS (Encode.string "no file found")

                        Just { file, internal } ->
                            case Module.functionDeclarationAt params.range file of
                                Nothing ->
                                    toJS (Encode.string "no function declaration found")

                                Just function ->
                                    let
                                        values =
                                            internal.values

                                        aliases =
                                            internal.aliases

                                        unions =
                                            internal.unions
                                    in
                                    function
                                        |> infer values aliases params.range
                                        |> Maybe.map (generate values aliases unions)
                                        |> Maybe.map completions
                                        |> Maybe.withDefault
                                            (toJS (Encode.string "no completions found"))
                    )


infer :
    Dict String Type
    -> List Alias
    -> Range
    -> Function
    -> Maybe ( Type, Dict String Type )
infer values aliases range function =
    Inference.inferHole
        { function = function
        , range = range
        , values = values
        , aliases = aliases
        }


generate :
    Dict String Type
    -> List Alias
    -> List Union
    -> ( Type, Dict String Type )
    -> List String
generate values aliases unions ( tipe, localValues ) =
    Generator.default
        |> Generator.addValues (Dict.map (\_ -> Type.normalize aliases) values)
        |> Generator.addValues (Dict.map (\_ -> Type.normalize aliases) localValues)
        |> Generator.addUnions unions
        |> Generator.for (Type.normalize aliases tipe)
        |> List.map Generator.exprToText


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
