port module Worker exposing (main)

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
    , module_ : Module
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
    , module_ = Module.fromFile file interface
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
                                thisModule =
                                    Module.fromFile file (Interface.build rawFile)

                                values =
                                    [ Dict.union
                                        thisModule.values
                                        (Dict.map (\_ -> Tuple.second)
                                            thisModule.constructors
                                        )
                                    , importedValues
                                    ]

                                importedValues =
                                    (RawFile.imports rawFile ++ defaultImports)
                                        |> List.concatMap
                                            (importedValuesFromImport model.modules)
                                        |> List.filter (not << unnecessary)
                                        |> Dict.fromList

                                unnecessary ( name, _ ) =
                                    (name == "Debug.todo")
                                        || (name == "always")
                                        || (name == "identity")

                                binops =
                                    List.foldl Dict.union
                                        Dict.empty
                                        (RawFile.imports rawFile
                                            ++ defaultImports
                                            |> List.map
                                                (importedBinopsFromImport model.modules)
                                        )
                            in
                            case
                                Inference.inferHole
                                    { function = function
                                    , range = range
                                    , binops = binops
                                    , values = List.foldl Dict.union Dict.empty values
                                    , aliases = thisModule.aliases
                                    }
                                    |> Result.mapError
                                        (inferenceErrorToString
                                            (List.foldl Dict.union Dict.empty values)
                                            thisModule.aliases
                                            thisModule.unions
                                            range
                                            function
                                        )
                                    |> Result.map
                                        (generateCompletions range
                                            values
                                            thisModule.aliases
                                            thisModule.unions
                                        )
                            of
                                Err error ->
                                    Cmd.batch
                                        [ toJS (Encode.string error)
                                        , completions []
                                        ]

                                Ok cmd ->
                                    cmd
                    )


defaultImports : List Import
defaultImports =
    -- import Basics exposing (..)
    -- import List exposing (List, (::))
    -- import Maybe exposing (Maybe(..))
    -- import Result exposing (Result(..))
    -- import String exposing (String)
    -- import Char exposing (Char)
    -- import Tuple
    --
    -- import Debug
    --
    -- import Platform exposing ( Program )
    -- import Platform.Cmd as Cmd exposing ( Cmd )
    -- import Platform.Sub as Sub exposing ( Sub )
    [ { moduleName = Node emptyRange [ "Basics" ]
      , moduleAlias = Nothing
      , exposingList = Just (Node emptyRange (All emptyRange))
      }
    , { moduleName = Node emptyRange [ "List" ]
      , moduleAlias = Nothing
      , exposingList =
            Just <|
                Node emptyRange <|
                    Explicit
                        [ Node emptyRange (TypeOrAliasExpose "List")
                        , Node emptyRange (InfixExpose "(::)")
                        ]
      }
    , { moduleName = Node emptyRange [ "Maybe" ]
      , moduleAlias = Nothing
      , exposingList =
            Just <|
                Node emptyRange <|
                    Explicit
                        [ Node emptyRange <|
                            TypeExpose
                                { name = "Maybe"
                                , open = Just emptyRange
                                }
                        ]
      }
    , { moduleName = Node emptyRange [ "Result" ]
      , moduleAlias = Nothing
      , exposingList =
            Just <|
                Node emptyRange <|
                    Explicit
                        [ Node emptyRange <|
                            TypeExpose
                                { name = "Result"
                                , open = Just emptyRange
                                }
                        ]
      }
    , { moduleName = Node emptyRange [ "String" ]
      , moduleAlias = Nothing
      , exposingList =
            Just <|
                Node emptyRange <|
                    Explicit
                        [ Node emptyRange (TypeOrAliasExpose "String") ]
      }
    , { moduleName = Node emptyRange [ "Char" ]
      , moduleAlias = Nothing
      , exposingList =
            Just <|
                Node emptyRange <|
                    Explicit
                        [ Node emptyRange (TypeOrAliasExpose "Char") ]
      }
    , { moduleName = Node emptyRange [ "Tuple" ]
      , moduleAlias = Nothing
      , exposingList = Nothing
      }
    , { moduleName = Node emptyRange [ "Debug" ]
      , moduleAlias = Nothing
      , exposingList = Nothing
      }
    , { moduleName = Node emptyRange [ "Platform" ]
      , moduleAlias = Nothing
      , exposingList =
            Just <|
                Node emptyRange <|
                    Explicit
                        [ Node emptyRange (TypeOrAliasExpose "Platform") ]
      }
    , { moduleName = Node emptyRange [ "Platform", "Cmd" ]
      , moduleAlias = Just (Node emptyRange [ "Cmd" ])
      , exposingList =
            Just <|
                Node emptyRange <|
                    Explicit
                        [ Node emptyRange (TypeOrAliasExpose "Cmd") ]
      }
    , { moduleName = Node emptyRange [ "Platform", "Sub" ]
      , moduleAlias = Just (Node emptyRange [ "Sub" ])
      , exposingList =
            Just <|
                Node emptyRange <|
                    Explicit
                        [ Node emptyRange (TypeOrAliasExpose "Sub") ]
      }
    ]


importedBinopsFromImport : Dict String ModuleData -> Import -> Dict String ( Infix, Type )
importedBinopsFromImport modules { moduleName } =
    let
        (Node _ name) =
            moduleName
    in
    case
        modules
            |> Dict.values
            |> List.find (\moduleData -> moduleData.name == name)
    of
        Nothing ->
            Dict.empty

        Just moduleData ->
            moduleData.module_.binops
                |> Dict.map
                    (\_ infix ->
                        let
                            (Node _ function) =
                                infix.function
                        in
                        ( infix
                        , Dict.get function moduleData.module_.values
                            |> Maybe.withDefault (Var "a")
                        )
                    )


importedValuesFromImport :
    Dict String ModuleData
    -> Import
    -> List ( String, Type )
importedValuesFromImport modules import_ =
    let
        (Node _ moduleName) =
            import_.moduleName

        ---- QUALIFY VALUES
        qualifyValue name =
            case import_.exposingList of
                Nothing ->
                    qualifyHelp name

                Just (Node _ (All _)) ->
                    name

                Just (Node _ (Explicit topLevelExposes)) ->
                    if List.any (isExposedValue name) topLevelExposes then
                        name

                    else
                        qualifyHelp name

        isExposedValue name (Node _ topLevelExpose) =
            case topLevelExpose of
                InfixExpose infixName ->
                    infixName == name

                FunctionExpose functionName ->
                    functionName == name

                TypeOrAliasExpose _ ->
                    False

                TypeExpose _ ->
                    False

        ---- QUALIFY CONSTRUCTORS
        qualifyConstructor ( name, ( typeName, tipe ) ) =
            case import_.exposingList of
                Nothing ->
                    ( qualifyHelp name, tipe )

                Just (Node _ (All _)) ->
                    ( name, tipe )

                Just (Node _ (Explicit topLevelExposes)) ->
                    if List.any (isExposedConstructor typeName) topLevelExposes then
                        ( name, tipe )

                    else
                        ( qualifyHelp name, tipe )

        isExposedConstructor typeName (Node _ topLevelExpose) =
            case topLevelExpose of
                InfixExpose infixName ->
                    False

                FunctionExpose functionName ->
                    False

                TypeOrAliasExpose _ ->
                    False

                TypeExpose exposedType ->
                    exposedType.name == typeName

        ---- HELP
        qualifyHelp name =
            case import_.moduleAlias of
                Nothing ->
                    moduleNameToString moduleName ++ "." ++ name

                Just (Node _ aliasedName) ->
                    moduleNameToString aliasedName ++ "." ++ name
    in
    case
        modules
            |> Dict.values
            |> List.find (\moduleData -> moduleData.name == moduleName)
    of
        Nothing ->
            []

        Just moduleData ->
            List.concat
                [ moduleData.module_.exposedValues
                    |> Dict.toList
                    |> List.map (Tuple.mapFirst qualifyValue)
                , moduleData.module_.exposedConstructors
                    |> Dict.toList
                    |> List.map qualifyConstructor
                ]


moduleNameToString : ModuleName -> String
moduleNameToString =
    String.join "."


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
    -> List (Dict String Type)
    -> List Alias
    -> List Union
    -> ( Type, Dict String Type )
    -> Cmd Msg
generateCompletions range globalValues aliases unions ( tipe, localValues ) =
    let
        addValues generator =
            List.foldl
                (\values ->
                    Generator.addValues (Dict.map (\_ -> Type.normalize aliases) values)
                )
                generator
                globalValues
    in
    Generator.default
        |> addValues
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
