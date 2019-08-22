port module Worker exposing (main)

import Canonical exposing (Alias, Module, ModuleData, Store, TodoItem, Union)
import Canonical.Annotation exposing (Annotation)
import Canonical.Type exposing (Type(..))
import Dict exposing (Dict)
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
import Parser
import Result.Extra as Result
import Set exposing (Set)
import Src


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
    { moduleName = RawFile.moduleName rawFile
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
                    let
                        newStore =
                            Canonical.add moduleData model.store
                    in
                    ( { model | store = newStore }
                    , Cmd.batch
                        [ store file data
                        , log ("reparsed " ++ file.fileName)
                        ]
                    )

                Err e ->
                    ( model
                    , log (file.fileName ++ ": " ++ e)
                    )

        Restore cached ->
            case parseCached cached of
                Ok moduleData ->
                    let
                        newStore =
                            Canonical.add moduleData model.store
                    in
                    ( { model | store = newStore }
                    , log ("restored " ++ cached.fileName)
                    )

                Err e ->
                    ( model
                    , Cmd.batch
                        [ log e
                        , log cached.fileName
                        ]
                    )

        For completionRequest ->
            ( model
            , case computeCompletions model.store completionRequest of
                Err error ->
                    Cmd.batch
                        [ log (errorToString error)
                        , completions []
                        ]

                Ok result ->
                    Cmd.batch
                        [ log (completionResultToString result)
                        , result.completions
                            |> List.map (completionToString result.range result.isArgument)
                            |> completions
                        ]
            )



---- COMPUTE COMPLETIONS


type Error
    = CannotGetRange
    | ParserError (List Parser.DeadEnd)
    | CannotFindFunctionDeclaration
    | CannotFindCurrentModule (List TodoItem) (Dict ModuleName Module) ModuleName
    | InferenceError CompletionRequest (Dict String Annotation) Module Range Function Inference.Error


computeCompletions : Store -> CompletionRequest -> Result Error CompletionResult
computeCompletions currentStore ({ row, column, src, fileName } as request) =
    Result.andThen2 (runInfer request currentStore)
        (computeRange row column src)
        (parseModule src)


computeRange : Int -> Int -> String -> Result Error Range
computeRange row column src =
    case
        src
            |> String.lines
            |> List.getAt (row - 1)
            |> Maybe.andThen
                (String.left (column - 1)
                    >> String.words
                    >> List.last
                )
            |> Maybe.map String.length
    of
        Nothing ->
            Err CannotGetRange

        Just wordLength ->
            Ok
                { start =
                    { column = column - wordLength
                    , row = row
                    }
                , end =
                    { column = column
                    , row = row
                    }
                }


parseModule : String -> Result Error RawFile
parseModule src =
    Result.mapError ParserError (Parser.parse src)


runInfer : CompletionRequest -> Store -> Range -> RawFile -> Result Error CompletionResult
runInfer request currentStore range rawFile =
    let
        ({ moduleName, file } as currentModuleData) =
            toModuleData request.fileName rawFile

        newStore =
            Canonical.replace currentModuleData currentStore
    in
    Result.andThen2 (infer request range moduleName)
        (getFunctionDeclaration range file)
        (findModule newStore.todo newStore.done moduleName)


getFunctionDeclaration : Range -> File -> Result Error Function
getFunctionDeclaration range file =
    Result.fromMaybe
        CannotFindFunctionDeclaration
        (Src.functionDeclarationAt range file)


findModule : List TodoItem -> Dict ModuleName Module -> ModuleName -> Result Error Module
findModule todo done moduleName =
    Result.fromMaybe
        (CannotFindCurrentModule todo done moduleName)
        (Dict.get moduleName done)



---- INFER


infer :
    CompletionRequest
    -> Range
    -> ModuleName
    -> Function
    -> Module
    -> Result Error CompletionResult
infer request range moduleName function currentModule =
    let
        globalValues =
            Dict.foldl
                (\qualifier values allValues ->
                    Dict.union allValues
                        (values
                            |> Dict.toList
                            |> List.filterMap (qualify qualifier)
                            |> Dict.fromList
                        )
                )
                Dict.empty
                currentModule.qualifiedValues
    in
    Inference.inferHole
        { function = function
        , range = range
        , moduleName = moduleName
        , currentModule = currentModule
        }
        |> Result.mapError
            (InferenceError request globalValues currentModule range function)
        |> Result.map
            (generateCompletions request
                range
                [ currentModule.values, standardValues, globalValues ]
                currentModule.aliases
                currentModule.unions
            )


qualify : ModuleName -> ( String, Annotation ) -> Maybe ( String, Annotation )
qualify qualifier ( name, annotation ) =
    if name == "always" then
        Nothing

    else if name == "identity" then
        Nothing

    else if name == "toString" then
        Nothing

    else if name == "todo" then
        Nothing

    else
        Just ( Src.qualifiedName qualifier name, annotation )


standardValues : Dict String Annotation
standardValues =
    Dict.fromList
        [ ( "[]", Canonical.Annotation.fromType (Canonical.Type.list (Var "a")) )
        , ( "\"\"", Canonical.Annotation.fromType Canonical.Type.string )
        , ( "0", Canonical.Annotation.fromType (Var "number") )
        ]



---- GENERATE COMPLETIONS


generateCompletions :
    CompletionRequest
    -> Range
    -> List (Dict String Annotation)
    -> Dict String Alias
    -> Dict String Union
    -> ( Type, Bool, Dict String Type )
    -> CompletionResult
generateCompletions request range globalValues aliases unions ( tipe, isArgument, localValues ) =
    let
        addGlobalValues generator =
            List.foldl Generator.addValues generator globalValues
    in
    { fileName = request.fileName
    , src = request.src
    , row = request.row
    , column = request.column
    , range = range
    , tipe = tipe
    , completions =
        Generator.default
            |> addGlobalValues
            |> Generator.addValues (Dict.map (\_ -> Canonical.Annotation.fromType) localValues)
            |> Generator.addUnions unions
            |> Generator.for tipe
    , isArgument = isArgument
    }


completionToString : Range -> Bool -> Expr -> String
completionToString range isArgument completion =
    case String.lines (Generator.exprToText isArgument completion) of
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


type alias CompletionResult =
    { fileName : String
    , src : String
    , row : Int
    , column : Int
    , range : Range
    , tipe : Type
    , completions : List Expr
    , isArgument : Bool
    }


completionResultToString : CompletionResult -> String
completionResultToString result =
    let
        addBulletPoint text =
            ">" ++ String.dropLeft 1 text
    in
    String.join "\n"
        [ ""
        , ""
        , ""
        , "--- SUCCESSFULLY COMPUTED COMPLETIONS ------------------------------------------"
        , ""
        , meta result.fileName result.row result.column
        , ""
        , snippet result.range result.src
        , ""
        , "Infered type: " ++ Canonical.Type.toString result.tipe
        , ""
        , "Completions:"
        , ""
        , String.join "\n" <|
            List.map
                (addBulletPoint
                    << indent 2
                    << Generator.exprToString result.isArgument
                )
                result.completions
        ]


meta : String -> Int -> Int -> String
meta fileName row column =
    String.join "\n"
        [ "File:   " ++ fileName
        , "Row:    " ++ String.fromInt row
        , "Column: " ++ String.fromInt column
        ]


snippet : Range -> String -> String
snippet range src =
    let
        displayedLines =
            5

        lineNumber index =
            String.padLeft lineNumberWidth ' ' <|
                String.fromInt (range.start.row - displayedLines + index)

        lineNumberWidth =
            String.length (String.fromInt range.start.row)
    in
    String.concat
        [ src
            |> String.lines
            |> List.drop (range.start.row - displayedLines)
            |> List.take displayedLines
            |> List.indexedMap
                (\index line ->
                    lineNumber index ++ "|" ++ line
                )
            |> String.join "\n"
        , "\n"
        , String.repeat (range.start.column + lineNumberWidth) " "
        , String.repeat (range.end.column - range.start.column) "^"
        ]


indent : Int -> String -> String
indent amount text =
    let
        indentation =
            String.repeat amount " "
    in
    text
        |> String.lines
        |> List.map (\line -> indentation ++ line)
        |> String.join "\n"



---- ERROR TO STRING


errorToString : Error -> String
errorToString error =
    let
        title text =
            "=== " ++ text ++ " " ++ String.repeat (75 - String.length text) "="

        wrap text =
            String.join "\n"
                [ ""
                , ""
                , ""
                , text
                ]
    in
    case error of
        CannotGetRange ->
            wrap <|
                title "CANNOT COMPUTE RANGE"

        ParserError deadEnds ->
            wrap <|
                title "CANNOT PARSE MODULE"

        CannotFindFunctionDeclaration ->
            wrap <|
                title "CANNOT FIND FUNCTION DECLARATION"

        CannotFindCurrentModule todo done moduleName ->
            let
                todoToString todoItem =
                    String.concat
                        [ moduleNameToString
                            todoItem.moduleData.moduleName
                        , " blocked by "
                        , String.join ", " <|
                            List.map moduleNameToString <|
                                Set.toList todoItem.blockedBy
                        ]

                moduleNameToString =
                    String.join "."
            in
            wrap <|
                String.concat
                    [ title "CANNOT FIND MODULE"
                    , moduleNameToString moduleName
                    , ". The following modules are available:\n\n  "
                    , String.join "\n  " (List.map moduleNameToString (Dict.keys done))
                    , "\n\nThe following modules are blocked:\n\n  "
                    , String.join "\n  " (List.map todoToString todo)
                    ]

        InferenceError request globalValues currentModule range function inferenceError ->
            wrap <|
                String.join "\n"
                    [ title "CANNOT INFER VALUE"
                    , ""
                    , meta request.fileName request.row request.column
                    , ""
                    , snippet range request.src
                    , ""
                    , Inference.errorToString inferenceError
                    , ""
                    , ""
                    , availableDataToString
                        (Dict.union globalValues currentModule.values)
                        currentModule.aliases
                        currentModule.unions
                    ]


availableDataToString :
    Dict String Annotation
    -> Dict String Alias
    -> Dict String Union
    -> String
availableDataToString values aliases unions =
    let
        unionToString ( name, union ) =
            String.concat
                [ String.join " " (name :: union.vars)
                , " = "
                , union.constructors
                    |> List.map constructorToString
                    |> String.join " | "
                ]

        constructorToString ( name, types ) =
            name
                :: List.map Canonical.Type.toString types
                |> String.join " "

        valueToString ( fieldName, fieldAnnotation ) =
            String.concat
                [ fieldName
                , " : "
                , Canonical.Annotation.toString fieldAnnotation
                ]
    in
    String.join "\n"
        [ "Unions:"
        , ""
        , unions
            |> Dict.toList
            |> List.map unionToString
            |> List.map (\line -> "  " ++ line)
            |> String.join "\n"
        , ""
        , ""
        , "Values:"
        , ""
        , List.map valueToString (Dict.toList values)
            |> List.map (\line -> "  " ++ line)
            |> String.join "\n"
        ]


log : String -> Cmd msg
log text =
    toJS (Encode.string text)


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
