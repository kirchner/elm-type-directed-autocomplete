port module Main exposing (main)

{-

   Copyright 2019 Fabian Kirchner

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

-}

import Browser
import Declaration exposing (Declaration(..))
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Elm.Docs exposing (Alias, Module, Union)
import Elm.Type exposing (Type(..))
import File exposing (File)
import File.Select
import Generator
    exposing
        ( Expr
        , Generator
        , accessor
        , addUnions
        , addValues
        , all
        , call
        , cases
        , exprToText
        , field
        , first
        , for
        , recordUpdate
        , takeValues
        , tuple
        , value
        )
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Task
import Type


port cache : Value -> Cmd msg


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { modules : List Module
    , packages : List String
    , targetType : String
    , code : String

    -- ALGORITHMS
    , suggestRecordUpdates : Bool
    , suggestTuples : Bool
    , suggestCases : Bool
    , suggestExactMatches : Bool
    , suggestOnceEvaluated : Bool
    , suggestTwiceEvaluated : Bool

    -- PACKAGES
    , importPackagesHover : Bool
    }


type alias Flags =
    { localStorage : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    case Decode.decodeString localStorageDecoder flags.localStorage of
        Ok storage ->
            ( { modules =
                    storage.packages
                        |> List.filterMap
                            (\rawPackage ->
                                rawPackage
                                    |> Decode.decodeString (Decode.list Elm.Docs.decoder)
                                    |> Result.toMaybe
                            )
                        |> List.concat
              , packages = storage.packages
              , targetType = ""
              , code = storage.code

              -- ALGORITHMS
              , suggestRecordUpdates = True
              , suggestTuples = True
              , suggestCases = True
              , suggestExactMatches = True
              , suggestOnceEvaluated = True
              , suggestTwiceEvaluated = True

              -- PACKAGES
              , importPackagesHover = False
              }
            , Cmd.none
            )

        Err _ ->
            ( { modules = []
              , packages = []
              , targetType = ""
              , code = defaultCode

              -- ALGORITHMS
              , suggestRecordUpdates = True
              , suggestTuples = True
              , suggestCases = True
              , suggestExactMatches = True
              , suggestOnceEvaluated = True
              , suggestTwiceEvaluated = True

              -- PACKAGES
              , importPackagesHover = False
              }
            , Cmd.batch
                [ Http.get
                    { url = "/elm-type-directed-autocomplete/docs.json"
                    , expect = Http.expectString GotPackage
                    }
                , cache
                    (encodeLocalStorage
                        { code = defaultCode
                        , packages = []
                        }
                    )
                ]
            )


defaultCode : String
defaultCode =
    """type alias Model =
  { time : Posix
  , name : String
  }

type Msg
  = Tick Posix
  | NameChanged String

model : Model

msg : Msg
"""


view : Model -> Html Msg
view model =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        (Element.row
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 32
            , Element.spacing 16
            ]
            [ viewTargetType model
            , viewCode model
            , viewOptions model
            ]
        )


viewOptions : Model -> Element Msg
viewOptions model =
    Element.column
        [ Element.width (Element.fillPortion 1)
        , Element.height Element.fill
        , Element.spacing 32
        , Font.family
            [ Font.monospace ]
        , Font.size 16
        ]
        [ viewSuggesters model
        , viewModules model
        ]


viewCode : Model -> Element Msg
viewCode model =
    Input.multiline
        [ Element.spacing 8
        , Element.width (Element.fillPortion 2)
        , Element.height Element.fill
        , Font.family
            [ Font.monospace ]
        , Font.size 16
        ]
        { onChange = CodeChanged
        , text = model.code
        , placeholder = Nothing
        , spellcheck = False
        , label =
            Input.labelAbove [ Font.bold ]
                (Element.text "Custom types, type aliases and declarations")
        }


viewTargetType : Model -> Element Msg
viewTargetType model =
    Element.column
        [ Element.width (Element.fillPortion 2)
        , Element.height Element.fill
        , Element.spacing 32
        , Font.family
            [ Font.monospace ]
        , Font.size 16
        ]
        [ Input.text
            [ Element.spacing 8
            , Element.width Element.fill
            , Input.focusedOnLoad
            ]
            { onChange = TargetTypeChanged
            , text = model.targetType
            , placeholder = Nothing
            , label =
                Input.labelAbove [] <|
                    Element.column
                        [ Element.width Element.fill
                        , Element.spacing 8
                        ]
                        [ Element.el [ Font.bold ]
                            (Element.text "Target type")
                        , Element.paragraph
                            [ Font.size 14
                            , Font.color (Element.rgb 0.3 0.3 0.3)
                            ]
                            [ Element.text "Try something like "
                            , bold "List String -> String"
                            , Element.text ", "
                            , bold "List Int -> List Int"
                            , Element.text " or one of the types you have defined on the left."
                            ]
                        , Element.paragraph
                            [ Font.size 14
                            , Font.color (Element.rgb 0.3 0.3 0.3)
                            ]
                            [ Element.text "To get more results you can activate more "
                            , bold "Algorithms"
                            , Element.text " or add more "
                            , bold "Modules"
                            , Element.text "."
                            ]
                        ]
            }
        , case decodeTargetType model.targetType of
            Nothing ->
                Element.none

            Just targetType ->
                Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.spacing 32
                    , Element.clip
                    , Element.htmlAttribute <|
                        Html.Attributes.style "flex-shrink" "1"
                    ]
                    [ Element.el [ Font.bold ]
                        (Element.text "Suggestions")
                    , viewExprs <|
                        suggest model
                            targetType
                    ]
        ]


viewSuggesters : Model -> Element Msg
viewSuggesters model =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 16
        ]
        [ Element.column
            [ Element.width Element.fill
            , Element.spacing 8
            ]
            [ Element.el [ Font.bold ]
                (Element.text "Algorithms")
            , Element.paragraph
                [ Font.size 14
                , Font.color (Element.rgb 0.3 0.3 0.3)
                ]
                [ Element.text "If you have a lot of modules/custom types/type aliases/declarations loaded, you may not want to select many algorithms."
                ]
            ]
        , Element.column
            [ Element.width Element.fill
            , Element.spacing 8
            ]
            [ viewSuggesterCheckbox
                { onChange = SuggestRecordUpdatesChecked
                , checked = model.suggestRecordUpdates
                , label = Element.text "Suggest record updates"
                }
            , viewSuggesterCheckbox
                { onChange = SuggestTuplesChecked
                , checked = model.suggestTuples
                , label = Element.text "Suggest tuples"
                }
            , viewSuggesterCheckbox
                { onChange = SuggestCasesChecked
                , checked = model.suggestCases
                , label = Element.text "Suggest case expressions"
                }
            , viewSuggesterCheckbox
                { onChange = SuggestExactMatchesChecked
                , checked = model.suggestExactMatches
                , label = Element.text "Suggest exact matches"
                }
            , viewSuggesterCheckbox
                { onChange = SuggestOnceEvaluatedChecked
                , checked = model.suggestOnceEvaluated
                , label = Element.text "Suggest functions with 1st argument"
                }
            , viewSuggesterCheckbox
                { onChange = SuggestTwiceEvaluatedChecked
                , checked = model.suggestTwiceEvaluated
                , label = Element.text "Suggest functions with 1st & 2nd arguments"
                }
            ]
        ]


bold text =
    Element.el [ Font.bold ] <|
        Element.text text


viewSuggesterCheckbox :
    { onChange : Bool -> Msg
    , checked : Bool
    , label : Element Msg
    }
    -> Element Msg
viewSuggesterCheckbox { onChange, checked, label } =
    Input.checkbox
        [ Element.width Element.fill
        ]
        { onChange = onChange
        , icon = Input.defaultCheckbox
        , checked = checked
        , label =
            Input.labelRight
                [ Element.width Element.fill ]
                label
        }


viewModules : Model -> Element Msg
viewModules model =
    let
        hoverAttributes attrs =
            if model.importPackagesHover then
                Background.color (Element.rgb 0.9 0.9 0.9)
                    :: attrs

            else
                attrs
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing 16
        ]
        [ Element.column
            [ Element.width Element.fill
            , Element.spacing 8
            ]
            [ Element.el [ Font.bold ]
                (Element.text "Modules")
            , Element.paragraph
                [ Font.size 14
                , Font.color (Element.rgb 0.3 0.3 0.3)
                ]
                [ Element.text "You have to upload the "
                , bold "docs.json"
                , Element.text " files of an Elm package in order to add its modules to the index. A simple way is to save the links below somewhere on your machine so you can drag and drop them here."
                ]
            ]
        , Element.paragraph
            [ Element.width Element.fill
            , Element.spacing 8
            ]
            [ Element.text <|
                String.join ", " <|
                    List.sort <|
                        List.map .name model.modules
            ]
        , if List.isEmpty model.modules then
            Element.none

          else
            Input.button
                [ Element.paddingXY 16 8
                , Border.rounded 5
                , Border.width 1
                , Border.color (Element.rgb 0 0 0)
                , Element.mouseOver
                    [ Background.color (Element.rgb 0.9 0.9 0.9) ]
                ]
                { onPress = Just RemoveAllModulesPressed
                , label =
                    Element.el
                        [ Font.bold ]
                        (Element.text "Remove all modules")
                }
        , Element.row
            [ Element.spacing 8
            , Element.width Element.fill
            ]
            [ Element.el
                ([ Element.width (Element.fillPortion 3)
                 , Element.height Element.fill
                 , Border.width 2
                 , Border.rounded 4
                 , Border.dashed
                 , hijackOn "dragenter" (Decode.succeed ImportPackagesDragEnter)
                 , hijackOn "dragover" (Decode.succeed ImportPackagesDragEnter)
                 , hijackOn "dragleave" (Decode.succeed ImportPackagesDragLeave)
                 , hijackOn "drop"
                    (Decode.at [ "dataTransfer", "files" ]
                        (Decode.oneOrMore ImportPackagesGotFiles File.decoder)
                    )
                 ]
                    |> hoverAttributes
                )
                (Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                    (Input.button
                        [ Element.paddingXY 16 8
                        , Border.rounded 5
                        , Border.width 1
                        , Border.color (Element.rgb 0 0 0)
                        , Element.mouseOver
                            [ Background.color (Element.rgb 0.9 0.9 0.9) ]
                        ]
                        { onPress = Just ImportPackagesPick
                        , label =
                            Element.el
                                [ Font.bold ]
                                (Element.text "Upload")
                        }
                    )
                )
            , Element.column
                [ Element.width (Element.fillPortion 1)
                , Element.spacing 8
                , Element.padding 4
                ]
                (List.map viewDocsLink
                    [ "elm/core"
                    , "elm/html"
                    , "elm/json"
                    , "elm/browser"
                    , "elm/url"
                    , "elm/http"
                    ]
                )
            ]
        ]


viewDocsLink : String -> Element msg
viewDocsLink name =
    Element.download
        [ Font.underline
        , Element.mouseOver
            [ Font.color (Element.rgb (59 / 255) (153 / 255) (252 / 255)) ]
        ]
        { url = "https://package.elm-lang.org/packages/" ++ name ++ "/latest/docs.json"
        , label = Element.text name
        }


hijackOn : String -> Decoder msg -> Element.Attribute msg
hijackOn event decoder =
    Element.htmlAttribute <|
        Html.Events.preventDefaultOn event (Decode.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )


decodeTargetType : String -> Maybe Type
decodeTargetType targetType =
    if targetType == "" then
        Nothing

    else
        Decode.decodeString Elm.Type.decoder ("\"" ++ targetType ++ "\"")
            |> Result.toMaybe


valuesFromModule : Module -> Dict String Type
valuesFromModule module_ =
    let
        nameAndType { name, tipe } =
            ( module_.name ++ "." ++ name, tipe )
    in
    module_.values
        |> List.map nameAndType
        |> Dict.fromList


viewExprs : List Expr -> Element msg
viewExprs exprs =
    let
        horizontalBar =
            Element.el
                [ Element.paddingXY 128 0
                , Element.width Element.fill
                ]
                (Element.el
                    [ Element.width Element.fill
                    , Element.height (Element.px 0)
                    , Border.widthEach
                        { top = 1
                        , bottom = 0
                        , left = 0
                        , right = 0
                        }
                    , Border.color (Element.rgb 0.8 0.8 0.8)
                    ]
                    Element.none
                )
    in
    Element.column
        [ Element.spacing 32
        , Element.scrollbarY
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        (List.intersperse horizontalBar
            (List.map viewExpr exprs)
        )


viewExpr : Expr -> Element msg
viewExpr expr =
    Element.column
        [ Font.family
            [ Font.monospace ]
        , Element.spacing 8
        ]
        (List.map Element.text <|
            String.split "\n" (exprToText expr)
        )


type Msg
    = TargetTypeChanged String
    | CodeChanged String
      -- ALGORITHMS
    | SuggestRecordUpdatesChecked Bool
    | SuggestTuplesChecked Bool
    | SuggestCasesChecked Bool
    | SuggestExactMatchesChecked Bool
    | SuggestOnceEvaluatedChecked Bool
    | SuggestTwiceEvaluatedChecked Bool
      -- PACKAGES
    | GotPackage (Result Http.Error String)
    | ImportPackagesDragEnter
    | ImportPackagesDragLeave
    | ImportPackagesGotFiles File (List File)
    | ImportPackagesPick
    | ImportPackagesGotPackage String (Result Decode.Error ( String, List Module ))
    | RemoveAllModulesPressed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TargetTypeChanged newTargetType ->
            ( { model | targetType = newTargetType }
            , Cmd.none
            )

        CodeChanged newCode ->
            ( { model | code = newCode }
            , cache
                (encodeLocalStorage
                    { code = newCode
                    , packages = model.packages
                    }
                )
            )

        -- ALGORITHMS
        SuggestRecordUpdatesChecked value ->
            ( { model | suggestRecordUpdates = value }
            , Cmd.none
            )

        SuggestTuplesChecked value ->
            ( { model | suggestTuples = value }
            , Cmd.none
            )

        SuggestCasesChecked value ->
            ( { model | suggestCases = value }
            , Cmd.none
            )

        SuggestExactMatchesChecked value ->
            ( { model | suggestExactMatches = value }
            , Cmd.none
            )

        SuggestOnceEvaluatedChecked value ->
            ( { model | suggestOnceEvaluated = value }
            , Cmd.none
            )

        SuggestTwiceEvaluatedChecked value ->
            ( { model | suggestTwiceEvaluated = value }
            , Cmd.none
            )

        -- PACKAGES
        GotPackage result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok rawPackage ->
                    case Decode.decodeString (Decode.list Elm.Docs.decoder) rawPackage of
                        Err _ ->
                            ( model, Cmd.none )

                        Ok modules ->
                            let
                                newPackages =
                                    rawPackage :: model.packages
                            in
                            ( { model
                                | modules = model.modules ++ modules
                                , packages = newPackages
                              }
                            , cache
                                (encodeLocalStorage
                                    { code = model.code
                                    , packages = newPackages
                                    }
                                )
                            )

        ImportPackagesDragEnter ->
            ( { model | importPackagesHover = True }
            , Cmd.none
            )

        ImportPackagesDragLeave ->
            ( { model | importPackagesHover = False }
            , Cmd.none
            )

        ImportPackagesGotFiles firstFile otherFiles ->
            ( { model
                | importPackagesHover = False
              }
            , Cmd.batch <|
                List.map
                    (\file ->
                        File.toString file
                            |> Task.andThen
                                (\rawFile ->
                                    case
                                        Decode.decodeString
                                            (Decode.list Elm.Docs.decoder)
                                            rawFile
                                    of
                                        Err error ->
                                            Task.fail error

                                        Ok modules ->
                                            Task.succeed ( rawFile, modules )
                                )
                            |> Task.attempt (ImportPackagesGotPackage (File.name file))
                    )
                    (firstFile :: otherFiles)
            )

        ImportPackagesPick ->
            ( model
            , File.Select.files [ "application/json" ] ImportPackagesGotFiles
            )

        ImportPackagesGotPackage name result ->
            case result of
                Err decodeError ->
                    ( model
                    , Cmd.none
                    )

                Ok ( rawPackage, modules ) ->
                    let
                        newPackages =
                            rawPackage :: model.packages
                    in
                    ( { model
                        | modules = model.modules ++ modules
                        , packages = newPackages
                      }
                    , cache
                        (encodeLocalStorage
                            { code = model.code
                            , packages = newPackages
                            }
                        )
                    )

        RemoveAllModulesPressed ->
            ( { model
                | modules = []
                , packages = []
              }
            , cache
                (encodeLocalStorage
                    { code = model.code
                    , packages = []
                    }
                )
            )


subscriptions _ =
    Sub.none



---- SUGGEST


suggest : Model -> Type -> List Expr
suggest model targetType =
    let
        declarations =
            Declaration.parse model.code

        localValues =
            declarations
                |> List.filterMap
                    (\declaration ->
                        case declaration of
                            Value name tipe ->
                                Just ( name, tipe )

                            _ ->
                                Nothing
                    )
                |> Dict.fromList
                |> Dict.map
                    (\_ tipe ->
                        tipe
                            |> removeScope
                            |> Type.normalize localAliases
                    )

        localAliases =
            declarations
                |> List.filterMap
                    (\declaration ->
                        case declaration of
                            TypeAlias name args tipe ->
                                Just
                                    { name = name
                                    , comment = ""
                                    , args = args
                                    , tipe = tipe
                                    }

                            _ ->
                                Nothing
                    )

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

        moduleValues =
            model.modules
                |> List.map valuesFromModule
                |> List.foldl Dict.union Dict.empty
                |> Dict.filter
                    (\name _ ->
                        not
                            (List.member name
                                [ "Basics.identity"
                                , "Basics.always"
                                , "Debug.todo"
                                , "Debug.toString"
                                , "Debug.log"
                                ]
                            )
                    )
                |> Dict.map
                    (\_ tipe ->
                        tipe
                            |> removeScope
                            |> Type.normalize localAliases
                    )
    in
    List.concat
        [ generator model
            |> addUnions localUnions
            |> addValues moduleValues
            |> addValues localValues
            |> for (Type.normalize localAliases targetType)
        ]


generator : Model -> Generator
generator model =
    all <|
        List.filterMap identity
            [ if model.suggestRecordUpdates then
                Just <|
                    recordUpdate value

              else
                Nothing
            , Just field
            , if model.suggestExactMatches then
                Just value

              else
                Nothing
            , if model.suggestTuples then
                Just <|
                    tuple
                        { first =
                            all
                                [ recordUpdate value
                                , call []
                                , call
                                    [ value
                                    , field
                                    ]
                                ]
                        , second =
                            all
                                [ recordUpdate value
                                , call []
                                , call
                                    [ value
                                    , field
                                    ]
                                ]
                        }

              else
                Nothing
            , if model.suggestCases then
                Just <|
                    cases
                        { matched = call []
                        , branch =
                            \newValues ->
                                all
                                    [ first
                                        [ recordUpdate <|
                                            first
                                                [ value
                                                    |> addValues newValues
                                                    |> takeValues 1
                                                , call
                                                    [ value
                                                        |> addValues newValues
                                                        |> takeValues 1
                                                    ]
                                                ]
                                        , value
                                        ]
                                    , tuple
                                        { first =
                                            first
                                                [ recordUpdate <|
                                                    first
                                                        [ value
                                                            |> addValues newValues
                                                            |> takeValues 1
                                                        , call
                                                            [ value
                                                                |> addValues newValues
                                                                |> takeValues 1
                                                            ]
                                                        ]
                                                , call []
                                                ]
                                        , second =
                                            first
                                                [ call []
                                                , call
                                                    [ value
                                                        |> addValues newValues
                                                    ]
                                                ]
                                        }
                                    ]
                        }

              else
                Nothing
            , if model.suggestOnceEvaluated then
                Just
                    (call
                        [ all
                            [ field
                            , value
                            , accessor
                            , call
                                [ value
                                    |> takeValues 1
                                ]
                            ]
                        ]
                    )

              else
                Nothing
            , Just accessor
            , if model.suggestTwiceEvaluated then
                Just
                    (call
                        [ all
                            [ value
                            , field
                            , accessor
                            , call
                                [ value
                                    |> takeValues 1
                                ]
                            ]
                        , all
                            [ field
                            , value
                                |> takeValues 1
                            ]
                        ]
                    )

              else
                Nothing
            ]


removeScope : Type -> Type
removeScope scopedType =
    case scopedType of
        Var name ->
            Var name

        Type name subTypes ->
            Type
                (String.split "." name
                    |> List.reverse
                    |> List.head
                    |> Maybe.withDefault name
                )
                (List.map removeScope subTypes)

        Lambda typeA typeB ->
            Lambda (removeScope typeA) (removeScope typeB)

        Tuple types ->
            Tuple (List.map removeScope types)

        Record values var ->
            Record
                (List.map (Tuple.mapSecond removeScope) values)
                var



---- DECODER


type alias Storage =
    { code : String
    , packages : List String
    }


localStorageDecoder : Decoder Storage
localStorageDecoder =
    Decode.succeed Storage
        |> Decode.required "code" Decode.string
        |> Decode.required "packages" (Decode.list Decode.string)



---- ENCODE


encodeLocalStorage : Storage -> Value
encodeLocalStorage storage =
    Encode.object
        [ ( "code", Encode.string storage.code )
        , ( "packages", Encode.list Encode.string storage.packages )
        ]
