port module Main exposing (main)

import Browser
import Declaration exposing (Declaration(..))
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Elm.Docs exposing (Alias, Module)
import Elm.Type exposing (Type(..))
import Expr exposing (Expr)
import Html exposing (Html)
import Html.Attributes
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Set
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
    { coreModules : List Module
    , targetType : String
    , code : String

    -- ALGORITHMS
    , suggestRecordUpdates : Bool
    , suggestTuples : Bool
    , suggestExactMatches : Bool
    , suggestOnceEvaluated : Bool
    , suggestTwiceEvaluated : Bool
    }


type alias Flags =
    { localStorage : String
    , coreJson : Value
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    case
        ( Decode.decodeString localStorageDecoder flags.localStorage
        , Decode.decodeValue (Decode.list Elm.Docs.decoder) flags.coreJson
        )
    of
        ( Ok storage, Ok coreModules ) ->
            ( { coreModules = coreModules
              , targetType = ""
              , code = storage.code

              -- ALGORITHMS
              , suggestRecordUpdates = True
              , suggestTuples = True
              , suggestExactMatches = True
              , suggestOnceEvaluated = True
              , suggestTwiceEvaluated = True
              }
            , Cmd.none
            )

        _ ->
            ( { coreModules = []
              , targetType = ""
              , code = ""

              -- ALGORITHMS
              , suggestRecordUpdates = True
              , suggestTuples = True
              , suggestExactMatches = True
              , suggestOnceEvaluated = True
              , suggestTwiceEvaluated = True
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        (Element.row
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ Element.column
                [ Element.width (Element.fillPortion 1)
                , Element.height Element.fill
                , Element.padding 64
                , Element.spacing 32
                , Font.family
                    [ Font.monospace ]
                , Font.size 16
                ]
                [ Input.text
                    [ Element.spacing 8
                    , Element.width Element.fill
                    ]
                    { onChange = TargetTypeChanged
                    , text = model.targetType
                    , placeholder = Nothing
                    , label =
                        Input.labelAbove [ Font.bold ]
                            (Element.text "Target type")
                    }
                , viewSuggesters model
                , case decodeTargetType model.targetType of
                    Nothing ->
                        Element.none

                    Just targetType ->
                        Element.column
                            [ Element.width Element.fill
                            , Element.height Element.fill
                            , Element.spacing 16
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
            , Element.column
                [ Element.width (Element.fillPortion 2)
                , Element.height Element.fill
                , Element.spacing 32
                , Element.padding 64
                , Font.family
                    [ Font.monospace ]
                , Font.size 16
                ]
                [ Input.multiline
                    [ Element.spacing 8
                    , Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    { onChange = CodeChanged
                    , text = model.code
                    , placeholder = Nothing
                    , spellcheck = False
                    , label =
                        Input.labelAbove [ Font.bold ]
                            (Element.text "Local custom types, type aliases and declarations")
                    }
                ]
            ]
        )


viewSuggesters : Model -> Element Msg
viewSuggesters model =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 16
        ]
        [ Element.el [ Font.bold ]
            (Element.text "Algorithms")
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
                { onChange = SuggestExactMatchesChecked
                , checked = model.suggestExactMatches
                , label = Element.text "Suggest exact matches"
                }
            , viewSuggesterCheckbox
                { onChange = SuggestOnceEvaluatedChecked
                , checked = model.suggestOnceEvaluated
                , label = Element.text "Suggest functions along with their 1st argument"
                }
            , viewSuggesterCheckbox
                { onChange = SuggestTwiceEvaluatedChecked
                , checked = model.suggestTwiceEvaluated
                , label = Element.text "Suggest functions along with their 1st and 2nd arguments"
                }
            ]
        ]


bold text =
    Element.el [ Font.bold ] <|
        Element.text text


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
    Element.column
        [ Element.spacing 16
        , Element.scrollbarY
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        (List.map viewExpr exprs)


viewExpr : Expr -> Element msg
viewExpr expr =
    Element.column
        [ Font.family
            [ Font.monospace ]
        , Element.spacing 8
        ]
        (List.map Element.text <|
            String.split "\n" (Expr.toString expr)
        )


type Msg
    = NoOp
    | GotCoreModule (Result Http.Error (List Module))
      --
    | TargetTypeChanged String
    | CodeChanged String
      -- ALGORITHMS
    | SuggestRecordUpdatesChecked Bool
    | SuggestTuplesChecked Bool
    | SuggestExactMatchesChecked Bool
    | SuggestOnceEvaluatedChecked Bool
    | SuggestTwiceEvaluatedChecked Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotCoreModule _ ->
            ( model, Cmd.none )

        TargetTypeChanged newTargetType ->
            ( { model | targetType = newTargetType }
            , Cmd.none
            )

        CodeChanged newCode ->
            ( { model | code = newCode }
            , cache (encodeLocalStorage { code = newCode })
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

        knownValues =
            model.coreModules
                |> List.map valuesFromModule
                |> List.foldl Dict.union Dict.empty
                |> Dict.union localValues
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
                |> Dict.union usefulConstants

        usefulConstants =
            Dict.fromList
                [ ( "0", Type "Int" [] )
                , ( "\"\"", Type "String" [] )
                ]
    in
    List.concat
        [ suggestHelp model
            knownValues
            (Type.normalize localAliases targetType)
        ]


suggestHelp : Model -> Dict String Type -> Type -> List Expr
suggestHelp model knownValues targetType =
    List.concat <|
        List.filterMap identity
            [ if model.suggestRecordUpdates then
                Just (Expr.suggestRecordUpdate knownValues targetType)

              else
                Nothing
            , if model.suggestTuples then
                Just
                    (Expr.suggestCreateTuple (suggestHelp model knownValues)
                        knownValues
                        targetType
                    )

              else
                Nothing
            , if model.suggestExactMatches then
                Just (Expr.suggestDirect knownValues targetType)

              else
                Nothing
            , if model.suggestOnceEvaluated then
                Just (Expr.suggestWithArgument knownValues targetType)

              else
                Nothing
            , if model.suggestTwiceEvaluated then
                Just (Expr.suggestWithTwoArguments knownValues targetType)

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
    { code : String }


localStorageDecoder : Decoder Storage
localStorageDecoder =
    Decode.succeed Storage
        |> Decode.required "code" Decode.string



---- ENCODE


encodeLocalStorage : Storage -> Value
encodeLocalStorage storage =
    Encode.object
        [ ( "code", Encode.string storage.code )
        ]
