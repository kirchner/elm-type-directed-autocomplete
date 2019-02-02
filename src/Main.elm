module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Elm.Docs exposing (Module)
import Elm.Type exposing (Type(..))
import Expr exposing (Expr)
import Html exposing (Html)
import Http
import Json.Decode as Decode
import Set
import Type


main : Program {} Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = Loading
    | Loaded
        { coreModules : List Module
        , targetType : String
        , localValues : Dict String Type
        , newName : String
        , newType : String
        }
    | Failed Http.Error


init _ =
    ( Loading
    , Http.get
        { url = "/docs/core.json"
        , expect = Http.expectJson GotCoreModule (Decode.list Elm.Docs.decoder)
        }
    )


view model =
    { title = "type-directed autocomplete"
    , body =
        [ Element.layout [] <|
            case model of
                Loading ->
                    Element.el
                        [ Element.centerX
                        , Element.centerY
                        ]
                        (Element.text "Loading package documentations...")

                Loaded data ->
                    viewLoaded data

                Failed error ->
                    Element.column
                        [ Element.centerX
                        , Element.centerY
                        , Element.spacing 10
                        ]
                        [ Element.text "Something went wrong loading package documentations."
                        ]
        ]
    }


viewLoaded data =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 64
            , Element.spacing 32
            , Font.family
                [ Font.monospace ]
            , Font.size 16
            ]
            (case decodeTargetType data.targetType of
                Just targetType ->
                    let
                        knownValues =
                            data.coreModules
                                |> List.map valuesFromModule
                                |> List.foldl Dict.union Dict.empty
                    in
                    [ Input.text
                        [ Element.spacing 8
                        , Element.width Element.fill
                        ]
                        { onChange = TargetTypeChanged
                        , text = data.targetType
                        , placeholder = Nothing
                        , label =
                            Input.labelAbove [ Font.bold ]
                                (Element.text "Target type")
                        }
                    , Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.spacing 16
                        ]
                        [ Element.el [ Font.bold ]
                            (Element.text "Suggestions")
                        , viewExprs <|
                            Expr.suggest (Dict.union data.localValues knownValues)
                                targetType
                        ]
                    ]

                Nothing ->
                    [ Input.text
                        [ Element.spacing 8
                        , Element.width Element.fill
                        ]
                        { onChange = TargetTypeChanged
                        , text = data.targetType
                        , placeholder = Nothing
                        , label = Input.labelAbove [ Font.bold ] (Element.text "Target type")
                        }
                    , Element.text "This is not a valid type"
                    ]
            )
        , Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 64
            , Element.spacing 32
            , Font.family
                [ Font.monospace ]
            , Font.size 16
            ]
            [ Element.column
                [ Element.width Element.fill
                , Element.spacing 16
                ]
                [ Element.el [ Font.bold ]
                    (Element.text "Local values")
                , viewValues data.localValues
                ]
            , Element.column
                [ Element.width Element.fill
                , Element.spacing 16
                ]
                [ Input.text
                    [ Element.spacing 8
                    , Element.width Element.fill
                    ]
                    { onChange = NewNameChanged
                    , text = data.newName
                    , placeholder = Nothing
                    , label = Input.labelAbove [ Font.bold ] (Element.text "Name")
                    }
                , Input.text
                    [ Element.spacing 8
                    , Element.width Element.fill
                    ]
                    { onChange = NewTypeChanged
                    , text = data.newType
                    , placeholder = Nothing
                    , label = Input.labelAbove [ Font.bold ] (Element.text "Type")
                    }
                , Input.button
                    [ Element.paddingXY 16 8
                    , Border.width 1
                    , Border.color (Element.rgb 0 0 0)
                    , Element.mouseOver
                        [ Background.color (Element.rgb 0.8 0.8 0.8)
                        ]
                    ]
                    { onPress = Just LocalValueAddPressed
                    , label = Element.el [ Font.bold ] (Element.text "Add")
                    }
                ]
            ]
        ]


viewValues values =
    Element.column
        [ Element.spacing 8
        ]
        (List.map viewValue (Dict.toList values))


viewValue : ( String, Type ) -> Element msg
viewValue ( name, type_ ) =
    Element.el
        [ Font.family
            [ Font.monospace ]
        ]
        (Element.text <|
            String.concat
                [ name
                , " : "
                , Type.toString type_
                ]
        )


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
        [ Element.spacing 8
        ]
        (List.map viewExpr exprs)


viewExpr : Expr -> Element msg
viewExpr expr =
    Element.el
        [ Font.family
            [ Font.monospace ]
        ]
        (Element.text (Expr.toString expr))


type Msg
    = GotCoreModule (Result Http.Error (List Module))
      --
    | TargetTypeChanged String
    | NewNameChanged String
    | NewTypeChanged String
    | LocalValueAddPressed


update msg model =
    case model of
        Loading ->
            case msg of
                GotCoreModule result ->
                    case result of
                        Ok coreModules ->
                            ( Loaded
                                { coreModules = coreModules
                                , targetType = ""
                                , localValues = Dict.empty
                                , newName = ""
                                , newType = ""
                                }
                            , Cmd.none
                            )

                        Err error ->
                            ( Failed error
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        Loaded data ->
            Tuple.mapFirst Loaded <|
                updateLoaded msg data

        Failed _ ->
            ( model, Cmd.none )


updateLoaded msg data =
    case msg of
        GotCoreModule _ ->
            ( data, Cmd.none )

        TargetTypeChanged newTargetType ->
            ( { data | targetType = newTargetType }
            , Cmd.none
            )

        NewNameChanged newValue ->
            ( { data | newName = newValue }
            , Cmd.none
            )

        NewTypeChanged newValue ->
            ( { data | newType = newValue }
            , Cmd.none
            )

        LocalValueAddPressed ->
            case
                Decode.decodeString Elm.Type.decoder
                    ("\"" ++ data.newType ++ "\"")
            of
                Err _ ->
                    ( data
                    , Cmd.none
                    )

                Ok type_ ->
                    ( { data
                        | newName = ""
                        , newType = ""
                        , localValues = Dict.insert data.newName type_ data.localValues
                      }
                    , Cmd.none
                    )


subscriptions _ =
    Sub.none
