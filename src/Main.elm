port module Main exposing (main)

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
import Json.Decode as Decode exposing (Decoder)
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
    , localValues : Dict String Type
    , newName : String
    , newType : String
    }


type alias Flags =
    { localValues : String
    , coreJson : Value
    }


init flags =
    case
        ( Decode.decodeString localValuesDecoder flags.localValues
        , Decode.decodeValue (Decode.list Elm.Docs.decoder) flags.coreJson
        )
    of
        ( Ok localValues, Ok coreModules ) ->
            ( { coreModules = coreModules
              , targetType = ""
              , localValues = localValues
              , newName = ""
              , newType = ""
              }
            , Cmd.none
            )

        _ ->
            ( { coreModules = []
              , targetType = ""
              , localValues = Dict.empty
              , newName = ""
              , newType = ""
              }
            , Cmd.none
            )


view model =
    Element.layout [] <|
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
                (case decodeTargetType model.targetType of
                    Just targetType ->
                        let
                            knownValues =
                                model.coreModules
                                    |> List.map valuesFromModule
                                    |> List.foldl Dict.union Dict.empty
                        in
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
                        , Element.column
                            [ Element.width Element.fill
                            , Element.height Element.fill
                            , Element.spacing 16
                            ]
                            [ Element.el [ Font.bold ]
                                (Element.text "Suggestions")
                            , viewExprs <|
                                Expr.suggest (Dict.union model.localValues knownValues)
                                    targetType
                            ]
                        ]

                    Nothing ->
                        [ Input.text
                            [ Element.spacing 8
                            , Element.width Element.fill
                            ]
                            { onChange = TargetTypeChanged
                            , text = model.targetType
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
                    , viewValues model.localValues
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
                        , text = model.newName
                        , placeholder = Nothing
                        , label = Input.labelAbove [ Font.bold ] (Element.text "Name")
                        }
                    , Input.text
                        [ Element.spacing 8
                        , Element.width Element.fill
                        ]
                        { onChange = NewTypeChanged
                        , text = model.newType
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
        , Element.width Element.fill
        ]
        (List.map viewValue (Dict.toList values))


viewValue : ( String, Type ) -> Element Msg
viewValue ( name, type_ ) =
    Element.row
        [ Font.family
            [ Font.monospace ]
        , Element.width Element.fill
        ]
        [ Element.text <|
            String.concat
                [ name
                , " : "
                , Type.toString type_
                ]
        , Element.el
            [ Element.alignRight ]
            (Input.button
                [ Font.color (Element.rgb 0.4 0.4 0.4)
                , Font.underline
                , Element.mouseOver
                    [ Font.color (Element.rgb 0 0 0)
                    ]
                ]
                { onPress = Just (LocalValueRemovePressed name)
                , label = Element.text "Remove"
                }
            )
        ]


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
    = NoOp
    | GotCoreModule (Result Http.Error (List Module))
      --
    | TargetTypeChanged String
    | NewNameChanged String
    | NewTypeChanged String
    | LocalValueAddPressed
    | LocalValueRemovePressed String


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

        NewNameChanged newValue ->
            ( { model | newName = newValue }
            , Cmd.none
            )

        NewTypeChanged newValue ->
            ( { model | newType = newValue }
            , Cmd.none
            )

        LocalValueAddPressed ->
            case
                Decode.decodeString Elm.Type.decoder
                    ("\"" ++ model.newType ++ "\"")
            of
                Err _ ->
                    ( model
                    , Cmd.none
                    )

                Ok type_ ->
                    let
                        newLocalValues =
                            Dict.insert model.newName type_ model.localValues
                    in
                    ( { model
                        | newName = ""
                        , newType = ""
                        , localValues = newLocalValues
                      }
                    , cache (encodeLocalValues newLocalValues)
                    )

        LocalValueRemovePressed name ->
            let
                newLocalValues =
                    Dict.remove name model.localValues
            in
            ( { model | localValues = newLocalValues }
            , cache (encodeLocalValues newLocalValues)
            )


subscriptions _ =
    Sub.none



---- DECODER


localValuesDecoder : Decoder (Dict String Type)
localValuesDecoder =
    Decode.dict typeDecoder


typeDecoder : Decoder Type
typeDecoder =
    Decode.field "kind" Decode.string
        |> Decode.andThen
            (\kind ->
                case kind of
                    "var" ->
                        Decode.map Var
                            (Decode.field "name" Decode.string)

                    "type" ->
                        Decode.map2 Type
                            (Decode.field "name" Decode.string)
                            (Decode.field "subTypes" (Decode.list typeDecoder))

                    "lambda" ->
                        Decode.map2 Lambda
                            (Decode.field "from" typeDecoder)
                            (Decode.field "to" typeDecoder)

                    "tuple" ->
                        Decode.map Tuple
                            (Decode.field "subTypes" (Decode.list typeDecoder))

                    "record" ->
                        Decode.fail "TODO"

                    _ ->
                        Decode.fail ("unsupported kind: " ++ kind)
            )



---- ENCODE


encodeLocalValues : Dict String Type -> Value
encodeLocalValues =
    Encode.dict identity encodeType


encodeType : Type -> Value
encodeType type_ =
    case type_ of
        Var name ->
            Encode.object
                [ ( "kind", Encode.string "var" )
                , ( "name", Encode.string name )
                ]

        Type name subTypes ->
            Encode.object
                [ ( "kind", Encode.string "type" )
                , ( "name", Encode.string name )
                , ( "subTypes", Encode.list encodeType subTypes )
                ]

        Lambda from to ->
            Encode.object
                [ ( "kind", Encode.string "lambda" )
                , ( "from", encodeType from )
                , ( "to", encodeType to )
                ]

        Tuple subTypes ->
            Encode.object
                [ ( "kind", Encode.string "tuple" )
                , ( "subTypes", Encode.list encodeType subTypes )
                ]

        Record _ _ ->
            Encode.object
                [ ( "kind", Encode.string "record" )
                ]
