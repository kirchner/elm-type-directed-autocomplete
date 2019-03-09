port module Main exposing (main)

import Browser
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

    -- LOCALE VALUES
    , localValues : Dict String Type
    , newName : String
    , newType : String

    -- LOCALE ALIASES
    , localAliases : Dict String Alias
    , newAliasName : String
    , newAliasType : String
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

              -- LOCAL VALUES
              , localValues = storage.localValues
              , newName = ""
              , newType = ""

              -- LOCAL ALIASES
              , localAliases = storage.localAliases
              , newAliasName = ""
              , newAliasType = ""
              }
            , Cmd.none
            )

        _ ->
            ( { coreModules = []
              , targetType = ""

              -- LOCAL VALUES
              , localValues = Dict.empty
              , newName = ""
              , newType = ""

              -- LOCAL ALIASES
              , localAliases = Dict.empty
              , newAliasName = ""
              , newAliasType = ""
              }
            , Cmd.none
            )


view : Model -> Html Msg
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
                                Expr.suggest
                                    (Dict.values model.localAliases)
                                    (Dict.union model.localValues knownValues)
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
                , Element.spacing 32
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
                            , label = Element.el [ Font.bold ] (Element.text "Add locale value")
                            }
                        ]
                    ]
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
                            (Element.text "Local aliases")
                        , viewAliases model.localAliases
                        ]
                    , Element.column
                        [ Element.width Element.fill
                        , Element.spacing 16
                        ]
                        [ Input.text
                            [ Element.spacing 8
                            , Element.width Element.fill
                            ]
                            { onChange = NewAliasNameChanged
                            , text = model.newAliasName
                            , placeholder = Nothing
                            , label = Input.labelAbove [ Font.bold ] (Element.text "Name")
                            }
                        , Input.text
                            [ Element.spacing 8
                            , Element.width Element.fill
                            ]
                            { onChange = NewAliasTypeChanged
                            , text = model.newAliasType
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
                            { onPress = Just LocalAliasAddPressed
                            , label = Element.el [ Font.bold ] (Element.text "Add locale alias")
                            }
                        ]
                    ]
                ]
            ]


viewValues : Dict String Type -> Element Msg
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


viewAliases : Dict String Alias -> Element Msg
viewAliases aliases =
    Element.column
        [ Element.spacing 8
        , Element.width Element.fill
        ]
        (List.map viewAlias (Dict.toList aliases))


viewAlias : ( String, Alias ) -> Element Msg
viewAlias ( name, alias_ ) =
    Element.row
        [ Font.family
            [ Font.monospace ]
        , Element.width Element.fill
        ]
        [ Element.text <|
            String.join " "
                [ "type alias"
                , if List.isEmpty alias_.args then
                    alias_.name

                  else
                    String.join " " (alias_.name :: alias_.args)
                , ":"
                , Type.toString alias_.tipe
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
                { onPress = Just (LocalAliasRemovePressed name)
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
        [ Element.spacing 16
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
    | NewNameChanged String
    | NewTypeChanged String
    | LocalValueAddPressed
    | LocalValueRemovePressed String
    | NewAliasNameChanged String
    | NewAliasTypeChanged String
    | LocalAliasAddPressed
    | LocalAliasRemovePressed String


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
                    , cache
                        (encodeLocalStorage
                            { localValues = newLocalValues
                            , localAliases = model.localAliases
                            }
                        )
                    )

        LocalValueRemovePressed name ->
            let
                newLocalValues =
                    Dict.remove name model.localValues
            in
            ( { model | localValues = newLocalValues }
            , cache
                (encodeLocalStorage
                    { localValues = newLocalValues
                    , localAliases = model.localAliases
                    }
                )
            )

        NewAliasNameChanged newName ->
            ( { model | newAliasName = newName }
            , Cmd.none
            )

        NewAliasTypeChanged newType ->
            ( { model | newAliasType = newType }
            , Cmd.none
            )

        LocalAliasAddPressed ->
            case
                Decode.decodeString Elm.Type.decoder
                    ("\"" ++ model.newAliasType ++ "\"")
            of
                Err _ ->
                    ( model, Cmd.none )

                Ok tipe ->
                    case String.words model.newAliasName of
                        name :: args ->
                            let
                                newLocalAliases =
                                    Dict.insert name
                                        { name = name
                                        , comment = ""
                                        , tipe = tipe
                                        , args = args
                                        }
                                        model.localAliases
                            in
                            ( { model
                                | newAliasName = ""
                                , newAliasType = ""
                                , localAliases = newLocalAliases
                              }
                            , cache
                                (encodeLocalStorage
                                    { localValues = model.localValues
                                    , localAliases = newLocalAliases
                                    }
                                )
                            )

                        [] ->
                            ( model, Cmd.none )

        LocalAliasRemovePressed name ->
            let
                newLocalAliases =
                    Dict.remove name model.localAliases
            in
            ( { model | localAliases = newLocalAliases }
            , cache
                (encodeLocalStorage
                    { localValues = model.localValues
                    , localAliases = newLocalAliases
                    }
                )
            )


subscriptions _ =
    Sub.none



---- DECODER


type alias Storage =
    { localValues : Dict String Type
    , localAliases : Dict String Alias
    }


localStorageDecoder : Decoder Storage
localStorageDecoder =
    Decode.succeed Storage
        |> Decode.required "localValues" localValuesDecoder
        |> Decode.required "localAliases" localAliasesDecoder


localValuesDecoder : Decoder (Dict String Type)
localValuesDecoder =
    Decode.dict typeDecoder


localAliasesDecoder : Decoder (Dict String Alias)
localAliasesDecoder =
    Decode.dict aliasDecoder


aliasDecoder : Decoder Alias
aliasDecoder =
    Decode.succeed Alias
        |> Decode.required "name" Decode.string
        |> Decode.required "comment" Decode.string
        |> Decode.required "args" (Decode.list Decode.string)
        |> Decode.required "tipe" typeDecoder


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
                        Decode.map2 Record
                            (Decode.field "values" (Decode.list valueDecoder))
                            (Decode.field "var" (Decode.maybe Decode.string))

                    _ ->
                        Decode.fail ("unsupported kind: " ++ kind)
            )


valueDecoder : Decoder ( String, Type )
valueDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "name" Decode.string)
        (Decode.field "type" typeDecoder)



---- ENCODE


encodeLocalStorage : Storage -> Value
encodeLocalStorage storage =
    Encode.object
        [ ( "localValues", encodeLocalValues storage.localValues )
        , ( "localAliases", encodeLocalAliases storage.localAliases )
        ]


encodeLocalValues : Dict String Type -> Value
encodeLocalValues =
    Encode.dict identity encodeType


encodeLocalAliases : Dict String Alias -> Value
encodeLocalAliases =
    Encode.dict identity encodeAlias


encodeAlias : Alias -> Value
encodeAlias alias_ =
    Encode.object
        [ ( "name", Encode.string alias_.name )
        , ( "comment", Encode.string alias_.comment )
        , ( "args", Encode.list Encode.string alias_.args )
        , ( "tipe", encodeType alias_.tipe )
        ]


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

        Record values maybeVar ->
            Encode.object
                [ ( "kind", Encode.string "record" )
                , ( "values", Encode.list encodeValue values )
                , ( "var"
                  , Maybe.withDefault Encode.null (Maybe.map Encode.string maybeVar)
                  )
                ]


encodeValue : ( String, Type ) -> Value
encodeValue ( name, type_ ) =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "type", encodeType type_ )
        ]
