module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Elm.Docs exposing (Module)
import Elm.Type exposing (Type(..))
import Html exposing (Html)
import Http
import Json.Decode as Decode


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
    Element.column
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
                    , Element.width (Element.px 640)
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
                    , viewExprs (suggest knownValues targetType)
                    ]
                ]

            Nothing ->
                [ Input.text
                    [ Element.spacing 8
                    , Element.width (Element.px 640)
                    ]
                    { onChange = TargetTypeChanged
                    , text = data.targetType
                    , placeholder = Nothing
                    , label = Input.labelAbove [ Font.bold ] (Element.text "Target type")
                    }
                , Element.text "This is not a valid type"
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
    case expr of
        Call name args ->
            Element.el
                [ Font.family
                    [ Font.monospace ]
                ]
                (Element.text (String.join " " (name :: args)))


type Msg
    = GotCoreModule (Result Http.Error (List Module))
      --
    | TargetTypeChanged String


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
        TargetTypeChanged newTargetType ->
            ( { data | targetType = newTargetType }
            , Cmd.none
            )

        _ ->
            ( data, Cmd.none )


subscriptions _ =
    Sub.none



---- SUGGEST


type Expr
    = Call String (List String)


suggest : Dict String Type -> Type -> List Expr
suggest knownValues targetType =
    let
        toExpr name =
            Call name []
    in
    knownValues
        |> Dict.filter
            (\name knownType ->
                removeScope knownType == removeScope targetType
            )
        |> Dict.keys
        |> List.map toExpr


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
                subTypes

        Lambda typeA typeB ->
            Lambda (removeScope typeA) (removeScope typeB)

        Tuple types ->
            Tuple (List.map removeScope types)

        Record values var ->
            Record
                (List.map (Tuple.mapSecond removeScope) values)
                var
