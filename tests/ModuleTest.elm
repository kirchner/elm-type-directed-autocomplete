module ModuleTest exposing (suite)

import Dict
import Elm.Interface
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Node
import Elm.Type exposing (Type(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Module
import Test exposing (..)


suite : Test
suite =
    let
        ( interface, file ) =
            case Elm.Parser.parse src of
                Err _ ->
                    Debug.todo "'src' is not a valid Elm module"

                Ok rawFile ->
                    ( Elm.Interface.build rawFile
                    , Elm.Processing.process Elm.Processing.init rawFile
                    )
    in
    concat
        [ test "exposed" <|
            \_ ->
                Module.exposed file interface
                    |> Expect.equal
                        (Just
                            { values =
                                Dict.fromList
                                    [ ( "update"
                                      , Lambda (Type "Msg" [])
                                            (Lambda (Type "Model" [])
                                                (Type "Model" [])
                                            )
                                      )
                                    ]
                            , aliases =
                                [ { name = "Model"
                                  , comment = ""
                                  , args = []
                                  , tipe =
                                        Record
                                            [ ( "name", Type "String" [] ) ]
                                            Nothing
                                  }
                                ]
                            , unions =
                                [ { name = "Msg"
                                  , comment = ""
                                  , args = []
                                  , tags = []
                                  }
                                ]
                            }
                        )
        , test "internal" <|
            \_ ->
                Module.internal file
                    |> Expect.equal
                        (Just
                            { values =
                                Dict.fromList
                                    [ ( "update"
                                      , Lambda (Type "Msg" [])
                                            (Lambda (Type "Model" [])
                                                (Type "Model" [])
                                            )
                                      )
                                    , ( "internal"
                                      , Lambda (Type "String" []) (Type "String" [])
                                      )
                                    ]
                            , aliases =
                                [ { name = "Model"
                                  , comment = ""
                                  , args = []
                                  , tipe =
                                        Record
                                            [ ( "name", Type "String" [] ) ]
                                            Nothing
                                  }
                                ]
                            , unions =
                                [ { name = "Internal"
                                  , comment = ""
                                  , args = []
                                  , tags = [ ( "Internal", [] ) ]
                                  }
                                , { name = "Msg"
                                  , comment = ""
                                  , args = []
                                  , tags =
                                        [ ( "NoOp", [] )
                                        , ( "NameChanged", [ Type "String" [] ] )
                                        ]
                                  }
                                ]
                            }
                        )
        , test "functionDeclarationAt" <|
            \_ ->
                Module.functionDeclarationAt
                    { start =
                        { row = 13
                        , column = 4
                        }
                    , end =
                        { row = 13
                        , column = 8
                        }
                    }
                    file
                    |> Maybe.map
                        (.declaration
                            >> Elm.Syntax.Node.value
                            >> .name
                            >> Elm.Syntax.Node.value
                        )
                    |> Expect.equal (Just "update")
        ]


src =
    """module Main exposing (Msg, Model, update)

type Msg
    = NoOp
    | NameChanged String

type alias Model =
    { name : String }


update : Msg -> Model -> Model
update msg model =
    model


internal : String -> String
internal text =
    text


type Internal
    = Internal
"""
