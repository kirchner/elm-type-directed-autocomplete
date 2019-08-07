module InferenceTest exposing (suite)

import Dict exposing (Dict)
import Elm.Docs exposing (Alias)
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.Range exposing (Range)
import Elm.Type exposing (Type(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Inference
import Module
import Test exposing (..)


suite : Test
suite =
    concat
        [ test "simple value" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : Int -> String
bar num =
    foo
"""
                    , range =
                        { start = { column = 5, row = 3 }
                        , end = { column = 8, row = 3 }
                        }
                    , values = Dict.empty
                    , aliases = []
                    }
                    |> Expect.equal
                        (Ok
                            ( Type "String" []
                            , Dict.fromList
                                [ ( "num", Type "Int" [] ) ]
                            )
                        )
        , test "tuple" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : Int -> ( String, Int )
bar num =
    ( foo, 123 )
"""
                    , range =
                        { start = { column = 7, row = 3 }
                        , end = { column = 10, row = 3 }
                        }
                    , values = Dict.empty
                    , aliases = []
                    }
                    |> Expect.equal
                        (Ok
                            ( Type "String" []
                            , Dict.fromList
                                [ ( "num", Type "Int" [] ) ]
                            )
                        )
        , test "record" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : Int -> { name : String, count : Int }
bar num =
    { name = foo
    , count = 1
    }
"""
                    , range =
                        { start = { column = 14, row = 3 }
                        , end = { column = 17, row = 3 }
                        }
                    , values = Dict.empty
                    , aliases = []
                    }
                    |> Expect.equal
                        (Ok
                            ( Type "String" []
                            , Dict.fromList
                                [ ( "num", Type "Int" [] ) ]
                            )
                        )
        , test "record update" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : { name : String, count : Int } -> { name : String, count : Int }
bar data =
    { data | name = foo }
"""
                    , range =
                        { start = { column = 21, row = 3 }
                        , end = { column = 24, row = 3 }
                        }
                    , values = Dict.empty
                    , aliases = []
                    }
                    |> Expect.equal
                        (Ok
                            ( Type "String" []
                            , Dict.fromList
                                [ ( "data"
                                  , Record
                                        [ ( "name", Type "String" [] )
                                        , ( "count", Type "Int" [] )
                                        ]
                                        Nothing
                                  )
                                ]
                            )
                        )
        , test "case" <|
            \_ ->
                inferHelp
                    { src =
                        """foo : Int -> Float -> ( Int, String )
foo int =
    \\float ->
        case int of
            0 ->
                ( 1, bar )

            _ ->
                ( 2, "foo" )
"""
                    , range =
                        { start = { column = 22, row = 6 }
                        , end = { column = 25, row = 6 }
                        }
                    , values = Dict.empty
                    , aliases = []
                    }
                    |> Expect.equal
                        (Ok
                            ( Type "String" []
                            , Dict.fromList
                                [ ( "int", Type "Int" [] )
                                , ( "float", Type "Float" [] )
                                ]
                            )
                        )
        , test "if" <|
            \_ ->
                inferHelp
                    { src =
                        """foo : Bool -> String
foo bool =
    if bool then
        bar
    else
        "bar"
"""
                    , range =
                        { start = { column = 9, row = 4 }
                        , end = { column = 12, row = 4 }
                        }
                    , values = Dict.empty
                    , aliases = []
                    }
                    |> Expect.equal
                        (Ok
                            ( Type "String" []
                            , Dict.fromList
                                [ ( "bool", Type "Bool" [] ) ]
                            )
                        )
        , test "with known value" <|
            \_ ->
                inferHelp
                    { src =
                        """foo : Int -> String
foo int =
    String.repeat int bar
"""
                    , range =
                        { start = { column = 23, row = 3 }
                        , end = { column = 26, row = 3 }
                        }
                    , values =
                        Dict.fromList
                            [ ( "String.repeat"
                              , Lambda (Type "Int" []) <|
                                    Lambda (Type "String" []) <|
                                        Type "String" []
                              )
                            ]
                    , aliases = []
                    }
                    |> Expect.equal
                        (Ok
                            ( Type "String" []
                            , Dict.fromList
                                [ ( "int", Type "Int" [] ) ]
                            )
                        )
        ]


inferHelp :
    { src : String
    , range : Range
    , values : Dict String Type
    , aliases : List Alias
    }
    -> Result Inference.Error ( Type, Dict String Type )
inferHelp { src, range, values, aliases } =
    let
        actualSrc =
            "module Main exposing (..)\n" ++ src
    in
    case Elm.Parser.parse actualSrc of
        Err error ->
            Debug.todo ("Could not parse src:\n" ++ actualSrc)

        Ok rawFile ->
            let
                file =
                    Elm.Processing.process Elm.Processing.init rawFile

                actualRange =
                    { start =
                        { column = range.start.column
                        , row = range.start.row + 1
                        }
                    , end =
                        { column = range.end.column
                        , row = range.end.row + 1
                        }
                    }
            in
            case Module.functionDeclarationAt actualRange file of
                Nothing ->
                    Debug.todo "No function declaration at the specified range"

                Just function ->
                    Inference.inferHole
                        { function = function
                        , range = actualRange
                        , values = values
                        , aliases = aliases
                        }
