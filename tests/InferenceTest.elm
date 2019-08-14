module InferenceTest exposing (suite)

import Canonical exposing (Associativity(..), Binop)
import Canonical.Annotation exposing (Annotation(..))
import Canonical.Type exposing (Type(..))
import Dict exposing (Dict)
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range, emptyRange)
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
                    , binops = Dict.empty
                    , values = Dict.empty
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.string
                            , Dict.fromList
                                [ ( "num", Canonical.Type.int ) ]
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
                    , binops = Dict.empty
                    , values = Dict.empty
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.string
                            , Dict.fromList
                                [ ( "num", Canonical.Type.int ) ]
                            )
                        )
        , test "infix operator" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : Int -> List Int
bar num =
    num :: foo
"""
                    , range =
                        { start = { column = 12, row = 3 }
                        , end = { column = 15, row = 3 }
                        }
                    , binops =
                        Dict.singleton "::"
                            { function = "cons"
                            , tipe =
                                ForAll [ "a" ] <|
                                    Lambda (Var "a")
                                        (Lambda
                                            (Canonical.Type.list (Var "a"))
                                            (Canonical.Type.list (Var "a"))
                                        )
                            , precedence = 5
                            , associativity = Right
                            }
                    , values = Dict.empty
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.list Canonical.Type.int
                            , Dict.fromList
                                [ ( "num", Canonical.Type.int ) ]
                            )
                        )
        , test "two infix operators" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : Int -> Int -> Int
bar numA numB =
    numA + numB * foo
"""
                    , range =
                        { start = { column = 19, row = 3 }
                        , end = { column = 22, row = 3 }
                        }
                    , binops =
                        Dict.fromList
                            [ ( "+"
                              , { function = "add"
                                , tipe =
                                    ForAll [] <|
                                        Lambda Canonical.Type.int
                                            (Lambda
                                                Canonical.Type.int
                                                Canonical.Type.int
                                            )
                                , precedence = 6
                                , associativity = Left
                                }
                              )
                            , ( "*"
                              , { function = "mul"
                                , tipe =
                                    ForAll [] <|
                                        Lambda Canonical.Type.int
                                            (Lambda
                                                Canonical.Type.int
                                                Canonical.Type.int
                                            )
                                , precedence = 7
                                , associativity = Left
                                }
                              )
                            ]
                    , values = Dict.empty
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.int
                            , Dict.fromList
                                [ ( "numA", Canonical.Type.int )
                                , ( "numB", Canonical.Type.int )
                                ]
                            )
                        )
        , test "function" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : Int -> String
bar int =
    foo int
"""
                    , range =
                        { start = { column = 5, row = 3 }
                        , end = { column = 8, row = 3 }
                        }
                    , binops = Dict.empty
                    , values = Dict.empty
                    }
                    |> Expect.equal
                        (Ok
                            ( Lambda
                                Canonical.Type.int
                                Canonical.Type.string
                            , Dict.fromList
                                [ ( "int", Canonical.Type.int )
                                ]
                            )
                        )
        , test "pipe operator" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : Int -> String
bar int =
    int
        |> foo
"""
                    , range =
                        { start = { column = 12, row = 4 }
                        , end = { column = 15, row = 4 }
                        }
                    , binops =
                        Dict.singleton "|>"
                            { function = "apR"
                            , tipe =
                                ForAll [ "a", "b" ] <|
                                    Lambda
                                        (Var "a")
                                        (Lambda
                                            (Lambda (Var "a") (Var "b"))
                                            (Var "b")
                                        )
                            , precedence = 0
                            , associativity = Left
                            }
                    , values = Dict.empty
                    }
                    |> Expect.equal
                        (Ok
                            ( Lambda
                                Canonical.Type.int
                                Canonical.Type.string
                            , Dict.fromList
                                [ ( "int", Canonical.Type.int )
                                ]
                            )
                        )
        , test "two pipe operators" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : Int -> String
bar int =
    int
        |> toFloat
        |> foo
"""
                    , range =
                        { start = { column = 12, row = 5 }
                        , end = { column = 15, row = 5 }
                        }
                    , binops =
                        Dict.singleton "|>"
                            { function = "apR"
                            , tipe =
                                ForAll [ "a", "b" ] <|
                                    Lambda
                                        (Var "a")
                                        (Lambda
                                            (Lambda (Var "a") (Var "b"))
                                            (Var "b")
                                        )
                            , precedence = 0
                            , associativity = Left
                            }
                    , values =
                        Dict.fromList
                            [ ( "toFloat"
                              , ForAll [] (Lambda Canonical.Type.int Canonical.Type.float)
                              )
                            ]
                    }
                    |> Expect.equal
                        (Ok
                            ( Lambda
                                Canonical.Type.float
                                Canonical.Type.string
                            , Dict.fromList
                                [ ( "int", Canonical.Type.int )
                                ]
                            )
                        )
        , test "using one value twice" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : List Int -> List String
bar ints =
    ints
        |> List.map toFloat
        |> List.map identity
        |> foo
"""
                    , range =
                        { start = { column = 12, row = 6 }
                        , end = { column = 15, row = 6 }
                        }
                    , binops =
                        Dict.singleton "|>"
                            { function = "apR"
                            , tipe =
                                ForAll [ "a", "b" ] <|
                                    Lambda
                                        (Var "a")
                                        (Lambda
                                            (Lambda (Var "a") (Var "b"))
                                            (Var "b")
                                        )
                            , precedence = 0
                            , associativity = Left
                            }
                    , values =
                        Dict.fromList
                            [ ( "toFloat"
                              , ForAll [] (Lambda Canonical.Type.int Canonical.Type.float)
                              )
                            , ( "identity"
                              , ForAll [ "a" ] (Lambda (Var "a") (Var "a"))
                              )
                            , ( "List.map"
                              , ForAll [ "a", "b" ]
                                    (Lambda (Lambda (Var "a") (Var "b"))
                                        (Lambda
                                            (Canonical.Type.list (Var "a"))
                                            (Canonical.Type.list (Var "b"))
                                        )
                                    )
                              )
                            ]
                    }
                    |> Expect.equal
                        (Ok
                            ( Lambda
                                (Canonical.Type.list Canonical.Type.float)
                                (Canonical.Type.list Canonical.Type.string)
                            , Dict.fromList
                                [ ( "ints", Canonical.Type.list Canonical.Type.int )
                                ]
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
                    , binops = Dict.empty
                    , values = Dict.empty
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.string
                            , Dict.fromList
                                [ ( "num", Canonical.Type.int ) ]
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
                    , binops = Dict.empty
                    , values = Dict.empty
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.string
                            , Dict.fromList
                                [ ( "data"
                                  , Record
                                        [ ( "name", Canonical.Type.string )
                                        , ( "count", Canonical.Type.int )
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
                    , binops = Dict.empty
                    , values = Dict.empty
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.string
                            , Dict.fromList
                                [ ( "int", Canonical.Type.int )
                                , ( "float", Canonical.Type.float )
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
                    , binops = Dict.empty
                    , values = Dict.empty
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.string
                            , Dict.fromList
                                [ ( "bool", Canonical.Type.bool ) ]
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
                    , binops = Dict.empty
                    , values =
                        Dict.fromList
                            [ ( "String.repeat"
                              , ForAll []
                                    (Lambda Canonical.Type.int <|
                                        Lambda
                                            Canonical.Type.string
                                            Canonical.Type.string
                                    )
                              )
                            ]
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.string
                            , Dict.fromList
                                [ ( "int", Canonical.Type.int ) ]
                            )
                        )

        --        , test "record accessor" <|
        --            \_ ->
        --                inferHelp
        --                    { src =
        --                        """update : Msg -> Model -> Model
        --update msg model =
        --    case msg of
        --        NoOp ->
        --            { model | count = model.count }
        --
        --        NameChanged ->
        --            foo
        --"""
        --                    , range =
        --                        { start = { column = 13, row = 8 }
        --                        , end = { column = 16, row = 8 }
        --                        }
        --                    , binops = Dict.empty
        --                    , values =
        --                        Dict.fromList
        --                            [ ( "msg", Type "Msg" [] )
        --                            , ( "model", Type "Model" [] )
        --                            , ( "NoOp", Type "Msg" [] )
        --                            , ( "NameChanged", Type "Msg" [] )
        --                            ]
        --                    , aliases =
        --                        [ { name = "Model"
        --                          , comment = ""
        --                          , args = []
        --                          , tipe =
        --                                Record
        --                                    [ ( "count", Canonical.Type.int )
        --                                    , ( "name", Canonical.Type.string )
        --                                    ]
        --                                    Nothing
        --                          }
        --                        ]
        --                    }
        --                    |> Expect.equal
        --                        (Ok
        --                            ( Record
        --                                [ ( "count", Canonical.Type.int )
        --                                , ( "name", Canonical.Type.string )
        --                                ]
        --                                Nothing
        --                            , Dict.empty
        --                            )
        --                        )
        , test "empty list pattern" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : List Int -> String
bar nums =
    case nums of
        [] ->
            foo
"""
                    , range =
                        { start = { column = 13, row = 5 }
                        , end = { column = 16, row = 5 }
                        }
                    , binops = Dict.empty
                    , values = Dict.empty
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.string
                            , Dict.fromList
                                [ ( "nums", Canonical.Type.list Canonical.Type.int )
                                ]
                            )
                        )
        , test "uncons pattern" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : List Int -> String
bar nums =
    case nums of
        first :: rest ->
            foo
"""
                    , range =
                        { start = { column = 13, row = 5 }
                        , end = { column = 16, row = 5 }
                        }
                    , binops = Dict.empty
                    , values = Dict.empty
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.string
                            , Dict.fromList
                                [ ( "first", Canonical.Type.int )
                                , ( "nums", Canonical.Type.list Canonical.Type.int )
                                , ( "rest", Canonical.Type.list Canonical.Type.int )
                                ]
                            )
                        )
        , test "record pattern" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : { count : Int } -> String
bar { count } =
    foo count
"""
                    , range =
                        { start = { column = 5, row = 3 }
                        , end = { column = 8, row = 3 }
                        }
                    , binops = Dict.empty
                    , values = Dict.empty
                    }
                    |> Expect.equal
                        (Ok
                            ( Lambda Canonical.Type.int Canonical.Type.string
                            , Dict.fromList
                                [ ( "count", Canonical.Type.int )
                                ]
                            )
                        )
        , test "as pattern" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : { count : Int } -> String
bar ({ count } as stuff) =
    foo stuff count
"""
                    , range =
                        { start = { column = 5, row = 3 }
                        , end = { column = 8, row = 3 }
                        }
                    , binops = Dict.empty
                    , values = Dict.empty
                    }
                    |> Expect.equal
                        (Ok
                            ( Lambda
                                (Record
                                    [ ( "count", Canonical.Type.int ) ]
                                    Nothing
                                )
                                (Lambda
                                    Canonical.Type.int
                                    Canonical.Type.string
                                )
                            , Dict.fromList
                                [ ( "count", Canonical.Type.int )
                                , ( "stuff"
                                  , Record
                                        [ ( "count", Canonical.Type.int ) ]
                                        Nothing
                                  )
                                ]
                            )
                        )
        ]


inferHelp :
    { src : String
    , range : Range
    , binops : Dict String Binop
    , values : Dict String Annotation
    }
    -> Result Inference.Error ( Type, Dict String Type )
inferHelp { src, range, binops, values } =
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
                        , moduleName = "Main"
                        , imports =
                            Dict.fromList
                                [ ( ""
                                  , Dict.fromList
                                        [ ( "Int", "Basics" )
                                        , ( "Float", "Basics" )
                                        , ( "String", "String" )
                                        , ( "List", "List" )
                                        , ( "Bool", "Basics" )
                                        ]
                                  )
                                ]
                        , binops = binops
                        , values = values
                        }
