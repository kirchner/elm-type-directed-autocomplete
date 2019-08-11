module InferenceTest exposing (suite)

import Dict exposing (Dict)
import Elm.Docs exposing (Alias)
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.Infix exposing (Infix, InfixDirection(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range, emptyRange)
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
                    , binops = Dict.empty
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
                    , binops = Dict.empty
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
                            ( { direction = Node emptyRange Right
                              , precedence = Node emptyRange 5
                              , operator = Node emptyRange "::"
                              , function = Node emptyRange "cons"
                              }
                            , Lambda (Var "a")
                                (Lambda
                                    (Type "List" [ Var "a" ])
                                    (Type "List" [ Var "a" ])
                                )
                            )
                    , values = Dict.empty
                    , aliases = []
                    }
                    |> Expect.equal
                        (Ok
                            ( Type "List" [ Type "Int" [] ]
                            , Dict.fromList
                                [ ( "num", Type "Int" [] ) ]
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
                              , ( { direction = Node emptyRange Left
                                  , precedence = Node emptyRange 6
                                  , operator = Node emptyRange "+"
                                  , function = Node emptyRange "add"
                                  }
                                , Lambda (Type "Int" [])
                                    (Lambda
                                        (Type "Int" [])
                                        (Type "Int" [])
                                    )
                                )
                              )
                            , ( "*"
                              , ( { direction = Node emptyRange Left
                                  , precedence = Node emptyRange 7
                                  , operator = Node emptyRange "*"
                                  , function = Node emptyRange "mul"
                                  }
                                , Lambda (Type "Int" [])
                                    (Lambda
                                        (Type "Int" [])
                                        (Type "Int" [])
                                    )
                                )
                              )
                            ]
                    , values = Dict.empty
                    , aliases = []
                    }
                    |> Expect.equal
                        (Ok
                            ( Type "Int" []
                            , Dict.fromList
                                [ ( "numA", Type "Int" [] )
                                , ( "numB", Type "Int" [] )
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
                    , aliases = []
                    }
                    |> Expect.equal
                        (Ok
                            ( Lambda
                                (Type "Int" [])
                                (Type "String" [])
                            , Dict.fromList
                                [ ( "int", Type "Int" [] )
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
                        Dict.fromList
                            [ ( "|>"
                              , ( { direction = Node emptyRange Left
                                  , precedence = Node emptyRange 0
                                  , operator = Node emptyRange "|>"
                                  , function = Node emptyRange "apR"
                                  }
                                , Lambda
                                    (Var "a")
                                    (Lambda
                                        (Lambda (Var "a") (Var "b"))
                                        (Var "b")
                                    )
                                )
                              )
                            ]
                    , values = Dict.empty
                    , aliases = []
                    }
                    |> Expect.equal
                        (Ok
                            ( Lambda
                                (Type "Int" [])
                                (Type "String" [])
                            , Dict.fromList
                                [ ( "int", Type "Int" [] )
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
                        Dict.fromList
                            [ ( "|>"
                              , ( { direction = Node emptyRange Left
                                  , precedence = Node emptyRange 0
                                  , operator = Node emptyRange "|>"
                                  , function = Node emptyRange "apR"
                                  }
                                , Lambda
                                    (Var "a")
                                    (Lambda
                                        (Lambda (Var "a") (Var "b"))
                                        (Var "b")
                                    )
                                )
                              )
                            ]
                    , values =
                        Dict.fromList
                            [ ( "toFloat"
                              , Lambda (Type "Int" []) (Type "Float" [])
                              )
                            ]
                    , aliases = []
                    }
                    |> Expect.equal
                        (Ok
                            ( Lambda
                                (Type "Float" [])
                                (Type "String" [])
                            , Dict.fromList
                                [ ( "int", Type "Int" [] )
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
                        Dict.fromList
                            [ ( "|>"
                              , ( { direction = Node emptyRange Left
                                  , precedence = Node emptyRange 0
                                  , operator = Node emptyRange "|>"
                                  , function = Node emptyRange "apR"
                                  }
                                , Lambda
                                    (Var "a")
                                    (Lambda
                                        (Lambda (Var "a") (Var "b"))
                                        (Var "b")
                                    )
                                )
                              )
                            ]
                    , values =
                        Dict.fromList
                            [ ( "toFloat"
                              , Lambda (Type "Int" []) (Type "Float" [])
                              )
                            , ( "identity"
                              , Lambda (Var "a") (Var "a")
                              )
                            , ( "List.map"
                              , Lambda (Lambda (Var "a") (Var "b"))
                                    (Lambda
                                        (Type "List" [ Var "a" ])
                                        (Type "List" [ Var "b" ])
                                    )
                              )
                            ]
                    , aliases = []
                    }
                    |> Expect.equal
                        (Ok
                            ( Lambda
                                (Type "List" [ Type "Float" [] ])
                                (Type "List" [ Type "String" [] ])
                            , Dict.fromList
                                [ ( "ints", Type "List" [ Type "Int" [] ] )
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
                    , binops = Dict.empty
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
                    , binops = Dict.empty
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
                    , binops = Dict.empty
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
                    , binops = Dict.empty
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
        , test "record accessor" <|
            \_ ->
                inferHelp
                    { src =
                        """update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            { model | count = model.count }

        NameChanged ->
            foo
"""
                    , range =
                        { start = { column = 13, row = 8 }
                        , end = { column = 16, row = 8 }
                        }
                    , binops = Dict.empty
                    , values =
                        Dict.fromList
                            [ ( "msg", Type "Msg" [] )
                            , ( "model", Type "Model" [] )
                            , ( "NoOp", Type "Msg" [] )
                            , ( "NameChanged", Type "Msg" [] )
                            ]
                    , aliases =
                        [ { name = "Model"
                          , comment = ""
                          , args = []
                          , tipe =
                                Record
                                    [ ( "count", Type "Int" [] )
                                    , ( "name", Type "String" [] )
                                    ]
                                    Nothing
                          }
                        ]
                    }
                    |> Expect.equal
                        (Ok
                            ( Record
                                [ ( "count", Type "Int" [] )
                                , ( "name", Type "String" [] )
                                ]
                                Nothing
                            , Dict.empty
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
                    , aliases = []
                    }
                    |> Expect.equal
                        (Ok
                            ( Type "String" []
                            , Dict.fromList
                                [ ( "first", Type "Int" [] )
                                , ( "nums", Type "List" [ Type "Int" [] ] )
                                , ( "rest", Type "List" [ Type "Int" [] ] )
                                ]
                            )
                        )
        ]


inferHelp :
    { src : String
    , range : Range
    , binops : Dict String ( Infix, Type )
    , values : Dict String Type
    , aliases : List Alias
    }
    -> Result Inference.Error ( Type, Dict String Type )
inferHelp { src, range, binops, values, aliases } =
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
                        , binops = binops
                        , values = values
                        , aliases = aliases
                        }
