module InferenceTest exposing (suite)

import Dict
import Elm.Type exposing (Type(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Inference
import Test exposing (..)


suite : Test
suite =
    concat
        [ test "simple value" <|
            \_ ->
                Inference.inferHole
                    { src =
                        """bar : Int -> String
bar num =
    foo
"""
                    , holeRange =
                        { start = { column = 5, row = 3 }
                        , end = { column = 8, row = 3 }
                        }
                    , values = Dict.empty
                    , typeAliases = []
                    }
                    |> Expect.equal
                        (Just
                            ( Type "String" []
                            , Dict.fromList
                                [ ( "num", Type "Int" [] ) ]
                            )
                        )
        , test "tuple" <|
            \_ ->
                Inference.inferHole
                    { src =
                        """bar : Int -> ( String, Int )
bar num =
    ( foo, 123 )
"""
                    , holeRange =
                        { start = { column = 7, row = 3 }
                        , end = { column = 10, row = 3 }
                        }
                    , values = Dict.empty
                    , typeAliases = []
                    }
                    |> Expect.equal
                        (Just
                            ( Type "String" []
                            , Dict.fromList
                                [ ( "num", Type "Int" [] ) ]
                            )
                        )
        , test "record" <|
            \_ ->
                Inference.inferHole
                    { src =
                        """bar : Int -> { name : String, count : Int }
bar num =
    { name = foo
    , count = 1
    }
"""
                    , holeRange =
                        { start = { column = 14, row = 3 }
                        , end = { column = 17, row = 3 }
                        }
                    , values = Dict.empty
                    , typeAliases = []
                    }
                    |> Expect.equal
                        (Just
                            ( Type "String" []
                            , Dict.fromList
                                [ ( "num", Type "Int" [] ) ]
                            )
                        )
        , test "record update" <|
            \_ ->
                Inference.inferHole
                    { src =
                        """bar : { name : String, count : Int } -> { name : String, count : Int }
bar data =
    { data | name = foo }
"""
                    , holeRange =
                        { start = { column = 21, row = 3 }
                        , end = { column = 24, row = 3 }
                        }
                    , values = Dict.empty
                    , typeAliases = []
                    }
                    |> Expect.equal
                        (Just
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
                Inference.inferHole
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
                    , holeRange =
                        { start = { column = 22, row = 6 }
                        , end = { column = 25, row = 6 }
                        }
                    , values = Dict.empty
                    , typeAliases = []
                    }
                    |> Expect.equal
                        (Just
                            ( Type "String" []
                            , Dict.fromList
                                [ ( "int", Type "Int" [] )
                                , ( "float", Type "Float" [] )
                                ]
                            )
                        )
        , test "if" <|
            \_ ->
                Inference.inferHole
                    { src =
                        """foo : Bool -> String
foo bool =
    if bool then
        bar
    else
        "bar"
"""
                    , holeRange =
                        { start = { column = 9, row = 4 }
                        , end = { column = 12, row = 4 }
                        }
                    , values = Dict.empty
                    , typeAliases = []
                    }
                    |> Expect.equal
                        (Just
                            ( Type "String" []
                            , Dict.fromList
                                [ ( "bool", Type "Bool" [] ) ]
                            )
                        )
        , test "with known value" <|
            \_ ->
                Inference.inferHole
                    { src =
                        """foo : Int -> String
foo int =
    String.repeat int bar
"""
                    , holeRange =
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
                    , typeAliases = []
                    }
                    |> Expect.equal
                        (Just
                            ( Type "String" []
                            , Dict.fromList
                                [ ( "int", Type "Int" [] ) ]
                            )
                        )
        ]
