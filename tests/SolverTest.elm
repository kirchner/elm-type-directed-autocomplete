module SolverTest exposing (suite)

import Canonical.Type exposing (Type(..))
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Solver
import Test exposing (..)


suite : Test
suite =
    concat
        [ test "a  ~  Int" <|
            \_ ->
                Solver.run
                    [ ( Var "a"
                      , Canonical.Type.int
                      )
                    ]
                    |> Expect.equal
                        (Ok (Dict.singleton "a" Canonical.Type.int))
        , test "List a  ~  List Int" <|
            \_ ->
                Solver.run
                    [ ( Canonical.Type.list (Var "a")
                      , Canonical.Type.list Canonical.Type.int
                      )
                    ]
                    |> Expect.equal
                        (Ok (Dict.singleton "a" Canonical.Type.int))
        , test "a -> String  ~  Int -> String" <|
            \_ ->
                Solver.run
                    [ ( Lambda (Var "a") Canonical.Type.string
                      , Lambda Canonical.Type.int Canonical.Type.string
                      )
                    ]
                    |> Expect.equal
                        (Ok (Dict.singleton "a" Canonical.Type.int))
        , test "String -> a  ~  String -> Int" <|
            \_ ->
                Solver.run
                    [ ( Lambda Canonical.Type.string (Var "a")
                      , Lambda Canonical.Type.string Canonical.Type.int
                      )
                    ]
                    |> Expect.equal
                        (Ok (Dict.singleton "a" Canonical.Type.int))
        , test "{ a | name : String }  ~  { name : String }" <|
            \_ ->
                Solver.run
                    [ ( Record [ ( "name", Canonical.Type.string ) ] (Just "a")
                      , Record [ ( "name", Canonical.Type.string ) ] Nothing
                      )
                    ]
                    |> Expect.equal
                        (Ok (Dict.singleton "a" (Record [] Nothing)))
        , test "{ name : String }  ~  { a | name : String }" <|
            \_ ->
                Solver.run
                    [ ( Record [ ( "name", Canonical.Type.string ) ] Nothing
                      , Record [ ( "name", Canonical.Type.string ) ] (Just "a")
                      )
                    ]
                    |> Expect.equal
                        (Ok (Dict.singleton "a" (Record [] Nothing)))
        , test "{ a | name : String }  ~  { name : String, count : Int }" <|
            \_ ->
                Solver.run
                    [ ( Record [ ( "name", Canonical.Type.string ) ] (Just "a")
                      , Record
                            [ ( "name", Canonical.Type.string )
                            , ( "count", Canonical.Type.int )
                            ]
                            Nothing
                      )
                    ]
                    |> Expect.equal
                        (Ok <|
                            Dict.singleton "a" <|
                                Record [ ( "count", Canonical.Type.int ) ] Nothing
                        )
        , test "{ name : String, count : Int }  ~  { a | name : String }" <|
            \_ ->
                Solver.run
                    [ ( Record
                            [ ( "name", Canonical.Type.string )
                            , ( "count", Canonical.Type.int )
                            ]
                            Nothing
                      , Record [ ( "name", Canonical.Type.string ) ] (Just "a")
                      )
                    ]
                    |> Expect.equal
                        (Ok <|
                            Dict.singleton "a" <|
                                Record [ ( "count", Canonical.Type.int ) ] Nothing
                        )
        , test "{ a | name : String }  ~  { b | count : Int }" <|
            \_ ->
                Solver.run
                    [ ( Record [ ( "name", Canonical.Type.string ) ] (Just "a")
                      , Record [ ( "count", Canonical.Type.int ) ] (Just "b")
                      )
                    ]
                    |> Expect.equal
                        (Ok <|
                            Dict.singleton "a" <|
                                Record [ ( "count", Canonical.Type.int ) ] (Just "b")
                        )
        , test "{ a | name : String }  ~  { a | count : Int }" <|
            \_ ->
                Solver.run
                    [ ( Record [ ( "name", Canonical.Type.string ) ] (Just "a")
                      , Record [ ( "count", Canonical.Type.int ) ] (Just "a")
                      )
                    ]
                    |> Expect.equal
                        (Err
                            (Solver.RecordUnificationMismatch
                                [ ( "name", Canonical.Type.string ) ]
                                [ ( "count", Canonical.Type.int ) ]
                            )
                        )
        , test "{ a | name : String, count : Int }  ~  { name : String }" <|
            \_ ->
                Solver.run
                    [ ( Record
                            [ ( "name", Canonical.Type.string )
                            , ( "count", Canonical.Type.int )
                            ]
                            (Just "a")
                      , Record [ ( "name", Canonical.Type.string ) ] Nothing
                      )
                    ]
                    |> Expect.equal
                        (Err
                            (Solver.RecordUnificationMismatch
                                [ ( "count", Canonical.Type.int )
                                , ( "name", Canonical.Type.string )
                                ]
                                [ ( "name", Canonical.Type.string ) ]
                            )
                        )
        , test "{ name : a }  ~  { name : String }" <|
            \_ ->
                Solver.run
                    [ ( Record [ ( "name", Var "a" ) ] Nothing
                      , Record [ ( "name", Canonical.Type.string ) ] Nothing
                      )
                    ]
                    |> Expect.equal
                        (Ok <|
                            Dict.fromList
                                [ ( "a", Canonical.Type.string ) ]
                        )
        , test "{ name : String }  ~  { name : a }" <|
            \_ ->
                Solver.run
                    [ ( Record [ ( "name", Canonical.Type.string ) ] Nothing
                      , Record [ ( "name", Var "a" ) ] Nothing
                      )
                    ]
                    |> Expect.equal
                        (Ok <|
                            Dict.fromList
                                [ ( "a", Canonical.Type.string ) ]
                        )
        , test "{ a | name : String }  ~  { b | name : String}" <|
            \_ ->
                Solver.run
                    [ ( Record [ ( "name", Canonical.Type.string ) ] (Just "a")
                      , Record [ ( "name", Canonical.Type.string ) ] (Just "b")
                      )
                    ]
                    |> Expect.equal
                        (Ok <|
                            Dict.fromList
                                [ ( "a", Record [] (Just "b") ) ]
                        )
        ]
