module SolverTest exposing (suite)

import Dict
import Elm.Type exposing (Type(..))
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
                      , Type "Int" []
                      )
                    ]
                    |> Expect.equal
                        (Ok (Dict.singleton "a" (Type "Int" [])))
        , test "List a  ~  List Int" <|
            \_ ->
                Solver.run
                    [ ( Type "List" [ Var "a" ]
                      , Type "List" [ Type "Int" [] ]
                      )
                    ]
                    |> Expect.equal
                        (Ok (Dict.singleton "a" (Type "Int" [])))
        , test "a -> String  ~  Int -> String" <|
            \_ ->
                Solver.run
                    [ ( Lambda (Var "a") (Type "String" [])
                      , Lambda (Type "Int" []) (Type "String" [])
                      )
                    ]
                    |> Expect.equal
                        (Ok (Dict.singleton "a" (Type "Int" [])))
        , test "String -> a  ~  String -> Int" <|
            \_ ->
                Solver.run
                    [ ( Lambda (Type "String" []) (Var "a")
                      , Lambda (Type "String" []) (Type "Int" [])
                      )
                    ]
                    |> Expect.equal
                        (Ok (Dict.singleton "a" (Type "Int" [])))
        , test "{ a | name : String }  ~  { name : String }" <|
            \_ ->
                Solver.run
                    [ ( Record [ ( "name", Type "String" [] ) ] (Just "a")
                      , Record [ ( "name", Type "String" [] ) ] Nothing
                      )
                    ]
                    |> Expect.equal
                        (Ok (Dict.singleton "a" (Record [] Nothing)))
        , test "{ name : String }  ~  { a | name : String }" <|
            \_ ->
                Solver.run
                    [ ( Record [ ( "name", Type "String" [] ) ] Nothing
                      , Record [ ( "name", Type "String" [] ) ] (Just "a")
                      )
                    ]
                    |> Expect.equal
                        (Ok (Dict.singleton "a" (Record [] Nothing)))
        , test "{ a | name : String }  ~  { name : String, count : Int }" <|
            \_ ->
                Solver.run
                    [ ( Record [ ( "name", Type "String" [] ) ] (Just "a")
                      , Record
                            [ ( "name", Type "String" [] )
                            , ( "count", Type "Int" [] )
                            ]
                            Nothing
                      )
                    ]
                    |> Expect.equal
                        (Ok <|
                            Dict.singleton "a" <|
                                Record [ ( "count", Type "Int" [] ) ] Nothing
                        )
        , test "{ name : String, count : Int }  ~  { a | name : String }" <|
            \_ ->
                Solver.run
                    [ ( Record
                            [ ( "name", Type "String" [] )
                            , ( "count", Type "Int" [] )
                            ]
                            Nothing
                      , Record [ ( "name", Type "String" [] ) ] (Just "a")
                      )
                    ]
                    |> Expect.equal
                        (Ok <|
                            Dict.singleton "a" <|
                                Record [ ( "count", Type "Int" [] ) ] Nothing
                        )
        , test "{ a | name : String }  ~  { b | count : Int }" <|
            \_ ->
                Solver.run
                    [ ( Record [ ( "name", Type "String" [] ) ] (Just "a")
                      , Record [ ( "count", Type "Int" [] ) ] (Just "b")
                      )
                    ]
                    |> Expect.equal
                        (Ok <|
                            Dict.singleton "a" <|
                                Record [ ( "count", Type "Int" [] ) ] (Just "b")
                        )
        , test "{ a | name : String }  ~  { a | count : Int }" <|
            \_ ->
                Solver.run
                    [ ( Record [ ( "name", Type "String" [] ) ] (Just "a")
                      , Record [ ( "count", Type "Int" [] ) ] (Just "a")
                      )
                    ]
                    |> Expect.equal
                        (Err
                            (Solver.RecordUnificationMismatch
                                [ ( "name", Type "String" [] ) ]
                                [ ( "count", Type "Int" [] ) ]
                            )
                        )
        , test "{ a | name : String, count : Int }  ~  { name : String }" <|
            \_ ->
                Solver.run
                    [ ( Record
                            [ ( "name", Type "String" [] )
                            , ( "count", Type "Int" [] )
                            ]
                            (Just "a")
                      , Record [ ( "name", Type "String" [] ) ] Nothing
                      )
                    ]
                    |> Expect.equal
                        (Err
                            (Solver.RecordUnificationMismatch
                                [ ( "count", Type "Int" [] )
                                , ( "name", Type "String" [] )
                                ]
                                [ ( "name", Type "String" [] ) ]
                            )
                        )
        , test "{ name : a }  ~  { name : String }" <|
            \_ ->
                Solver.run
                    [ ( Record [ ( "name", Var "a" ) ] Nothing
                      , Record [ ( "name", Type "String" [] ) ] Nothing
                      )
                    ]
                    |> Expect.equal
                        (Ok <|
                            Dict.fromList
                                [ ( "a", Type "String" [] ) ]
                        )
        , test "{ name : String }  ~  { name : a }" <|
            \_ ->
                Solver.run
                    [ ( Record [ ( "name", Type "String" [] ) ] Nothing
                      , Record [ ( "name", Var "a" ) ] Nothing
                      )
                    ]
                    |> Expect.equal
                        (Ok <|
                            Dict.fromList
                                [ ( "a", Type "String" [] ) ]
                        )
        , test "{ a | name : String }  ~  { b | name : String}" <|
            \_ ->
                Solver.run
                    [ ( Record [ ( "name", Type "String" [] ) ] (Just "a")
                      , Record [ ( "name", Type "String" [] ) ] (Just "b")
                      )
                    ]
                    |> Expect.equal
                        (Ok <|
                            Dict.fromList
                                [ ( "a", Record [] (Just "b") ) ]
                        )
        ]
