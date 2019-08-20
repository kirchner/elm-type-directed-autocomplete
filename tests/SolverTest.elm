module SolverTest exposing (suite)

import Canonical.Type exposing (Type(..))
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Solver exposing (Comparability(..), Error(..), Unifiability(..))
import Test exposing (..)


suite : Test
suite =
    concat
        [ testRun
        , testUnifiability
        ]


testRun : Test
testRun =
    describe "run"
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
        , test "Result err ok  ~  Result String Int" <|
            \_ ->
                Solver.run
                    [ ( Canonical.Type.result (Var "err") (Var "ok")
                      , Canonical.Type.result Canonical.Type.string Canonical.Type.int
                      )
                    ]
                    |> Expect.equal
                        (Ok
                            (Dict.fromList
                                [ ( "err", Canonical.Type.string )
                                , ( "ok", Canonical.Type.int )
                                ]
                            )
                        )
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


testUnifiability : Test
testUnifiability =
    describe "unifiability"
        [ test "a  ~  b" <|
            \_ ->
                Solver.unifiability
                    { typeA = a
                    , typeB = b
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            (Dict.singleton "a" b)
                        )
        , test "a  ~  Int" <|
            \_ ->
                Solver.unifiability
                    { typeA = a
                    , typeB = Canonical.Type.int
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            (Dict.singleton "a" Canonical.Type.int)
                        )
        , test "Int  ~  a" <|
            \_ ->
                Solver.unifiability
                    { typeA = Canonical.Type.int
                    , typeB = a
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeBIsMoreGeneral
                            (Dict.singleton "a" Canonical.Type.int)
                        )
        , test "List a  ~  List Int" <|
            \_ ->
                Solver.unifiability
                    { typeA = Canonical.Type.list a
                    , typeB = Canonical.Type.list Canonical.Type.int
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            (Dict.singleton "a" Canonical.Type.int)
                        )
        , test "List Int  ~  List a" <|
            \_ ->
                Solver.unifiability
                    { typeA = Canonical.Type.list Canonical.Type.int
                    , typeB = Canonical.Type.list a
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeBIsMoreGeneral
                            (Dict.singleton "a" Canonical.Type.int)
                        )
        , test "Result err ok  ~  Result String Int" <|
            \_ ->
                Solver.unifiability
                    { typeA = Canonical.Type.result (Var "err") (Var "ok")
                    , typeB = Canonical.Type.result Canonical.Type.string Canonical.Type.int
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            (Dict.fromList
                                [ ( "err", Canonical.Type.string )
                                , ( "ok", Canonical.Type.int )
                                ]
                            )
                        )
        , test "number  ~  Int" <|
            \_ ->
                Solver.unifiability
                    { typeA = num
                    , typeB = Canonical.Type.int
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            (Dict.singleton "number" Canonical.Type.int)
                        )
        , test "number  ~  Float" <|
            \_ ->
                Solver.unifiability
                    { typeA = num
                    , typeB = Canonical.Type.float
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            (Dict.singleton "number" Canonical.Type.float)
                        )
        , test "number  ~  String" <|
            \_ ->
                Solver.unifiability
                    { typeA = num
                    , typeB = Canonical.Type.string
                    }
                    |> Expect.equal (NotUnifiable (NotNumberType Canonical.Type.string))
        , test "comparable  ~  String" <|
            \_ ->
                Solver.unifiability
                    { typeA = comparable
                    , typeB = Canonical.Type.string
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            (Dict.singleton "comparable" Canonical.Type.string)
                        )
        , test "comparable  ~  List Int" <|
            \_ ->
                Solver.unifiability
                    { typeA = comparable
                    , typeB = Canonical.Type.list Canonical.Type.int
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            (Dict.singleton "comparable"
                                (Canonical.Type.list Canonical.Type.int)
                            )
                        )
        , test "comparableA  ~  List comparableB" <|
            \_ ->
                Solver.unifiability
                    { typeA = comparableA
                    , typeB = Canonical.Type.list comparableB
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            (Dict.singleton "comparableA"
                                (Canonical.Type.list comparableB)
                            )
                        )
        , test "comparable  ~  List number" <|
            \_ ->
                Solver.unifiability
                    { typeA = comparable
                    , typeB = Canonical.Type.list num
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            (Dict.singleton "comparable" (Canonical.Type.list num))
                        )
        , test "comparable  ~  a -> b" <|
            \_ ->
                Solver.unifiability
                    { typeA = comparable
                    , typeB = Lambda a b
                    }
                    |> Expect.equal (NotUnifiable (NotComparableType (Lambda a b)))
        , test "Result err Int  ~  Result String a" <|
            \_ ->
                Solver.unifiability
                    { typeA = Canonical.Type.result err Canonical.Type.int
                    , typeB = Canonical.Type.result Canonical.Type.string a
                    }
                    |> Expect.equal
                        (Unifiable
                            NotComparable
                            (Dict.fromList
                                [ ( "err", Canonical.Type.string )
                                , ( "a", Canonical.Type.int )
                                ]
                            )
                        )
        , test "Result String Int  ~  Result String Int" <|
            \_ ->
                Solver.unifiability
                    { typeA =
                        Canonical.Type.result
                            Canonical.Type.string
                            Canonical.Type.int
                    , typeB =
                        Canonical.Type.result
                            Canonical.Type.string
                            Canonical.Type.int
                    }
                    |> Expect.equal
                        (Unifiable
                            TypesAreEqual
                            Dict.empty
                        )
        , test "String  ~  Int" <|
            \_ ->
                Solver.unifiability
                    { typeA = Canonical.Type.string
                    , typeB = Canonical.Type.int
                    }
                    |> Expect.equal
                        (NotUnifiable
                            (UnificationFail Canonical.Type.string Canonical.Type.int)
                        )
        , test "(a -> b)  ~  (Int -> Int)" <|
            \_ ->
                Solver.unifiability
                    { typeA = Lambda a b
                    , typeB = Lambda Canonical.Type.int Canonical.Type.int
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            (Dict.fromList
                                [ ( "a", Canonical.Type.int )
                                , ( "b", Canonical.Type.int )
                                ]
                            )
                        )
        , test "(Int -> Int)  ~  (a -> a)" <|
            \_ ->
                Solver.unifiability
                    { typeA = Lambda Canonical.Type.int Canonical.Type.int
                    , typeB = Lambda a b
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeBIsMoreGeneral
                            (Dict.fromList
                                [ ( "a", Canonical.Type.int )
                                , ( "b", Canonical.Type.int )
                                ]
                            )
                        )
        , test "( a, b )  ~  ( Int, Int )" <|
            \_ ->
                Solver.unifiability
                    { typeA = Tuple [ a, b ]
                    , typeB = Tuple [ Canonical.Type.int, Canonical.Type.int ]
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            (Dict.fromList
                                [ ( "a", Canonical.Type.int )
                                , ( "b", Canonical.Type.int )
                                ]
                            )
                        )
        , test "( Int, Int )  ~  ( a, a )" <|
            \_ ->
                Solver.unifiability
                    { typeA = Tuple [ Canonical.Type.int, Canonical.Type.int ]
                    , typeB = Tuple [ a, b ]
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeBIsMoreGeneral
                            (Dict.fromList
                                [ ( "a", Canonical.Type.int )
                                , ( "b", Canonical.Type.int )
                                ]
                            )
                        )
        , test "{ field : Int }  ~  { field : Int }" <|
            \_ ->
                Solver.unifiability
                    { typeA = Record [ ( "field", Canonical.Type.int ) ] Nothing
                    , typeB = Record [ ( "field", Canonical.Type.int ) ] Nothing
                    }
                    |> Expect.equal
                        (Unifiable TypesAreEqual Dict.empty)
        , test "{ field : a }  ~  { field : Int }" <|
            \_ ->
                Solver.unifiability
                    { typeA = Record [ ( "field", a ) ] Nothing
                    , typeB = Record [ ( "field", Canonical.Type.int ) ] Nothing
                    }
                    |> Expect.equal
                        (Unifiable TypeAIsMoreGeneral
                            (Dict.singleton "a" Canonical.Type.int)
                        )
        , test "{ field : Int }  ~  { field : a }" <|
            \_ ->
                Solver.unifiability
                    { typeA = Record [ ( "field", Canonical.Type.int ) ] Nothing
                    , typeB = Record [ ( "field", a ) ] Nothing
                    }
                    |> Expect.equal
                        (Unifiable TypeBIsMoreGeneral
                            (Dict.singleton "a" Canonical.Type.int)
                        )
        , test "{ a | field : Int }  ~  { field : Int }" <|
            \_ ->
                Solver.unifiability
                    { typeA = Record [ ( "field", Canonical.Type.int ) ] (Just "a")
                    , typeB = Record [ ( "field", Canonical.Type.int ) ] Nothing
                    }
                    |> Expect.equal
                        (Unifiable TypeAIsMoreGeneral
                            (Dict.singleton "a" (Record [] Nothing))
                        )
        , test "{ a | fieldA : Int }  ~  { fieldA : Int, fieldB : Int }" <|
            \_ ->
                Solver.unifiability
                    { typeA = Record [ ( "fieldA", Canonical.Type.int ) ] (Just "a")
                    , typeB =
                        Record
                            [ ( "fieldA", Canonical.Type.int )
                            , ( "fieldB", Canonical.Type.int )
                            ]
                            Nothing
                    }
                    |> Expect.equal
                        (Unifiable TypeAIsMoreGeneral
                            (Dict.singleton "a"
                                (Record [ ( "fieldB", Canonical.Type.int ) ]
                                    Nothing
                                )
                            )
                        )
        , test "{ field : Int }  ~  { a | field : Int }" <|
            \_ ->
                Solver.unifiability
                    { typeA = Record [ ( "field", Canonical.Type.int ) ] Nothing
                    , typeB = Record [ ( "field", Canonical.Type.int ) ] (Just "a")
                    }
                    |> Expect.equal
                        (Unifiable TypeBIsMoreGeneral
                            (Dict.singleton "a" (Record [] Nothing))
                        )
        , test "{ fieldA : Int, fieldB : Int }  ~  { a | fieldB : Int }" <|
            \_ ->
                Solver.unifiability
                    { typeA =
                        Record
                            [ ( "fieldA", Canonical.Type.int )
                            , ( "fieldB", Canonical.Type.int )
                            ]
                            Nothing
                    , typeB = Record [ ( "fieldA", Canonical.Type.int ) ] (Just "a")
                    }
                    |> Expect.equal
                        (Unifiable TypeBIsMoreGeneral
                            (Dict.singleton "a"
                                (Record [ ( "fieldB", Canonical.Type.int ) ]
                                    Nothing
                                )
                            )
                        )
        , test "{ a | field : Int }  ~  { a | field : Int }" <|
            \_ ->
                Solver.unifiability
                    { typeA = Record [ ( "field", Canonical.Type.int ) ] (Just "a")
                    , typeB = Record [ ( "field", Canonical.Type.int ) ] (Just "a")
                    }
                    |> Expect.equal (Unifiable TypesAreEqual Dict.empty)
        ]



---- HELPER


a =
    Var "a"


b =
    Var "b"


c =
    Var "c"


num =
    Var "number"


comparable =
    Var "comparable"


comparableA =
    Var "comparableA"


comparableB =
    Var "comparableB"


err =
    Var "err"
