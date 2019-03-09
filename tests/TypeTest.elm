module TypeTest exposing (suite)

import Dict
import Elm.Type exposing (Type(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import State exposing (State)
import Test exposing (..)
import Type


suite : Test
suite =
    concat
        [ normalizeTest
        , unifierTest
        , unifiableTest
        ]


normalizeTest : Test
normalizeTest =
    let
        aliasA =
            { name = "A"
            , comment = ""
            , args = []
            , tipe = int
            }

        a =
            Type "A" []

        int =
            Type "Int" []

        list tipe =
            Type "List" [ tipe ]

        varA =
            Var "a"

        varB =
            Var "b"
    in
    only <|
        describe "normalize"
            [ test "of [ type alias A = Int ] A" <|
                \_ ->
                    Type.normalize [ aliasA ] a
                        |> Expect.equal int
            , test "of [ type alias A = Int ] (A -> A)" <|
                \_ ->
                    Type.normalize [ aliasA ] (Lambda a a)
                        |> Expect.equal (Lambda int int)
            , test "of [ type alias A = Int ] ( A, A )" <|
                \_ ->
                    Type.normalize [ aliasA ] (Tuple [ a, a ])
                        |> Expect.equal (Tuple [ int, int ])
            , test "of [ type alias A = Int ] { field : A }" <|
                \_ ->
                    Type.normalize [ aliasA ] (Record [ ( "field", a ) ] Nothing)
                        |> Expect.equal (Record [ ( "field", int ) ] Nothing)
            , test "of [ type alias A = List Int ] A" <|
                \_ ->
                    Type.normalize
                        [ { name = "A"
                          , comment = ""
                          , args = []
                          , tipe = list int
                          }
                        ]
                        a
                        |> Expect.equal (list int)
            , test "of [ type alias A a = List a ] (A Int)" <|
                \_ ->
                    Type.normalize
                        [ { name = "A"
                          , comment = ""
                          , args = [ "a" ]
                          , tipe = list varA
                          }
                        ]
                        (Type "A" [ int ])
                        |> Expect.equal (list int)
            , test "of [ type alias A a = List a ] (A a)" <|
                \_ ->
                    Type.normalize
                        [ { name = "A"
                          , comment = ""
                          , args = [ "a" ]
                          , tipe = list varA
                          }
                        ]
                        (Type "A" [ varA ])
                        |> Expect.equal (list varA)
            , test "of [ type alias A a = List a ] (A b)" <|
                \_ ->
                    Type.normalize
                        [ { name = "A"
                          , comment = ""
                          , args = [ "a" ]
                          , tipe = list varB
                          }
                        ]
                        (Type "A" [ varA ])
                        |> Expect.equal (list varB)
            ]


unifierTest : Test
unifierTest =
    let
        varA =
            Var "a"

        varB =
            Var "b"

        varNumber =
            Var "number"

        varComparable =
            Var "comparable"

        varComparableA =
            Var "comparableA"

        varComparableB =
            Var "comparableB"

        int =
            Type "Int" []

        float =
            Type "Float" []

        string =
            Type "String" []

        list tipe =
            Type "List" [ tipe ]
    in
    describe "unifier"
        [ test "of a and Int" <|
            \_ ->
                Type.unifier varA int
                    |> Expect.equal
                        (Just
                            { bindTypeVariables = Dict.singleton "a" int
                            , bindRecordVariables = Dict.empty
                            }
                        )
        , test "of Int and b" <|
            \_ ->
                Type.unifier int (Var "b")
                    |> Expect.equal
                        (Just
                            { bindTypeVariables = Dict.singleton "b" int
                            , bindRecordVariables = Dict.empty
                            }
                        )
        , test "of number and Int" <|
            \_ ->
                Type.unifier varNumber int
                    |> Expect.equal
                        (Just
                            { bindTypeVariables = Dict.singleton "number" int
                            , bindRecordVariables = Dict.empty
                            }
                        )
        , test "of number and Float" <|
            \_ ->
                Type.unifier varNumber float
                    |> Expect.equal
                        (Just
                            { bindTypeVariables = Dict.singleton "number" float
                            , bindRecordVariables = Dict.empty
                            }
                        )
        , test "of number and String" <|
            \_ ->
                Type.unifier varNumber string
                    |> Expect.equal Nothing
        , test "of comparable and String" <|
            \_ ->
                Type.unifier varComparable string
                    |> Expect.equal
                        (Just
                            { bindTypeVariables = Dict.singleton "comparable" string
                            , bindRecordVariables = Dict.empty
                            }
                        )
        , test "of comparable and List Int" <|
            \_ ->
                Type.unifier varComparable (list int)
                    |> Expect.equal
                        (Just
                            { bindTypeVariables = Dict.singleton "comparable" (list int)
                            , bindRecordVariables = Dict.empty
                            }
                        )
        , test "of comparableA and List comparableB" <|
            \_ ->
                Type.unifier varComparableA (list varComparableB)
                    |> Expect.equal
                        (Just
                            { bindTypeVariables =
                                Dict.singleton "comparableA" (list varComparableB)
                            , bindRecordVariables = Dict.empty
                            }
                        )
        , test "of comparable and List number" <|
            \_ ->
                Type.unifier varComparable (list varNumber)
                    |> Expect.equal
                        (Just
                            { bindTypeVariables =
                                Dict.singleton "comparable" (list varNumber)
                            , bindRecordVariables = Dict.empty
                            }
                        )
        , test "of a and b" <|
            \_ ->
                Type.unifier varA varB
                    |> Expect.equal
                        (Just
                            { bindTypeVariables = Dict.singleton "a" varB
                            , bindRecordVariables = Dict.empty
                            }
                        )
        , test "of (a -> b) and (Int -> Int)" <|
            \_ ->
                Type.unifier
                    (Lambda varA varB)
                    (Lambda int int)
                    |> Expect.equal
                        (Just
                            { bindTypeVariables =
                                Dict.fromList
                                    [ ( "a", int )
                                    , ( "b", int )
                                    ]
                            , bindRecordVariables = Dict.empty
                            }
                        )
        , test "of (Int -> Int) and (a -> b)" <|
            \_ ->
                Type.unifier
                    (Lambda int int)
                    (Lambda varA varB)
                    |> Expect.equal
                        (Just
                            { bindTypeVariables =
                                Dict.fromList
                                    [ ( "a", int )
                                    , ( "b", int )
                                    ]
                            , bindRecordVariables = Dict.empty
                            }
                        )
        , test "of ( a, b ) and ( Int, Int )" <|
            \_ ->
                Type.unifier
                    (Tuple [ varA, varB ])
                    (Tuple [ int, int ])
                    |> Expect.equal
                        (Just
                            { bindTypeVariables =
                                Dict.fromList
                                    [ ( "a", int )
                                    , ( "b", int )
                                    ]
                            , bindRecordVariables = Dict.empty
                            }
                        )
        , test "of ( Int, Int ) and ( a, b )" <|
            \_ ->
                Type.unifier
                    (Tuple [ int, int ])
                    (Tuple [ varA, varB ])
                    |> Expect.equal
                        (Just
                            { bindTypeVariables =
                                Dict.fromList
                                    [ ( "a", int )
                                    , ( "b", int )
                                    ]
                            , bindRecordVariables = Dict.empty
                            }
                        )
        , test "of Int and Int" <|
            \_ ->
                Type.unifier int int
                    |> Expect.equal
                        (Just
                            { bindTypeVariables = Dict.empty
                            , bindRecordVariables = Dict.empty
                            }
                        )
        , test "of { field : Int } and { field : Int }" <|
            \_ ->
                Type.unifier
                    (Record [ ( "field", int ) ] Nothing)
                    (Record [ ( "field", int ) ] Nothing)
                    |> Expect.equal
                        (Just
                            { bindTypeVariables = Dict.empty
                            , bindRecordVariables = Dict.empty
                            }
                        )
        , test "of { field : a } and { field : Int }" <|
            \_ ->
                Type.unifier
                    (Record [ ( "field", varA ) ] Nothing)
                    (Record [ ( "field", int ) ] Nothing)
                    |> Expect.equal
                        (Just
                            { bindTypeVariables = Dict.singleton "a" int
                            , bindRecordVariables = Dict.empty
                            }
                        )
        , test "of { a | field : Int } and { field : Int }" <|
            \_ ->
                Type.unifier
                    (Record [ ( "field", int ) ] (Just "a"))
                    (Record [ ( "field", int ) ] Nothing)
                    |> Expect.equal
                        (Just
                            { bindTypeVariables = Dict.empty
                            , bindRecordVariables = Dict.singleton "a" ( [], Nothing )
                            }
                        )
        , test "of { a | fieldA : Int } and { fieldA : Int, fieldB : Int }" <|
            \_ ->
                Type.unifier
                    (Record [ ( "fieldA", int ) ] (Just "a"))
                    (Record
                        [ ( "fieldA", int )
                        , ( "fieldB", int )
                        ]
                        Nothing
                    )
                    |> Expect.equal
                        (Just
                            { bindTypeVariables = Dict.empty
                            , bindRecordVariables =
                                Dict.singleton "a"
                                    ( [ ( "fieldB", int ) ]
                                    , Nothing
                                    )
                            }
                        )
        , test "of { field : Int } and { field : a }" <|
            \_ ->
                Type.unifier
                    (Record [ ( "field", int ) ] Nothing)
                    (Record [ ( "field", varA ) ] Nothing)
                    |> Expect.equal
                        (Just
                            { bindTypeVariables = Dict.singleton "a" int
                            , bindRecordVariables = Dict.empty
                            }
                        )
        , test "of { field : Int } and { a | field : Int }" <|
            \_ ->
                Type.unifier
                    (Record [ ( "field", int ) ] Nothing)
                    (Record [ ( "field", int ) ] (Just "a"))
                    |> Expect.equal
                        (Just
                            { bindTypeVariables = Dict.empty
                            , bindRecordVariables = Dict.singleton "a" ( [], Nothing )
                            }
                        )
        , test "of { fieldA : Int, fieldB : Int } and { a | fieldA : Int }" <|
            \_ ->
                Type.unifier
                    (Record
                        [ ( "fieldA", int )
                        , ( "fieldB", int )
                        ]
                        Nothing
                    )
                    (Record [ ( "fieldA", int ) ] (Just "a"))
                    |> Expect.equal
                        (Just
                            { bindTypeVariables = Dict.empty
                            , bindRecordVariables =
                                Dict.singleton "a"
                                    ( [ ( "fieldB", int ) ]
                                    , Nothing
                                    )
                            }
                        )
        , test "of { a | field : Int } and { a | field : Int }" <|
            \_ ->
                Type.unifier
                    (Record [ ( "field", int ) ] Nothing)
                    (Record [ ( "field", int ) ] Nothing)
                    |> Expect.equal
                        (Just
                            { bindTypeVariables = Dict.empty
                            , bindRecordVariables = Dict.empty
                            }
                        )
        ]


unifiableTest : Test
unifiableTest =
    let
        varA =
            Var "a"

        varB =
            Var "b"

        varNumber =
            Var "number"

        varComparable =
            Var "comparable"

        varComparableA =
            Var "comparableA"

        varComparableB =
            Var "comparableB"

        int =
            Type "Int" []

        float =
            Type "Float" []

        string =
            Type "String" []

        list tipe =
            Type "List" [ tipe ]
    in
    only <|
        describe "unifiable"
            [ test "of (a -> Int) and (Int -> Int) with a = Int" <|
                \_ ->
                    Type.unifiable
                        (Lambda varA int)
                        (Lambda int int)
                        |> State.run
                            { bindTypeVariables = Dict.singleton "a" int
                            , bindRecordVariables = Dict.empty
                            }
                        |> Expect.equal
                            ( True
                            , { bindTypeVariables = Dict.singleton "a" int
                              , bindRecordVariables = Dict.empty
                              }
                            )
            , test "of (a -> Int) and (Int -> Int) with a = Float" <|
                \_ ->
                    Type.unifiable
                        (Lambda varA int)
                        (Lambda int int)
                        |> State.run
                            { bindTypeVariables = Dict.singleton "a" float
                            , bindRecordVariables = Dict.empty
                            }
                        |> Expect.equal
                            ( False
                            , { bindTypeVariables = Dict.singleton "a" float
                              , bindRecordVariables = Dict.empty
                              }
                            )
            ]



---- HELPER


generalize : Type -> State Int Type
generalize type_ =
    let
        replaceWithVar =
            State.advance <|
                \count ->
                    ( Var ("b" ++ String.fromInt count)
                    , count + 1
                    )
    in
    case type_ of
        Type _ [] ->
            replaceWithVar

        Type name subTypes ->
            if List.all isGeneral subTypes then
                replaceWithVar

            else
                subTypes
                    |> State.traverse generalize
                    |> State.map (Type name)

        Lambda from to ->
            if isGeneral from && isGeneral to then
                replaceWithVar

            else
                State.map2 Lambda
                    (generalize from)
                    (generalize to)

        _ ->
            State.state type_


isGeneral : Type -> Bool
isGeneral type_ =
    case type_ of
        Var _ ->
            True

        _ ->
            False



---- FUZZER


typeFuzzer : Fuzzer Type
typeFuzzer =
    typeFuzzerHelp 3


typeFuzzerHelp : Int -> Fuzzer Type
typeFuzzerHelp depth =
    if depth >= 0 then
        Fuzz.oneOf
            [ varFuzzer
            , customTypeFuzzer
            , Fuzz.map2 Lambda
                (typeFuzzerHelp (depth - 1))
                (typeFuzzerHelp (depth - 1))
            , Fuzz.map Tuple
                (Fuzz.list (typeFuzzerHelp (depth - 1)))
            ]

    else
        Fuzz.oneOf
            [ varFuzzer
            , Fuzz.constant (Type "Bool" [])
            , Fuzz.constant (Type "Int" [])
            , Fuzz.constant (Type "Float" [])
            , Fuzz.constant (Type "String" [])
            ]


customTypeFuzzer : Fuzzer Type
customTypeFuzzer =
    Fuzz.intRange 1 3
        |> Fuzz.map
            (\int ->
                Type ("Custom" ++ String.fromInt int)
                    (List.repeat int (Type "Int" []))
            )


varFuzzer : Fuzzer Type
varFuzzer =
    Fuzz.intRange 1 10
        |> Fuzz.map
            (\int ->
                Var ("a" ++ String.fromInt int)
            )
