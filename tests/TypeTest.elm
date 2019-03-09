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
        [ isGeneralizationOfTest
        , unifierTest
        ]


isGeneralizationOfTest : Test
isGeneralizationOfTest =
    describe "isGeneralizationOf"
        [ test "a == Int" <|
            \_ ->
                Var "a"
                    |> Type.isGeneralizationOf (Type "Int" [])
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( True, Dict.fromList [ ( "a", Type "Int" [] ) ] )
        , test "a == b" <|
            \_ ->
                Var "a"
                    |> Type.isGeneralizationOf (Var "a")
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( True, Dict.fromList [ ( "a", Var "a" ) ] )
        , test "a == List a" <|
            \_ ->
                Var "a"
                    |> Type.isGeneralizationOf (Type "List" [ Var "a" ])
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( True, Dict.fromList [ ( "a", Type "List" [ Var "a" ] ) ] )
        , test "List a == List String" <|
            \_ ->
                Type "List" [ Var "a" ]
                    |> Type.isGeneralizationOf
                        (Type "List" [ Type "String" [] ])
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( True
                        , Dict.fromList
                            [ ( "a", Type "String" [] ) ]
                        )
        , test "a -> a == Int -> Int" <|
            \_ ->
                Lambda (Var "a") (Var "a")
                    |> Type.isGeneralizationOf
                        (Lambda (Type "Int" []) (Type "Int" []))
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( True
                        , Dict.fromList
                            [ ( "a", Type "Int" [] ) ]
                        )
        , test "a -> a == a -> a" <|
            \_ ->
                Lambda (Var "a") (Var "a")
                    |> Type.isGeneralizationOf (Lambda (Var "a") (Var "a"))
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( True
                        , Dict.fromList [ ( "a", Var "a" ) ]
                        )
        , test "a -> a == b -> b" <|
            \_ ->
                Lambda (Var "a") (Var "a")
                    |> Type.isGeneralizationOf (Lambda (Var "b") (Var "b"))
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( True
                        , Dict.fromList [ ( "a", Var "b" ) ]
                        )
        , test "a -> b == a -> a" <|
            \_ ->
                Lambda (Var "a") (Var "b")
                    |> Type.isGeneralizationOf (Lambda (Var "a") (Var "a"))
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( True
                        , Dict.fromList
                            [ ( "a", Var "a" )
                            , ( "b", Var "a" )
                            ]
                        )
        , test "List a -> Int == List String -> Int" <|
            \_ ->
                Lambda (Type "List" [ Var "a" ]) (Type "Int" [])
                    |> Type.isGeneralizationOf
                        (Lambda (Type "List" [ Type "String" [] ]) (Type "Int" []))
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( True
                        , Dict.fromList
                            [ ( "a", Type "String" [] ) ]
                        )
        , test "List comparable -> comparable == List Int -> Int" <|
            \_ ->
                Lambda
                    (Type "List" [ Var "comparable" ])
                    (Var "comparable")
                    |> Type.isGeneralizationOf
                        (Lambda
                            (Type "List" [ Type "Int" [] ])
                            (Type "Int" [])
                        )
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( True
                        , Dict.fromList
                            [ ( "comparable", Type "Int" [] ) ]
                        )
        , test "number == Int" <|
            \_ ->
                Var "number"
                    |> Type.isGeneralizationOf (Type "Int" [])
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( True
                        , Dict.fromList
                            [ ( "number", Type "Int" [] ) ]
                        )
        , test "number == Float" <|
            \_ ->
                Var "number"
                    |> Type.isGeneralizationOf (Type "Float" [])
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( True
                        , Dict.fromList
                            [ ( "number", Type "Float" [] ) ]
                        )
        , test "number /= String" <|
            \_ ->
                Var "number"
                    |> Type.isGeneralizationOf (Type "String" [])
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( False
                        , Dict.empty
                        )
        , test "a -> a /= Int -> String" <|
            \_ ->
                Lambda (Var "a") (Var "a")
                    |> Type.isGeneralizationOf
                        (Lambda (Type "Int" []) (Type "String" []))
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( False
                        , Dict.fromList [ ( "a", Type "Int" [] ) ]
                        )
        , test "List a -> List a /= List Int -> List" <|
            \_ ->
                Lambda
                    (Type "List" [ Var "a" ])
                    (Type "List" [ Var "a" ])
                    |> Type.isGeneralizationOf
                        (Lambda
                            (Type "List" [ Type "Int" [] ])
                            (Type "List" [])
                        )
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( False
                        , Dict.fromList [ ( "a", Type "Int" [] ) ]
                        )
        , fuzz typeFuzzer "every type is a generalization of itself" <|
            \type_ ->
                type_
                    |> expectToBeGeneralizationOf type_
        , fuzz typeFuzzer "every type is a generalization of a generalization of itself" <|
            \type_ ->
                type_
                    |> generalize
                    |> State.finalValue 1
                    |> expectToBeGeneralizationOf type_
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
    only <|
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



---- EXPECTATIONS


expectToBeGeneralizationOf : Type -> Type -> Expectation
expectToBeGeneralizationOf typeA typeB =
    typeB
        |> Type.isGeneralizationOf typeA
        |> State.finalValue Dict.empty
        |> Expect.true
            ("Expected\n\n    "
                ++ Type.toString typeB
                ++ "\n\nto be a generalization of\n\n    "
                ++ Type.toString typeA
                ++ "\n"
            )



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
