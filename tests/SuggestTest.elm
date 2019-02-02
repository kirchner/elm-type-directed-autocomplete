module SuggestTest exposing (suite)

import Dict
import Elm.Type exposing (Type(..))
import Expect
import Fuzz exposing (Fuzzer)
import State exposing (State)
import Suggest
import Test exposing (..)


suite : Test
suite =
    concat
        [ isGeneralizationOfTest ]


isGeneralizationOfTest : Test
isGeneralizationOfTest =
    describe "isGeneralizationOf"
        [ test "a == Int" <|
            \_ ->
                Var "a"
                    |> Suggest.isGeneralizationOf (Type "Int" [])
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( True, Dict.fromList [ ( "a", Type "Int" [] ) ] )
        , test "a == b" <|
            \_ ->
                Var "a"
                    |> Suggest.isGeneralizationOf (Var "a")
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( True, Dict.fromList [ ( "a", Var "a" ) ] )
        , test "a == List a" <|
            \_ ->
                Var "a"
                    |> Suggest.isGeneralizationOf (Type "List" [ Var "a" ])
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( True, Dict.fromList [ ( "a", Type "List" [ Var "a" ] ) ] )
        , test "List a == List String" <|
            \_ ->
                Type "List" [ Var "a" ]
                    |> Suggest.isGeneralizationOf
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
                    |> Suggest.isGeneralizationOf
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
                    |> Suggest.isGeneralizationOf (Lambda (Var "a") (Var "a"))
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( True
                        , Dict.fromList [ ( "a", Var "a" ) ]
                        )
        , test "a -> a == b -> b" <|
            \_ ->
                Lambda (Var "a") (Var "a")
                    |> Suggest.isGeneralizationOf (Lambda (Var "b") (Var "b"))
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( True
                        , Dict.fromList [ ( "a", Var "b" ) ]
                        )
        , test "a -> b == a -> a" <|
            \_ ->
                Lambda (Var "a") (Var "b")
                    |> Suggest.isGeneralizationOf (Lambda (Var "a") (Var "a"))
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
                    |> Suggest.isGeneralizationOf
                        (Lambda (Type "List" [ Type "String" [] ]) (Type "Int" []))
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( True
                        , Dict.fromList
                            [ ( "a", Type "String" [] ) ]
                        )
        , test "number == Int" <|
            \_ ->
                Var "number"
                    |> Suggest.isGeneralizationOf (Type "Int" [])
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( True
                        , Dict.fromList
                            [ ( "number", Type "Int" [] ) ]
                        )
        , test "number == Float" <|
            \_ ->
                Var "number"
                    |> Suggest.isGeneralizationOf (Type "Float" [])
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( True
                        , Dict.fromList
                            [ ( "number", Type "Float" [] ) ]
                        )
        , test "number /= String" <|
            \_ ->
                Var "number"
                    |> Suggest.isGeneralizationOf (Type "String" [])
                    |> State.run Dict.empty
                    |> Expect.equal
                        ( False
                        , Dict.empty
                        )
        , test "a -> a /= Int -> String" <|
            \_ ->
                Lambda (Var "a") (Var "a")
                    |> Suggest.isGeneralizationOf
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
                    |> Suggest.isGeneralizationOf
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


expectToBeGeneralizationOf typeA typeB =
    typeB
        |> Suggest.isGeneralizationOf typeA
        |> State.finalValue Dict.empty
        |> Expect.true
            ("Expected\n\n    "
                ++ Suggest.printType typeB
                ++ "\n\nto be a generalization of\n\n    "
                ++ Suggest.printType typeA
                ++ "\n"
            )



---- HELP


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
