module TypeTest exposing (suite)

{-

   Copyright 2019 Fabian Kirchner

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

-}

import Dict
import Elm.Type exposing (Type(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import State exposing (State)
import Test exposing (..)
import Type
    exposing
        ( Comparability(..)
        , Unifiability(..)
        , noSubstitutions
        )


suite : Test
suite =
    concat
        [ normalizeTest
        , unifiabilityTest
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

        typeA =
            Type "A" []
    in
    describe "normalize"
        [ test "of [ type alias A = Int ] A" <|
            \_ ->
                Type.normalize [ aliasA ] typeA
                    |> Expect.equal int
        , test "of [ type alias A = Int ] (A -> A)" <|
            \_ ->
                Type.normalize [ aliasA ] (Lambda typeA typeA)
                    |> Expect.equal (Lambda int int)
        , test "of [ type alias A = Int ] ( A, A )" <|
            \_ ->
                Type.normalize [ aliasA ] (Tuple [ typeA, typeA ])
                    |> Expect.equal (Tuple [ int, int ])
        , test "of [ type alias A = Int ] { field : A }" <|
            \_ ->
                Type.normalize [ aliasA ] (Record [ ( "field", typeA ) ] Nothing)
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
                    typeA
                    |> Expect.equal (list int)
        , test "of [ type alias A a = List a ] (A Int)" <|
            \_ ->
                Type.normalize
                    [ { name = "A"
                      , comment = ""
                      , args = [ "a" ]
                      , tipe = list a
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
                      , tipe = list a
                      }
                    ]
                    (Type "A" [ a ])
                    |> Expect.equal (list a)
        , test "of [ type alias A a = List a ] (A b)" <|
            \_ ->
                Type.normalize
                    [ { name = "A"
                      , comment = ""
                      , args = [ "a" ]
                      , tipe = list b
                      }
                    ]
                    (Type "A" [ a ])
                    |> Expect.equal (list b)
        , test "of [ type alias A = { b : B }, type alias B = Int ] A" <|
            \_ ->
                Type.normalize
                    [ { name = "A"
                      , comment = ""
                      , args = []
                      , tipe = Record [ ( "b", Type "B" [] ) ] Nothing
                      }
                    , { name = "B"
                      , comment = ""
                      , args = []
                      , tipe = Type "Int" []
                      }
                    ]
                    (Type "A" [])
                    |> Expect.equal
                        (Record
                            [ ( "b", Type "Int" [] ) ]
                            Nothing
                        )
        ]


unifiabilityTest : Test
unifiabilityTest =
    describe "unifiability"
        [ test "of a and b" <|
            \_ ->
                Type.unifiability
                    { typeA = a
                    , typeB = b
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            { noSubstitutions | bindTypeVariables = Dict.singleton "a" b }
                        )
        , test "of a and Int" <|
            \_ ->
                Type.unifiability
                    { typeA = a
                    , typeB = int
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            { noSubstitutions | bindTypeVariables = Dict.singleton "a" int }
                        )
        , test "of Int and a" <|
            \_ ->
                Type.unifiability
                    { typeA = int
                    , typeB = a
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeBIsMoreGeneral
                            { noSubstitutions | bindTypeVariables = Dict.singleton "a" int }
                        )
        , test "of List a and List Int" <|
            \_ ->
                Type.unifiability
                    { typeA = list a
                    , typeB = list int
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            { noSubstitutions | bindTypeVariables = Dict.singleton "a" int }
                        )
        , test "of List Int and List a" <|
            \_ ->
                Type.unifiability
                    { typeA = list int
                    , typeB = list a
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeBIsMoreGeneral
                            { noSubstitutions | bindTypeVariables = Dict.singleton "a" int }
                        )
        , test "of number and Int" <|
            \_ ->
                Type.unifiability
                    { typeA = num
                    , typeB = int
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            { noSubstitutions
                                | bindTypeVariables = Dict.singleton "number" int
                            }
                        )
        , test "of number and Float" <|
            \_ ->
                Type.unifiability
                    { typeA = num
                    , typeB = float
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            { noSubstitutions
                                | bindTypeVariables = Dict.singleton "number" float
                            }
                        )
        , test "of number and String" <|
            \_ ->
                Type.unifiability
                    { typeA = num
                    , typeB = string
                    }
                    |> Expect.equal NotUnifiable
        , test "of comparable and String" <|
            \_ ->
                Type.unifiability
                    { typeA = comparable
                    , typeB = string
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            { noSubstitutions
                                | bindTypeVariables = Dict.singleton "comparable" string
                            }
                        )
        , test "of comparable and List Int" <|
            \_ ->
                Type.unifiability
                    { typeA = comparable
                    , typeB = list int
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            { noSubstitutions
                                | bindTypeVariables =
                                    Dict.singleton "comparable" (list int)
                            }
                        )
        , test "of comparableA and List comparableB" <|
            \_ ->
                Type.unifiability
                    { typeA = comparableA
                    , typeB = list comparableB
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            { noSubstitutions
                                | bindTypeVariables =
                                    Dict.singleton "comparableA" (list comparableB)
                            }
                        )
        , test "of comparable and List number" <|
            \_ ->
                Type.unifiability
                    { typeA = comparable
                    , typeB = list num
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            { noSubstitutions
                                | bindTypeVariables =
                                    Dict.singleton "comparable" (list num)
                            }
                        )
        , test "of Result err Int and Result String a" <|
            \_ ->
                Type.unifiability
                    { typeA = result err int
                    , typeB = result string a
                    }
                    |> Expect.equal
                        (Unifiable
                            NotComparable
                            { noSubstitutions
                                | bindTypeVariables =
                                    Dict.fromList
                                        [ ( "err", string )
                                        , ( "a", int )
                                        ]
                            }
                        )
        , test "of Result String Int and Result String Int" <|
            \_ ->
                Type.unifiability
                    { typeA = result string int
                    , typeB = result string int
                    }
                    |> Expect.equal
                        (Unifiable
                            TypesAreEqual
                            noSubstitutions
                        )
        , test "of String and Int" <|
            \_ ->
                Type.unifiability
                    { typeA = string
                    , typeB = int
                    }
                    |> Expect.equal NotUnifiable
        , test "of (a -> b) and (Int -> Int)" <|
            \_ ->
                Type.unifiability
                    { typeA = Lambda a b
                    , typeB = Lambda int int
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            { noSubstitutions
                                | bindTypeVariables =
                                    Dict.fromList
                                        [ ( "a", int )
                                        , ( "b", int )
                                        ]
                            }
                        )
        , test "of (Int -> Int) and (a -> a)" <|
            \_ ->
                Type.unifiability
                    { typeA = Lambda int int
                    , typeB = Lambda a b
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeBIsMoreGeneral
                            { noSubstitutions
                                | bindTypeVariables =
                                    Dict.fromList
                                        [ ( "a", int )
                                        , ( "b", int )
                                        ]
                            }
                        )
        , test "of ( a, b ) and ( Int, Int )" <|
            \_ ->
                Type.unifiability
                    { typeA = Tuple [ a, b ]
                    , typeB = Tuple [ int, int ]
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeAIsMoreGeneral
                            { noSubstitutions
                                | bindTypeVariables =
                                    Dict.fromList
                                        [ ( "a", int )
                                        , ( "b", int )
                                        ]
                            }
                        )
        , test "of ( Int, Int ) and ( a, a )" <|
            \_ ->
                Type.unifiability
                    { typeA = Tuple [ int, int ]
                    , typeB = Tuple [ a, b ]
                    }
                    |> Expect.equal
                        (Unifiable
                            TypeBIsMoreGeneral
                            { noSubstitutions
                                | bindTypeVariables =
                                    Dict.fromList
                                        [ ( "a", int )
                                        , ( "b", int )
                                        ]
                            }
                        )
        , test "of { field : Int } and { field : Int }" <|
            \_ ->
                Type.unifiability
                    { typeA = Record [ ( "field", int ) ] Nothing
                    , typeB = Record [ ( "field", int ) ] Nothing
                    }
                    |> Expect.equal
                        (Unifiable TypesAreEqual noSubstitutions)
        , test "of { field : a } and { field : Int }" <|
            \_ ->
                Type.unifiability
                    { typeA = Record [ ( "field", a ) ] Nothing
                    , typeB = Record [ ( "field", int ) ] Nothing
                    }
                    |> Expect.equal
                        (Unifiable TypeAIsMoreGeneral
                            { noSubstitutions
                                | bindTypeVariables = Dict.singleton "a" int
                            }
                        )
        , test "of { field : Int } and { field : a }" <|
            \_ ->
                Type.unifiability
                    { typeA = Record [ ( "field", int ) ] Nothing
                    , typeB = Record [ ( "field", a ) ] Nothing
                    }
                    |> Expect.equal
                        (Unifiable TypeBIsMoreGeneral
                            { noSubstitutions
                                | bindTypeVariables = Dict.singleton "a" int
                            }
                        )

        --, test "of { a | field : Int } and { field : Int }" <|
        --    \_ ->
        --        Type.unifiability
        --            { typeA = Record [ ( "field", int ) ] (Just "a")
        --            , typeB = Record [ ( "field", int ) ] Nothing
        --            }
        --            |> Expect.equal
        --                (Unifiable TypeAIsMoreGeneral
        --                    { noSubstitutions
        --                        | bindRecordVariables =
        --                            Dict.singleton "a" ( [], Nothing )
        --                    }
        --                )
        --, test "of { a | fieldA : Int } and { fieldA : Int, fieldB : Int }" <|
        --    \_ ->
        --        Type.unifiability
        --            { typeA = Record [ ( "fieldA", int ) ] (Just "a")
        --            , typeB =
        --                Record
        --                    [ ( "fieldA", int )
        --                    , ( "fieldB", int )
        --                    ]
        --                    Nothing
        --            }
        --            |> Expect.equal
        --                (Unifiable TypeAIsMoreGeneral
        --                    { noSubstitutions
        --                        | bindRecordVariables =
        --                            Dict.singleton "a"
        --                                ( [ ( "fieldB", int ) ]
        --                                , Nothing
        --                                )
        --                    }
        --                )
        --, test "of { field : Int } and { a | field : Int }" <|
        --    \_ ->
        --        Type.unifiability
        --            { typeA = Record [ ( "field", int ) ] Nothing
        --            , typeB = Record [ ( "field", int ) ] (Just "a")
        --            }
        --            |> Expect.equal
        --                (Unifiable TypeBIsMoreGeneral
        --                    { noSubstitutions
        --                        | bindRecordVariables =
        --                            Dict.singleton "a" ( [], Nothing )
        --                    }
        --                )
        --, test "of { fieldA : Int, fieldB : Int } and { a | fieldB : Int }" <|
        --    \_ ->
        --        Type.unifiability
        --            { typeA =
        --                Record
        --                    [ ( "fieldA", int )
        --                    , ( "fieldB", int )
        --                    ]
        --                    Nothing
        --            , typeB = Record [ ( "fieldA", int ) ] (Just "a")
        --            }
        --            |> Expect.equal
        --                (Unifiable TypeBIsMoreGeneral
        --                    { noSubstitutions
        --                        | bindRecordVariables =
        --                            Dict.singleton "a"
        --                                ( [ ( "fieldB", int ) ]
        --                                , Nothing
        --                                )
        --                    }
        --                )
        , test "of { a | field : Int } and { a | field : Int }" <|
            \_ ->
                Type.unifiability
                    { typeA = Record [ ( "field", int ) ] (Just "a")
                    , typeB = Record [ ( "field", int ) ] (Just "a")
                    }
                    |> Expect.equal (Unifiable TypesAreEqual noSubstitutions)
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


int =
    Type "Int" []


float =
    Type "Float" []


string =
    Type "String" []


list tipe =
    Type "List" [ tipe ]


result error a_ =
    Type "Result" [ error, a_ ]


err =
    Var "err"


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
            (\int_ ->
                Type ("Custom" ++ String.fromInt int_)
                    (List.repeat int_ (Type "Int" []))
            )


varFuzzer : Fuzzer Type
varFuzzer =
    Fuzz.intRange 1 10
        |> Fuzz.map
            (\int_ ->
                Var ("a" ++ String.fromInt int_)
            )
