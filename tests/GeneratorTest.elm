module GeneratorTest exposing (suite)

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

import Canonical exposing (Alias, Union)
import Canonical.Annotation exposing (Annotation(..))
import Canonical.Type exposing (Type(..))
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Generator
    exposing
        ( Generator
        , addUnions
        , addValues
        , all
        , call
        , cases
        , exprToString
        , firstN
        , for
        , record
        , recordUpdate
        , takeValues
        , tuple
        , value
        )
import Set
import Test exposing (..)


suite : Test
suite =
    concat
        [ describe "generators"
            [ valueTest
            , callTest
            , tupleTest
            , recordTest
            , recordUpdateTest
            , casesTest
            , allTest
            , equivalenceTest
            , firstNTest
            , todoTest
            ]
        , takeValuesTest
        ]


valueTest : Test
valueTest =
    testGenerator "value"
        (call []
            |> addValues
                (Dict.insert "Basics.identity"
                    (Canonical.Annotation.fromType (Lambda a a))
                    values
                )
        )
        [ ( Canonical.Type.int
          , [ "int" ]
          )
        , ( Lambda Canonical.Type.int Canonical.Type.int
          , [ "Basics.identity"
            , "Basics.negate"
            ]
          )
        , ( Lambda a a
          , [ "Basics.identity" ]
          )

        -- TODO fix
        --, ( Lambda a Canonical.Type.int
        --  , []
        --  )
        ]


callTest : Test
callTest =
    describe "call"
        [ testGenerator "value"
            (call [ value ]
                |> addValues
                    (Dict.insert "Basics.identity"
                        (Canonical.Annotation.fromType (Lambda a a))
                        values
                    )
            )
            [ ( Canonical.Type.int
              , [ "Basics.identity int"
                , "Basics.negate int"
                ]
              )
            , ( a
              , []
              )
            ]
        , testGenerator "value value"
            (call
                [ value
                , value
                ]
                |> addValues values
            )
            [ ( Canonical.Type.int
              , [ "Basics.modBy int int" ]
              )
            , ( Canonical.Type.list Canonical.Type.string
              , [ "List.map String.fromInt ints"
                , "List.map String.reverse strings"
                ]
              )
            , ( a
              , []
              )
            , ( Canonical.Type.list a
              , []
              )
            ]
        , testGenerator "value (call value)"
            (call
                [ value
                , call [ value ]
                ]
                |> addValues
                    (Dict.insert "Basics.identity"
                        (Canonical.Annotation.fromType (Lambda a a))
                        values
                    )
            )
            [ ( Canonical.Type.int
              , [ "Basics.modBy int (Basics.identity int)"
                , "Basics.modBy int (Basics.negate int)"
                ]
              )
            , ( a
              , []
              )
            ]
        , testGenerator "value value value"
            (call
                [ value
                , value
                , value
                ]
                |> addValues values
            )
            [ ( Canonical.Type.string
              , [ "List.foldl String.append string strings" ]
              )
            , ( a
              , []
              )
            ]
        , describe "call [ call [ value ] ]"
            [ testGenerator "outer function more general"
                (call
                    [ call
                        [ call [] ]
                    ]
                    |> addValues
                        (Dict.map (\_ -> Canonical.Annotation.fromType) <|
                            Dict.fromList
                                [ ( "f"
                                  , Lambda
                                        (Canonical.Type.set a)
                                        (Canonical.Type.list a)
                                  )
                                , ( "g"
                                  , Lambda
                                        Canonical.Type.string
                                        (Canonical.Type.set Canonical.Type.string)
                                  )
                                , ( "h", a )
                                ]
                        )
                )
                [ ( Canonical.Type.list Canonical.Type.string
                  , [ "f (g h)" ]
                  )
                ]
            , testGenerator "outer function more special"
                (call
                    [ call
                        [ call [] ]
                    ]
                    |> addValues
                        (Dict.map (\_ -> Canonical.Annotation.fromType) <|
                            Dict.fromList
                                [ ( "f"
                                  , Lambda
                                        (Canonical.Type.set Canonical.Type.string)
                                        (Canonical.Type.list Canonical.Type.string)
                                  )
                                , ( "g", Lambda a (Canonical.Type.set a) )
                                , ( "h", Canonical.Type.string )
                                ]
                        )
                )
                [ ( Canonical.Type.list Canonical.Type.string
                  , [ "f (g h)" ]
                  )
                ]
            ]
        ]


tupleTest : Test
tupleTest =
    describe "tuple"
        [ testGenerator "( value, value )"
            (tuple
                { first = call []
                , second = call []
                }
                |> addValues values
            )
            [ ( Tuple [ Canonical.Type.int, Canonical.Type.int ]
              , [ "( int, int )" ]
              )
            , ( Tuple [ a, a ]
              , []
              )
            , ( Tuple [ a, Canonical.Type.int ]
              , []
              )
            , ( Tuple [ Canonical.Type.int, a ]
              , []
              )
            ]
        , testGenerator "( value, call value )"
            (tuple
                { first = call []
                , second = call [ value ]
                }
                |> addValues
                    (Dict.insert "Basics.identity"
                        (Canonical.Annotation.fromType (Lambda a a))
                        values
                    )
            )
            [ ( Tuple [ Canonical.Type.int, Canonical.Type.int ]
              , [ "( int, Basics.identity int )"
                , "( int, Basics.negate int )"
                ]
              )
            ]
        ]


recordTest : Test
recordTest =
    describe "record"
        [ testGenerator "value"
            (record value
                |> addValues
                    (Dict.map (\_ -> Canonical.Annotation.fromType) <|
                        Dict.fromList
                            [ ( "int", Canonical.Type.int )
                            , ( "float", Canonical.Type.float )
                            , ( "listInt", Canonical.Type.list Canonical.Type.int )
                            ]
                    )
            )
            [ ( Record
                    [ ( "int", Canonical.Type.int )
                    , ( "float", Canonical.Type.float )
                    , ( "listInt", Canonical.Type.list Canonical.Type.int )
                    ]
                    Nothing
              , [ """{ int = int
, float = float
, listInt = listInt
}"""
                ]
              )
            ]
        ]


recordUpdateTest : Test
recordUpdateTest =
    describe "recordUpdate"
        [ testGenerator "value"
            (recordUpdate (call [])
                |> addValues values
            )
            [ ( Record
                    [ ( "int", Canonical.Type.int )
                    , ( "string", Canonical.Type.string )
                    ]
                    Nothing
              , [ "{ record | int = int }"
                , "{ record | string = string }"
                ]
              )
            ]
        ]


casesTest : Test
casesTest =
    describe "cases"
        [ testGenerator "value branches"
            (cases
                { matched = call []
                , branch =
                    \newValues ->
                        call []
                            |> addValues newValues
                }
                |> addValues values
            )
            [ ( Canonical.Type.int
              , [ """case msg of
    NewInt newInt ->
        newInt

    NewFloat newFloat ->
        int

    NewString newString ->
        int"""
                , """case msg of
    NewInt newInt ->
        int

    NewFloat newFloat ->
        int

    NewString newString ->
        int"""
                ]
              )
            , ( a
              , []
              )
            ]
        ]


allTest : Test
allTest =
    let
        int =
            Canonical.Annotation.fromType Canonical.Type.int
    in
    describe "all"
        [ describe "add and take"
            [ testGenerator "add one and take 1"
                (call []
                    |> addValues (Dict.singleton "int" int)
                    |> takeValues 1
                )
                [ ( Canonical.Type.int
                  , [ "int" ]
                  )
                ]
            , testGenerator "add two and take 1"
                (call []
                    |> addValues (Dict.singleton "intA" int)
                    |> addValues (Dict.singleton "intB" int)
                    |> takeValues 1
                )
                [ ( Canonical.Type.int
                  , [ "intB" ]
                  )
                ]
            , testGenerator "add two, take 1 and add one"
                (call []
                    |> addValues (Dict.singleton "intA" int)
                    |> addValues (Dict.singleton "intB" int)
                    |> takeValues 1
                    |> addValues (Dict.singleton "intC" int)
                )
                [ ( Canonical.Type.int
                  , [ "intC"
                    , "intB"
                    ]
                  )
                ]
            ]
        , testGenerator "nested three times"
            (all
                [ all
                    [ call []
                        |> addValues (Dict.singleton "intC" int)
                    ]
                    |> addValues (Dict.singleton "intB" int)
                ]
                |> addValues (Dict.singleton "intA" int)
            )
            [ ( Canonical.Type.int
              , [ "intC"
                , "intB"
                , "intA"
                ]
              )
            ]
        , describe "nested three times with takeValues 1"
            [ testGenerator "innermost level"
                (all
                    [ all
                        [ call []
                            |> addValues (Dict.singleton "intC" int)
                            |> takeValues 1
                        ]
                        |> addValues (Dict.singleton "intB" int)
                    ]
                    |> addValues (Dict.singleton "intA" int)
                )
                [ ( Canonical.Type.int
                  , [ "intC" ]
                  )
                ]
            , testGenerator "middle level"
                (all
                    [ all
                        [ call []
                            |> addValues (Dict.singleton "intC" int)
                        ]
                        |> addValues (Dict.singleton "intB" int)
                        |> takeValues 1
                    ]
                    |> addValues (Dict.singleton "intA" int)
                )
                [ ( Canonical.Type.int
                  , [ "intC"
                    , "intB"
                    ]
                  )
                ]
            , testGenerator "outermost level"
                (all
                    [ all
                        [ call []
                            |> addValues (Dict.singleton "intC" int)
                        ]
                        |> addValues (Dict.singleton "intB" int)
                    ]
                    |> addValues (Dict.singleton "intA" int)
                    |> takeValues 1
                )
                [ ( Canonical.Type.int
                  , [ "intC"
                    , "intB"
                    , "intA"
                    ]
                  )
                ]
            ]
        , testGenerator "three times addValues"
            (call []
                |> addValues (Dict.singleton "intA" int)
                |> addValues (Dict.singleton "intB" int)
                |> addValues (Dict.singleton "intC" int)
            )
            [ ( Canonical.Type.int
              , [ "intC"
                , "intB"
                , "intA"
                ]
              )
            ]
        , testGenerator "three times addValues with takeValues 1"
            (call []
                |> addValues (Dict.singleton "intA" int)
                |> addValues (Dict.singleton "intB" int)
                |> addValues (Dict.singleton "intC" int)
                |> takeValues 1
            )
            [ ( Canonical.Type.int
              , [ "intC" ]
              )
            ]
        ]


firstNTest : Test
firstNTest =
    describe "firstN"
        [ testGenerator "only 1 result from 2 possibilities"
            (firstN 1 (call [])
                |> addValues
                    (Dict.map (\_ -> Canonical.Annotation.fromType) <|
                        Dict.fromList
                            [ ( "int1", Canonical.Type.int )
                            , ( "int2", Canonical.Type.int )
                            ]
                    )
            )
            [ ( Canonical.Type.int
              , [ "int1" ]
              )
            ]
        , testGenerator "only 2 in nested generator"
            (call
                [ firstN 2 value ]
                |> addValues
                    (Dict.map (\_ -> Canonical.Annotation.fromType) <|
                        Dict.fromList
                            [ ( "int1", Canonical.Type.int )
                            , ( "int2", Canonical.Type.int )
                            , ( "int3", Canonical.Type.int )
                            , ( "func", Lambda Canonical.Type.int Canonical.Type.int )
                            ]
                    )
            )
            [ ( Canonical.Type.int
              , [ "func int1"
                , "func int2"
                ]
              )
            ]
        ]


todoTest : Test
todoTest =
    describe "todo"
        [ testGenerator "with no additinal values"
            Generator.todo
            [ ( Canonical.Type.int
              , [ "Debug.todo \"implementation missing\"" ]
              )
            ]
        , testGenerator "with cases"
            (cases
                { matched = call []
                , branch =
                    \_ ->
                        Generator.todo
                }
                |> addValues values
            )
            [ ( Canonical.Type.int
              , [ """case msg of
    NewInt newInt ->
        Debug.todo "implementation missing"

    NewFloat newFloat ->
        Debug.todo "implementation missing"

    NewString newString ->
        Debug.todo "implementation missing\""""
                ]
              )
            ]
        ]


equivalenceTest : Test
equivalenceTest =
    let
        generatorA =
            call
                [ all
                    [ value
                    , call [ value ]
                    ]
                , value
                ]
                |> addValues values

        generatorB =
            all
                [ call
                    [ value
                    , value
                    ]
                , call
                    [ call [ value ]
                    , value
                    ]
                ]
                |> addValues values
    in
    describe
        ("call [ all [ value, call [ value ] ], value ] "
            ++ "== all [ call [ value, value ], call [ call [ value ], value ] ]"
        )
        [ test "Int" <|
            \_ ->
                for Canonical.Type.int generatorA
                    |> List.map (exprToString False)
                    |> Expect.equal
                        (for Canonical.Type.int generatorB
                            |> List.map (exprToString False)
                        )
        , test "List Int" <|
            \_ ->
                for (Canonical.Type.list Canonical.Type.int) generatorA
                    |> List.map (exprToString False)
                    |> Expect.equal
                        (for (Canonical.Type.list Canonical.Type.int) generatorB
                            |> List.map (exprToString False)
                        )
        ]


takeValuesTest : Test
takeValuesTest =
    describe "takeValues"
        [ testGenerator "at 0"
            (call []
                |> addValues values
                |> takeValues 0
            )
            [ ( Canonical.Type.int
              , []
              )
            ]
        , testGenerator "at 1"
            (call []
                |> addValues values
                |> takeValues 1
            )
            [ ( Canonical.Type.int
              , [ "int" ]
              )
            ]
        , testGenerator "call value at 0"
            (call
                [ value
                    |> addValues
                        (Dict.singleton "newInt"
                            (Canonical.Annotation.fromType Canonical.Type.int)
                        )
                    |> takeValues 1
                ]
                |> addValues values
            )
            [ ( Canonical.Type.int
              , [ "Basics.negate newInt" ]
              )
            ]
        , testGenerator "cases"
            (cases
                { matched = call []
                , branch =
                    \newValues ->
                        all
                            [ call []
                                |> addValues newValues
                                |> takeValues 1
                            , call []
                            ]
                }
                |> addValues values
            )
            [ ( Canonical.Type.int
              , [ """case msg of
    NewInt newInt ->
        newInt

    NewFloat newFloat ->
        int

    NewString newString ->
        int"""
                , """case msg of
    NewInt newInt ->
        int

    NewFloat newFloat ->
        int

    NewString newString ->
        int"""
                ]
              )
            ]
        , testGenerator "cases with recordUpdate"
            (cases
                { matched = call []
                , branch =
                    \newValues ->
                        all
                            [ recordUpdate
                                (call []
                                    |> addValues newValues
                                    |> takeValues 1
                                )
                            , call []
                            ]
                }
                |> addValues (Dict.remove "int" values)
            )
            [ ( Record
                    [ ( "int", Canonical.Type.int ) ]
                    Nothing
              , [ """case msg of
    NewInt newInt ->
        { intRecord | int = newInt }

    NewFloat newFloat ->
        intRecord

    NewString newString ->
        intRecord"""
                , """case msg of
    NewInt newInt ->
        intRecord

    NewFloat newFloat ->
        intRecord

    NewString newString ->
        intRecord"""
                ]
              )
            ]
        ]


a : Type
a =
    Var "a"


b : Type
b =
    Var "b"


msg : Type
msg =
    Type [ "Main" ] "Msg" []


values : Dict String Annotation
values =
    Dict.map (\_ -> Canonical.Annotation.fromType) <|
        Dict.fromList
            [ ( "int", Canonical.Type.int )
            , ( "ints", Canonical.Type.list Canonical.Type.int )
            , ( "float", Canonical.Type.float )
            , ( "string", Canonical.Type.string )
            , ( "strings", Canonical.Type.list Canonical.Type.string )
            , ( "record"
              , Record
                    [ ( "int", Canonical.Type.int )
                    , ( "string", Canonical.Type.string )
                    ]
                    Nothing
              )
            , ( "intRecord"
              , Record
                    [ ( "int", Canonical.Type.int ) ]
                    Nothing
              )
            , ( "msg", msg )

            -- FUNCTIONS WITH ONE ARGUMENT
            , ( "Basics.negate", Lambda Canonical.Type.int Canonical.Type.int )
            , ( "String.reverse", Lambda Canonical.Type.string Canonical.Type.string )
            , ( "String.fromInt", Lambda Canonical.Type.int Canonical.Type.string )

            -- FUNCTIONS WITH TWO ARGUMENTS
            , ( "Basics.modBy"
              , Lambda Canonical.Type.int
                    (Lambda Canonical.Type.int
                        Canonical.Type.int
                    )
              )
            , ( "List.map"
              , Lambda (Lambda a b)
                    (Lambda
                        (Canonical.Type.list a)
                        (Canonical.Type.list b)
                    )
              )
            , ( "String.append"
              , Lambda Canonical.Type.string
                    (Lambda
                        Canonical.Type.string
                        Canonical.Type.string
                    )
              )

            -- FUNCTIONS WITH THREE ARGUMENTS
            , ( "List.foldl"
              , Lambda
                    (Lambda a (Lambda b b))
                    (Lambda b (Lambda (Canonical.Type.list a) b))
              )
            ]


unions : Dict String Union
unions =
    Dict.fromList
        [ ( "Msg"
          , { moduleName = [ "Main" ]
            , vars = []
            , constructors =
                [ ( "NewInt", [ Canonical.Type.int ] )
                , ( "NewFloat", [ Canonical.Type.float ] )
                , ( "NewString", [ Canonical.Type.string ] )
                ]
            }
          )
        ]


testGenerator : String -> Generator -> List ( Type, List String ) -> Test
testGenerator description generator tests =
    let
        testHelp ( targetType, expectation ) =
            test (typeToString targetType) <|
                \_ ->
                    generator
                        |> addUnions unions
                        |> for targetType
                        |> List.map (exprToString False)
                        |> Expect.equal expectation
    in
    describe description <|
        List.map testHelp tests


typeToString : Type -> String
typeToString tipe =
    case tipe of
        Var name ->
            name

        Type moduleName name subTypes ->
            String.join " "
                (String.join "." (moduleName ++ [ name ])
                    :: List.map typeToString subTypes
                )

        Lambda from to ->
            String.join " "
                [ typeToString from
                , "->"
                , typeToString to
                ]

        Tuple subTypes ->
            String.join " "
                [ "("
                , String.join ", " (List.map typeToString subTypes)
                , ")"
                ]

        Unit ->
            "()"

        Record fields maybeVar ->
            let
                fieldToString ( field, subType ) =
                    String.join " "
                        [ field
                        , "="
                        , typeToString subType
                        ]
            in
            String.join " "
                [ case maybeVar of
                    Nothing ->
                        "{"

                    Just var ->
                        "{" ++ var ++ " |"
                , String.join ", " (List.map fieldToString fields)
                , "}"
                ]
