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

import Canonical exposing (Constructor, Type)
import Canonical.Annotation exposing (Annotation(..))
import Canonical.Type as Can
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Generator
    exposing
        ( Generator
        , Union
        , addValues
        , all
        , call
        , cases
        , exprToString
        , firstN
        , for
        , jsonDecoder
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

            --, jsonDecoderTest
            ]
        , takeValuesTest
        ]


valueTest : Test
valueTest =
    testGenerator "value"
        (call []
            |> addValues
                (Dict.insert "Basics.identity"
                    (Canonical.Annotation.fromType (Can.Lambda a a))
                    values
                )
        )
        [ ( Can.int
          , [ "int" ]
          )
        , ( Can.Lambda Can.int Can.int
          , [ "Basics.identity"
            , "Basics.negate"
            ]
          )
        , ( Can.Lambda a a
          , [ "Basics.identity" ]
          )

        -- TODO fix
        --, ( Can.Lambda a Can.int
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
                        (Canonical.Annotation.fromType (Can.Lambda a a))
                        values
                    )
            )
            [ ( Can.int
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
            [ ( Can.int
              , [ "Basics.modBy int int" ]
              )
            , ( Can.list Can.string
              , [ "List.map String.fromInt ints"
                , "List.map String.reverse strings"
                ]
              )
            , ( a
              , []
              )
            , ( Can.list a
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
                        (Canonical.Annotation.fromType (Can.Lambda a a))
                        values
                    )
            )
            [ ( Can.int
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
            [ ( Can.string
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
                                  , Can.Lambda
                                        (Can.set a)
                                        (Can.list a)
                                  )
                                , ( "g"
                                  , Can.Lambda
                                        Can.string
                                        (Can.set Can.string)
                                  )
                                , ( "h", a )
                                ]
                        )
                )
                [ ( Can.list Can.string
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
                                  , Can.Lambda
                                        (Can.set Can.string)
                                        (Can.list Can.string)
                                  )
                                , ( "g", Can.Lambda a (Can.set a) )
                                , ( "h", Can.string )
                                ]
                        )
                )
                [ ( Can.list Can.string
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
            [ ( Can.Tuple [ Can.int, Can.int ]
              , [ "( int, int )" ]
              )
            , ( Can.Tuple [ a, a ]
              , []
              )
            , ( Can.Tuple [ a, Can.int ]
              , []
              )
            , ( Can.Tuple [ Can.int, a ]
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
                        (Canonical.Annotation.fromType (Can.Lambda a a))
                        values
                    )
            )
            [ ( Can.Tuple [ Can.int, Can.int ]
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
                            [ ( "int", Can.int )
                            , ( "float", Can.float )
                            , ( "listInt", Can.list Can.int )
                            ]
                    )
            )
            [ ( Can.Record
                    [ ( "int", Can.int )
                    , ( "float", Can.float )
                    , ( "listInt", Can.list Can.int )
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
            [ ( Can.Record
                    [ ( "int", Can.int )
                    , ( "string", Can.string )
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
            [ ( Can.int
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
            Canonical.Annotation.fromType Can.int
    in
    describe "all"
        [ describe "add and take"
            [ testGenerator "add one and take 1"
                (call []
                    |> addValues (Dict.singleton "int" int)
                    |> takeValues 1
                )
                [ ( Can.int
                  , [ "int" ]
                  )
                ]
            , testGenerator "add two and take 1"
                (call []
                    |> addValues (Dict.singleton "intA" int)
                    |> addValues (Dict.singleton "intB" int)
                    |> takeValues 1
                )
                [ ( Can.int
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
                [ ( Can.int
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
            [ ( Can.int
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
                [ ( Can.int
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
                [ ( Can.int
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
                [ ( Can.int
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
            [ ( Can.int
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
            [ ( Can.int
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
                            [ ( "int1", Can.int )
                            , ( "int2", Can.int )
                            ]
                    )
            )
            [ ( Can.int
              , [ "int1" ]
              )
            ]
        , testGenerator "only 2 in nested generator"
            (call
                [ firstN 2 value ]
                |> addValues
                    (Dict.map (\_ -> Canonical.Annotation.fromType) <|
                        Dict.fromList
                            [ ( "int1", Can.int )
                            , ( "int2", Can.int )
                            , ( "int3", Can.int )
                            , ( "func", Can.Lambda Can.int Can.int )
                            ]
                    )
            )
            [ ( Can.int
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
            [ ( Can.int
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
            [ ( Can.int
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


jsonDecoderTest : Test
jsonDecoderTest =
    describe "jsonDecoder"
        [ testGenerator "single value"
            (Generator.jsonDecoder
                |> Generator.addValues
                    (Dict.fromList
                        [ ( "Decode.int"
                          , Canonical.Annotation.fromType <|
                                Can.Type [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Can.int ]
                          )
                        ]
                    )
            )
            [ ( Can.Type [ "Json", "Decode" ] "Decoder" [ Can.int ]
              , [ "Decode.int" ]
              )
            ]
        , testGenerator "an aliased record"
            (Generator.jsonDecoder
                |> Generator.addValues
                    (Dict.fromList
                        [ ( "Decode.int"
                          , Canonical.Annotation.fromType <|
                                Can.Type [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Can.int ]
                          )
                        , ( "Decode.string"
                          , Canonical.Annotation.fromType <|
                                Can.Type [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Can.string ]
                          )
                        ]
                    )
             --|> Generator.addAliases
             --    (Dict.fromList
             --        [ ( "User"
             --          , { vars = []
             --            , tipe =
             --                Can.Record
             --                    [ ( "age", Can.int )
             --                    , ( "name", Can.string )
             --                    ]
             --                    Nothing
             --            }
             --          )
             --        ]
             --    )
            )
            [ ( Can.Type [ "Json", "Decode" ]
                    "Decoder"
                    [ Can.Record
                        [ ( "age", Can.int )
                        , ( "name", Can.string )
                        ]
                        Nothing
                    ]
              , [ "Decode.succeed User |> Decode.required \"age\" Decode.int |> Decode.required \"name\" Decode.string" ]
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
                for Can.int generatorA
                    |> List.map (exprToString False)
                    |> Expect.equal
                        (for Can.int generatorB
                            |> List.map (exprToString False)
                        )
        , test "List Int" <|
            \_ ->
                for (Can.list Can.int) generatorA
                    |> List.map (exprToString False)
                    |> Expect.equal
                        (for (Can.list Can.int) generatorB
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
            [ ( Can.int
              , []
              )
            ]
        , testGenerator "at 1"
            (call []
                |> addValues values
                |> takeValues 1
            )
            [ ( Can.int
              , [ "int" ]
              )
            ]
        , testGenerator "call value at 0"
            (call
                [ value
                    |> addValues
                        (Dict.singleton "newInt"
                            (Canonical.Annotation.fromType Can.int)
                        )
                    |> takeValues 1
                ]
                |> addValues values
            )
            [ ( Can.int
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
            [ ( Can.int
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
            [ ( Can.Record
                    [ ( "int", Can.int ) ]
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


a : Can.Type
a =
    Can.Var "a"


b : Can.Type
b =
    Can.Var "b"


msg : Can.Type
msg =
    Can.Type [ "Main" ] "Msg" []


values : Dict String Annotation
values =
    Dict.map (\_ -> Canonical.Annotation.fromType) <|
        Dict.fromList
            [ ( "int", Can.int )
            , ( "ints", Can.list Can.int )
            , ( "float", Can.float )
            , ( "string", Can.string )
            , ( "strings", Can.list Can.string )
            , ( "record"
              , Can.Record
                    [ ( "int", Can.int )
                    , ( "string", Can.string )
                    ]
                    Nothing
              )
            , ( "intRecord"
              , Can.Record
                    [ ( "int", Can.int ) ]
                    Nothing
              )
            , ( "msg", msg )

            -- FUNCTIONS WITH ONE ARGUMENT
            , ( "Basics.negate", Can.Lambda Can.int Can.int )
            , ( "String.reverse", Can.Lambda Can.string Can.string )
            , ( "String.fromInt", Can.Lambda Can.int Can.string )

            -- FUNCTIONS WITH TWO ARGUMENTS
            , ( "Basics.modBy"
              , Can.Lambda Can.int
                    (Can.Lambda Can.int
                        Can.int
                    )
              )
            , ( "List.map"
              , Can.Lambda (Can.Lambda a b)
                    (Can.Lambda
                        (Can.list a)
                        (Can.list b)
                    )
              )
            , ( "String.append"
              , Can.Lambda Can.string
                    (Can.Lambda
                        Can.string
                        Can.string
                    )
              )

            -- FUNCTIONS WITH THREE ARGUMENTS
            , ( "List.foldl"
              , Can.Lambda
                    (Can.Lambda a (Can.Lambda b b))
                    (Can.Lambda b (Can.Lambda (Can.list a) b))
              )
            ]


unions : List Union
unions =
    [ { name = "Msg"
      , tipe = Can.Type [ "Main" ] "Msg" []
      , constructors =
            [ { tag = "NewInt"
              , types = [ Can.int ]
              }
            , { tag = "NewFloat"
              , types = [ Can.float ]
              }
            , { tag = "NewString"
              , types = [ Can.string ]
              }
            ]
      }
    ]


testGenerator : String -> Generator -> List ( Can.Type, List String ) -> Test
testGenerator description generator tests =
    let
        testHelp ( targetType, expectation ) =
            test (typeToString targetType) <|
                \_ ->
                    generator
                        |> Generator.addUnions unions
                        |> for targetType
                        |> List.map (exprToString False)
                        |> Expect.equal expectation
    in
    describe description <|
        List.map testHelp tests


typeToString : Can.Type -> String
typeToString tipe =
    case tipe of
        Can.Var name ->
            name

        Can.Type moduleName name subTypes ->
            String.join " "
                (String.join "." (moduleName ++ [ name ])
                    :: List.map typeToString subTypes
                )

        Can.Lambda from to ->
            String.join " "
                [ typeToString from
                , "->"
                , typeToString to
                ]

        Can.Tuple subTypes ->
            String.join " "
                [ "("
                , String.join ", " (List.map typeToString subTypes)
                , ")"
                ]

        Can.Unit ->
            "()"

        Can.Record fields maybeVar ->
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
