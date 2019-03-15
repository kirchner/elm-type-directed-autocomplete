module SuggestTest exposing (suite)

import Dict exposing (Dict)
import Elm.Docs exposing (Union)
import Elm.Type exposing (Type(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Set
import Suggest
    exposing
        ( Generator
        , addUnions
        , addValues
        , all
        , call
        , cases
        , exprToString
        , for
        , recordUpdate
        , takeValues
        , tuple
        , value
        )
import Test exposing (..)


suite : Test
suite =
    concat
        [ describe "generators"
            [ valueTest
            , callTest
            , tupleTest
            , recordUpdateTest
            , casesTest
            , allTest
            ]
        , takeValuesTest
        ]


valueTest : Test
valueTest =
    testGenerator "value"
        (value
            |> addValues values
        )
        [ ( int
          , [ "int" ]
          )
        , ( Lambda int int
          , [ "Basics.negate"
            , "Basics.identity"
            ]
          )
        , ( Lambda a a
          , [ "Basics.identity" ]
          )
        , ( Lambda a int
          , []
          )
        ]


callTest : Test
callTest =
    describe "call"
        [ testGenerator "value"
            (call [ value ]
                |> addValues values
            )
            [ ( int
              , [ "Basics.negate int"
                , "Basics.identity int"
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
            [ ( int
              , [ "Basics.modBy int int" ]
              )
            , ( list string
              , [ "List.map String.reverse strings"
                , "List.map String.fromInt ints"
                ]
              )
            , ( a
              , []
              )
            ]
        , testGenerator "value (call value)"
            (call
                [ value
                , call [ value ]
                ]
                |> addValues values
            )
            [ ( int
              , [ "Basics.modBy int (Basics.negate int)"
                , "Basics.modBy int (Basics.identity int)"
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
            [ ( string
              , [ "List.foldl String.append string strings" ]
              )
            , ( a
              , []
              )
            ]
        ]


tupleTest : Test
tupleTest =
    describe "tuple"
        [ testGenerator "( value, value )"
            (tuple
                { first = value
                , second = value
                }
                |> addValues values
            )
            [ ( Tuple [ int, int ]
              , [ "( int, int )" ]
              )
            , ( Tuple [ a, a ]
              , []
              )
            , ( Tuple [ a, int ]
              , []
              )
            , ( Tuple [ int, a ]
              , []
              )
            ]
        , testGenerator "( value, call value )"
            (tuple
                { first = value
                , second = call [ value ]
                }
                |> addValues values
            )
            [ ( Tuple [ int, int ]
              , [ "( int, Basics.negate int )"
                , "( int, Basics.identity int )"
                ]
              )
            ]
        ]


recordUpdateTest : Test
recordUpdateTest =
    describe "recordUpdate"
        [ testGenerator "value"
            (recordUpdate value
                |> addValues values
            )
            [ ( record
                    [ ( "int", int )
                    , ( "string", string )
                    ]
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
                { matched = value
                , branch =
                    \newValues ->
                        value
                            |> addValues newValues
                }
                |> addValues values
            )
            [ ( int
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
    describe "all"
        [ describe "add and take"
            [ testGenerator "add one and take 1"
                (value
                    |> addValues (Dict.singleton "int" int)
                    |> takeValues 1
                )
                [ ( int
                  , [ "int" ]
                  )
                ]
            , testGenerator "add two and take 1"
                (value
                    |> addValues (Dict.singleton "intA" int)
                    |> addValues (Dict.singleton "intB" int)
                    |> takeValues 1
                )
                [ ( int
                  , [ "intB" ]
                  )
                ]
            , testGenerator "add two, take 1 and add one"
                (value
                    |> addValues (Dict.singleton "intA" int)
                    |> addValues (Dict.singleton "intB" int)
                    |> takeValues 1
                    |> addValues (Dict.singleton "intC" int)
                )
                [ ( int
                  , [ "intC"
                    , "intB"
                    ]
                  )
                ]
            ]
        , testGenerator "nested three times"
            (all
                [ all
                    [ value
                        |> addValues (Dict.singleton "intC" int)
                    ]
                    |> addValues (Dict.singleton "intB" int)
                ]
                |> addValues (Dict.singleton "intA" int)
            )
            [ ( int
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
                        [ value
                            |> addValues (Dict.singleton "intC" int)
                            |> takeValues 1
                        ]
                        |> addValues (Dict.singleton "intB" int)
                    ]
                    |> addValues (Dict.singleton "intA" int)
                )
                [ ( int
                  , [ "intC" ]
                  )
                ]
            , testGenerator "middle level"
                (all
                    [ all
                        [ value
                            |> addValues (Dict.singleton "intC" int)
                        ]
                        |> addValues (Dict.singleton "intB" int)
                        |> takeValues 1
                    ]
                    |> addValues (Dict.singleton "intA" int)
                )
                [ ( int
                  , [ "intC"
                    , "intB"
                    ]
                  )
                ]
            , testGenerator "outermost level"
                (all
                    [ all
                        [ value
                            |> addValues (Dict.singleton "intC" int)
                        ]
                        |> addValues (Dict.singleton "intB" int)
                    ]
                    |> addValues (Dict.singleton "intA" int)
                    |> takeValues 1
                )
                [ ( int
                  , [ "intC"
                    , "intB"
                    , "intA"
                    ]
                  )
                ]
            ]
        , testGenerator "three times addValues"
            (value
                |> addValues (Dict.singleton "intA" int)
                |> addValues (Dict.singleton "intB" int)
                |> addValues (Dict.singleton "intC" int)
            )
            [ ( int
              , [ "intC"
                , "intB"
                , "intA"
                ]
              )
            ]
        , testGenerator "three times addValues with takeValues 1"
            (value
                |> addValues (Dict.singleton "intA" int)
                |> addValues (Dict.singleton "intB" int)
                |> addValues (Dict.singleton "intC" int)
                |> takeValues 1
            )
            [ ( int
              , [ "intC" ]
              )
            ]
        ]


takeValuesTest : Test
takeValuesTest =
    describe "takeValues"
        [ testGenerator "at 0"
            (value
                |> addValues values
                |> takeValues 0
            )
            [ ( int
              , []
              )
            ]
        , testGenerator "at 1"
            (value
                |> addValues values
                |> takeValues 1
            )
            [ ( int
              , [ "int" ]
              )
            ]
        , testGenerator "call value at 0"
            (call
                [ value
                    |> addValues (Dict.singleton "newInt" int)
                    |> takeValues 1
                ]
                |> addValues values
            )
            [ ( int
              , [ "Basics.negate newInt"
                , "Basics.identity newInt"
                ]
              )
            ]
        , testGenerator "cases"
            (cases
                { matched = value
                , branch =
                    \newValues ->
                        all
                            [ value
                                |> addValues newValues
                                |> takeValues 1
                            , value
                            ]
                }
                |> addValues values
            )
            [ ( int
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
                { matched = value
                , branch =
                    \newValues ->
                        recordUpdate
                            (value
                                |> addValues newValues
                                |> takeValues 1
                            )
                }
                |> addValues values
            )
            [ ( record
                    [ ( "int", int )
                    , ( "string", string )
                    ]
              , []
              )
            ]
        ]


int : Type
int =
    Type "Int" []


float : Type
float =
    Type "Float" []


string : Type
string =
    Type "String" []


a : Type
a =
    Var "a"


b : Type
b =
    Var "b"


list : Type -> Type
list tipe =
    Type "List" [ tipe ]


record : List ( String, Type ) -> Type
record fields =
    Record fields Nothing


msg : Type
msg =
    Type "Msg" []


values : Dict String Type
values =
    Dict.fromList
        [ ( "int", int )
        , ( "ints", list int )
        , ( "float", float )
        , ( "string", string )
        , ( "strings", list string )
        , ( "record"
          , record
                [ ( "int", int )
                , ( "string", string )
                ]
          )
        , ( "msg", msg )

        -- FUNCTIONS WITH ONE ARGUMENT
        , ( "Basics.negate", Lambda int int )
        , ( "String.reverse", Lambda string string )
        , ( "Basics.identity", Lambda a a )
        , ( "String.fromInt", Lambda int string )

        -- FUNCTIONS WITH TWO ARGUMENTS
        , ( "Basics.modBy", Lambda int (Lambda int int) )
        , ( "List.map", Lambda (Lambda a b) (Lambda (list a) (list b)) )
        , ( "String.append", Lambda string (Lambda string string) )

        -- FUNCTIONS WITH THREE ARGUMENTS
        , ( "List.foldl"
          , Lambda
                (Lambda a (Lambda b b))
                (Lambda b (Lambda (list a) b))
          )
        ]


unions : List Union
unions =
    [ { name = "Msg"
      , comment = ""
      , args = []
      , tags =
            [ ( "NewInt", [ int ] )
            , ( "NewFloat", [ float ] )
            , ( "NewString", [ string ] )
            ]
      }
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
                        |> List.map exprToString
                        |> Expect.equal expectation
    in
    describe description <|
        List.map testHelp tests


typeToString : Type -> String
typeToString tipe =
    case tipe of
        Var name ->
            name

        Type name subTypes ->
            String.join " " (name :: List.map typeToString subTypes)

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
