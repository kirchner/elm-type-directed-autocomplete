module SuggestTest exposing (suite)

import Dict exposing (Dict)
import Elm.Type exposing (Type(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Set
import Suggest
    exposing
        ( Generator
        , addValues
        , call
        , exprToString
        , for
        , value
        )
import Test exposing (..)


suite : Test
suite =
    concat
        [ valueTest
        , callTest
        ]


valueTest : Test
valueTest =
    describe "value"
        [ testGenerator value
            [ ( int
              , [ "int" ]
              )
            , ( Lambda int int
              , [ "Basics.identity"
                , "Basics.negate"
                ]
              )
            , ( Lambda a a
              , [ "Basics.identity"
                , "Basics.negate"
                , "String.reverse"
                ]
              )
            , ( Lambda a int
              , [ "Basics.identity"
                , "Basics.negate"
                ]
              )
            ]
        ]


callTest : Test
callTest =
    describe "call"
        [ describe "one value argument"
            [ testGenerator (call [ value ])
                [ ( int
                  , [ "Basics.identity int"
                    , "Basics.negate int"
                    ]
                  )
                ]
            ]
        , describe "two value arguments"
            [ testGenerator (call [ value, value ])
                [ ( int
                  , [ "Basics.modBy int int" ]
                  )
                , ( list string
                  , [ "List.map String.reverse strings"
                    , "List.map String.fromInt ints"
                    ]
                  )
                ]
            ]
        , describe "one value argument, one called argument"
            [ testGenerator (call [ value, call [ value ] ])
                [ ( int
                  , [ "Basics.modBy int (Basics.identity int)"
                    , "Basics.modBy int (Basics.negate int)"
                    ]
                  )
                ]
            ]
        , describe "three value arguments"
            [ testGenerator (call [ value, value, value ])
                [ ( string
                  , [ "List.foldl String.append string strings" ]
                  )
                ]
            ]
        ]


int =
    Type "Int" []


float =
    Type "Float" []


string =
    Type "String" []


a =
    Var "a"


b =
    Var "b"


list tipe =
    Type "List" [ tipe ]


testGenerator : Generator -> List ( Type, List String ) -> Test
testGenerator generator tests =
    let
        testHelp ( input, expectation ) =
            test (typeToString input) <|
                \_ ->
                    generator
                        |> addValues values
                        |> for input
                        |> List.map exprToString
                        |> Set.fromList
                        |> Expect.equal (Set.fromList expectation)

        values =
            Dict.fromList
                [ ( "int", int )
                , ( "ints", list int )
                , ( "float", float )
                , ( "string", string )
                , ( "strings", list string )

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
    in
    concat <|
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
