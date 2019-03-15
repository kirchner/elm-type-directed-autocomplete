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
        , call
        , cases
        , exprToString
        , for
        , recordUpdate
        , tuple
        , value
        )
import Test exposing (..)


suite : Test
suite =
    concat
        [ valueTest
        , callTest
        , tupleTest
        , recordUpdateTest
        , casesTest
        ]


valueTest : Test
valueTest =
    testGenerator "value" value <|
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


callTest : Test
callTest =
    describe "call"
        [ testGenerator "value"
            (call [ value ])
            [ ( int
              , [ "Basics.identity int"
                , "Basics.negate int"
                ]
              )
            ]
        , testGenerator "value value"
            (call
                [ value
                , value
                ]
            )
            [ ( int
              , [ "Basics.modBy int int" ]
              )
            , ( list string
              , [ "List.map String.reverse strings"
                , "List.map String.fromInt ints"
                ]
              )
            ]
        , testGenerator "value (call value)"
            (call
                [ value
                , call [ value ]
                ]
            )
            [ ( int
              , [ "Basics.modBy int (Basics.identity int)"
                , "Basics.modBy int (Basics.negate int)"
                ]
              )
            ]
        , testGenerator "value value value"
            (call
                [ value
                , value
                , value
                ]
            )
            [ ( string
              , [ "List.foldl String.append string strings" ]
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
            )
            [ ( Tuple [ int, int ]
              , [ "( int, int )" ]
              )
            ]
        , testGenerator "( value, call value )"
            (tuple
                { first = value
                , second = call [ value ]
                }
            )
            [ ( Tuple [ int, int ]
              , [ "( int, Basics.identity int )"
                , "( int, Basics.negate int )"
                ]
              )
            ]
        ]


recordUpdateTest : Test
recordUpdateTest =
    describe "recordUpdate"
        [ testGenerator "value"
            (recordUpdate value)
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
            )
            [ ( int
              , [ """case msg of
    NewInt newInt ->
        int

    NewFloat newFloat ->
        int

    NewString newString ->
        int"""
                , """case msg of
    NewInt newInt ->
        newInt

    NewFloat newFloat ->
        int

    NewString newString ->
        int"""
                ]
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
                        |> addValues values
                        |> addUnions unions
                        |> for targetType
                        |> List.map exprToString
                        |> Set.fromList
                        |> Expect.equal (Set.fromList expectation)
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
