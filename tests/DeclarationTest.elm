module DeclarationTest exposing (suite)

import Declaration exposing (Declaration(..))
import Elm.Type exposing (Type(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)


suite : Test
suite =
    concat
        [ parseTest ]


parseTest : Test
parseTest =
    describe "parse"
        [ test "num : Int" <|
            \_ ->
                Declaration.parse "num : Int"
                    |> Expect.equal
                        [ Value "num" (Type "Int" []) ]
        , test "type alias MyInt = Int" <|
            \_ ->
                Declaration.parse "type alias MyInt = Int"
                    |> Expect.equal
                        [ TypeAlias "MyInt" [] (Type "Int" []) ]
        , test "type alias Model = { foo : Int, bar : Float }" <|
            \_ ->
                Declaration.parse "type alias Mode = { foo : Int, bar : Float }"
                    |> Expect.equal
                        [ TypeAlias "Mode"
                            []
                            (Record
                                [ ( "foo", Type "Int" [] )
                                , ( "bar", Type "Float" [] )
                                ]
                                Nothing
                            )
                        ]
        , test "type Bool = True | False" <|
            \_ ->
                Declaration.parse "type Bool = True | False"
                    |> Expect.equal
                        [ CustomType "Bool"
                            []
                            [ ( "True", [] )
                            , ( "False", [] )
                            ]
                        ]
        , test "type User = Guest | Registered String" <|
            \_ ->
                Declaration.parse "type User = Guest | Registered String"
                    |> Expect.equal
                        [ CustomType "User"
                            []
                            [ ( "Guest", [] )
                            , ( "Registered", [ Type "String" [] ] )
                            ]
                        ]
        , test "type Result err a = Err err | Ok a" <|
            \_ ->
                Declaration.parse "type Result err a = Err err | Ok a"
                    |> Expect.equal
                        [ CustomType "Result"
                            [ "err", "a" ]
                            [ ( "Err", [ Var "err" ] )
                            , ( "Ok", [ Var "a" ] )
                            ]
                        ]
        ]
