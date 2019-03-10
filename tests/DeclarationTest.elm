module DeclarationTest exposing (suite)

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
