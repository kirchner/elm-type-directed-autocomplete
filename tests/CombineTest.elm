module CombineTest exposing (suite)

import Combine exposing (..)
import Expect
import Test exposing (..)


suite : Test
suite =
    concat
        [ combineTest
        ]


combineTest : Test
combineTest =
    let
        data =
            [ ( [ [ 1, 2, 3 ]
                , [ 4, 5, 6 ]
                ]
              , [ [ 1, 4 ]
                , [ 1, 5 ]
                , [ 1, 6 ]
                , [ 2, 4 ]
                , [ 2, 5 ]
                , [ 2, 6 ]
                , [ 3, 4 ]
                , [ 3, 5 ]
                , [ 3, 6 ]
                ]
              )
            , ( [ [ 1, 2, 3 ]
                , [ 4, 5, 6 ]
                , [ 7, 8, 9 ]
                ]
              , [ [ 1, 4, 7 ]
                , [ 1, 4, 8 ]
                , [ 1, 4, 9 ]
                , [ 1, 5, 7 ]
                , [ 1, 5, 8 ]
                , [ 1, 5, 9 ]
                , [ 1, 6, 7 ]
                , [ 1, 6, 8 ]
                , [ 1, 6, 9 ]
                , [ 2, 4, 7 ]
                , [ 2, 4, 8 ]
                , [ 2, 4, 9 ]
                , [ 2, 5, 7 ]
                , [ 2, 5, 8 ]
                , [ 2, 5, 9 ]
                , [ 2, 6, 7 ]
                , [ 2, 6, 8 ]
                , [ 2, 6, 9 ]
                , [ 3, 4, 7 ]
                , [ 3, 4, 8 ]
                , [ 3, 4, 9 ]
                , [ 3, 5, 7 ]
                , [ 3, 5, 8 ]
                , [ 3, 5, 9 ]
                , [ 3, 6, 7 ]
                , [ 3, 6, 8 ]
                , [ 3, 6, 9 ]
                ]
              )
            , ( [ [ 1, 2, 3 ]
                , [ 4, 5 ]
                , [ 6 ]
                ]
              , [ [ 1, 4, 6 ]
                , [ 1, 5, 6 ]
                , [ 2, 4, 6 ]
                , [ 2, 5, 6 ]
                , [ 3, 4, 6 ]
                , [ 3, 5, 6 ]
                ]
              )
            , ( [ [ 1, 2, 3 ]
                , [ 4 ]
                , [ 5, 6 ]
                ]
              , [ [ 1, 4, 5 ]
                , [ 1, 4, 6 ]
                , [ 2, 4, 5 ]
                , [ 2, 4, 6 ]
                , [ 3, 4, 5 ]
                , [ 3, 4, 6 ]
                ]
              )
            , ( [ [ 1 ]
                , [ 2, 3, 4, 5, 6, 7 ]
                ]
              , [ [ 1, 2 ]
                , [ 1, 3 ]
                , [ 1, 4 ]
                , [ 1, 5 ]
                , [ 1, 6 ]
                , [ 1, 7 ]
                ]
              )
            , ( [ [ 1, 2, 3, 4, 5, 6, 7 ] ]
              , [ [ 1 ]
                , [ 2 ]
                , [ 3 ]
                , [ 4 ]
                , [ 5 ]
                , [ 6 ]
                , [ 7 ]
                ]
              )
            , ( [ [ 1, 2 ]
                , []
                , [ 3, 4 ]
                ]
              , []
              )
            ]

        inputToString input =
            String.concat
                [ "[ "
                , String.join ", " <|
                    List.map
                        (\list ->
                            String.concat
                                [ "[ "
                                , String.join ", " (List.map String.fromInt list)
                                , " ]"
                                ]
                        )
                        input
                , " ]"
                ]
    in
    describe "combine" <|
        List.map
            (\( input, output ) ->
                let
                    maxLength =
                        List.foldl (*) 1 (List.map List.length input)
                in
                concat
                    [ describe ("with input == " ++ inputToString input)
                        (List.range 0 maxLength
                            |> List.map
                                (\limit ->
                                    test ("with limit == " ++ String.fromInt limit) <|
                                        \_ ->
                                            input
                                                |> combine (Just limit)
                                                |> Expect.equal
                                                    (List.take limit output)
                                )
                        )
                    , test ("with input == " ++ inputToString input ++ " and no limit") <|
                        \_ ->
                            input
                                |> combine Nothing
                                |> Expect.equal output
                    ]
            )
            data
