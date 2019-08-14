module CanonicalTest exposing (suite)

import Canonical exposing (Associativity(..), Module, Type(..))
import Dict exposing (Dict)
import Elm.Interface
import Elm.Parser
import Elm.Processing
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)


suite : Test
suite =
    concat
        [ test "module without imports exposing everything" <|
            \_ ->
                canonicalizeModuleHelp Dict.empty
                    "Foo.Bar"
                    """module Foo.Bar exposing (..)

type Msg
    = NoOp
    | SomeOp


type alias AliasedMsg =
    Msg

func : Msg -> Msg
func msg =
    msg


infix left 6 (+) = add

add : number -> number -> number
add =
    Elm.Kernel.Basics.add
"""
                    |> Expect.equal
                        { exposed =
                            [ "add"
                            , "func"
                            , "Msg"
                            , "NoOp"
                            , "SomeOp"
                            , "AliasedMsg"
                            , "+"
                            ]
                        , values =
                            Dict.fromList
                                [ ( "add"
                                  , Lambda
                                        (Var "number")
                                        (Lambda
                                            (Var "number")
                                            (Var "number")
                                        )
                                  )
                                , ( "func"
                                  , Lambda
                                        (Type "Foo.Bar" "Msg" [])
                                        (Type "Foo.Bar" "Msg" [])
                                  )
                                ]
                        , unions =
                            Dict.fromList
                                [ ( "Msg"
                                  , { vars = []
                                    , constructors =
                                        Dict.fromList
                                            [ ( "NoOp", [] )
                                            , ( "SomeOp", [] )
                                            ]
                                    }
                                  )
                                ]
                        , aliases =
                            Dict.fromList
                                [ ( "AliasedMsg"
                                  , { vars = []
                                    , tipe = Type "Foo.Bar" "Msg" []
                                    }
                                  )
                                ]
                        , binops =
                            Dict.fromList
                                [ ( "+"
                                  , { function = "add"
                                    , tipe =
                                        Lambda
                                            (Var "number")
                                            (Lambda
                                                (Var "number")
                                                (Var "number")
                                            )
                                    , precedence = 6
                                    , associativity = Left
                                    }
                                  )
                                ]
                        }
        , test "module without imports exposing function" <|
            \_ ->
                canonicalizeModuleHelp Dict.empty
                    "Foo.Bar"
                    """module Foo.Bar exposing (func)

type Msg
    = NoOp
    | SomeOp


type alias AliasedMsg =
    Msg

func : Msg -> Msg
func msg =
    msg


infix left 6 (+) = add

add : number -> number -> number
add =
    Elm.Kernel.Basics.add
"""
                    |> .exposed
                    |> Expect.equal [ "func" ]
        , test "module without imports exposing opaque type" <|
            \_ ->
                canonicalizeModuleHelp Dict.empty
                    "Foo.Bar"
                    """module Foo.Bar exposing (Msg)

type Msg
    = NoOp
    | SomeOp


type alias AliasedMsg =
    Msg

func : Msg -> Msg
func msg =
    msg


infix left 6 (+) = add

add : number -> number -> number
add =
    Elm.Kernel.Basics.add
"""
                    |> .exposed
                    |> Expect.equal [ "Msg" ]
        , test "module without imports exposing type with constructors" <|
            \_ ->
                canonicalizeModuleHelp Dict.empty
                    "Foo.Bar"
                    """module Foo.Bar exposing (Msg(..))

type Msg
    = NoOp
    | SomeOp


type alias AliasedMsg =
    Msg

func : Msg -> Msg
func msg =
    msg


infix left 6 (+) = add

add : number -> number -> number
add =
    Elm.Kernel.Basics.add
"""
                    |> .exposed
                    |> Expect.equal [ "Msg", "NoOp", "SomeOp" ]
        , test "module without imports exposing type alias" <|
            \_ ->
                canonicalizeModuleHelp Dict.empty
                    "Foo.Bar"
                    """module Foo.Bar exposing (AliasedMsg)

type Msg
    = NoOp
    | SomeOp


type alias AliasedMsg =
    Msg

func : Msg -> Msg
func msg =
    msg


infix left 6 (+) = add

add : number -> number -> number
add =
    Elm.Kernel.Basics.add
"""
                    |> .exposed
                    |> Expect.equal [ "AliasedMsg" ]
        , test "module without imports exposing binop" <|
            \_ ->
                canonicalizeModuleHelp Dict.empty
                    "Foo.Bar"
                    """module Foo.Bar exposing ((+))

type Msg
    = NoOp
    | SomeOp


type alias AliasedMsg =
    Msg

func : Msg -> Msg
func msg =
    msg


infix left 6 (+) = add

add : number -> number -> number
add =
    Elm.Kernel.Basics.add
"""
                    |> .exposed
                    |> Expect.equal [ "+" ]
        ]


canonicalizeModuleHelp : Dict String Module -> String -> String -> Module
canonicalizeModuleHelp importedModules name src =
    case Elm.Parser.parse src of
        Err error ->
            Debug.todo ("Could not parse src:\n" ++ src)

        Ok rawFile ->
            let
                file =
                    Elm.Processing.process Elm.Processing.init rawFile

                interface =
                    Elm.Interface.build rawFile
            in
            Canonical.canonicalizeModule importedModules name file interface
