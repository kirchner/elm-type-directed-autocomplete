module InferenceTest exposing (suite)

import Canonical exposing (Associativity(..), Binop, Module, ModuleData, Store)
import Canonical.Annotation exposing (Annotation(..))
import Canonical.Type exposing (Type(..))
import Dict exposing (Dict)
import Elm.Interface
import Elm.Parser
import Elm.Processing
import Elm.RawFile exposing (RawFile)
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range, emptyRange)
import Expect exposing (Expectation)
import Fixtures.Basics
import Fixtures.List
import Fixtures.Maybe
import Fuzz exposing (Fuzzer)
import Inference
import Src
import Test exposing (..)


suite : Test
suite =
    concat
        [ test "simple value" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : Float -> Int
bar num =
    foo
"""
                    , range =
                        { start = { column = 5, row = 3 }
                        , end = { column = 8, row = 3 }
                        }
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.int
                            , False
                            , Dict.fromList
                                [ ( "num", Canonical.Type.float ) ]
                            )
                        )
        , test "tuple" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : Int -> ( Float, Int )
bar num =
    ( foo, 123 )
"""
                    , range =
                        { start = { column = 7, row = 3 }
                        , end = { column = 10, row = 3 }
                        }
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.float
                            , False
                            , Dict.fromList
                                [ ( "num", Canonical.Type.int ) ]
                            )
                        )
        , test "infix operator" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : Int -> List Int
bar num =
    num :: foo
"""
                    , range =
                        { start = { column = 12, row = 3 }
                        , end = { column = 15, row = 3 }
                        }
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.list Canonical.Type.int
                            , True
                            , Dict.fromList
                                [ ( "num", Canonical.Type.int ) ]
                            )
                        )
        , test "two infix operators" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : Int -> Int -> Int
bar numA numB =
    numA + numB * foo
"""
                    , range =
                        { start = { column = 19, row = 3 }
                        , end = { column = 22, row = 3 }
                        }
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.int
                            , True
                            , Dict.fromList
                                [ ( "numA", Canonical.Type.int )
                                , ( "numB", Canonical.Type.int )
                                ]
                            )
                        )
        , test "function" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : Int -> Float
bar int =
    foo int
"""
                    , range =
                        { start = { column = 5, row = 3 }
                        , end = { column = 8, row = 3 }
                        }
                    }
                    |> Expect.equal
                        (Ok
                            ( Lambda
                                Canonical.Type.int
                                Canonical.Type.float
                            , True
                            , Dict.fromList
                                [ ( "int", Canonical.Type.int )
                                ]
                            )
                        )
        , test "pipe operator" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : Int -> Float
bar int =
    int
        |> foo
"""
                    , range =
                        { start = { column = 12, row = 4 }
                        , end = { column = 15, row = 4 }
                        }
                    }
                    |> Expect.equal
                        (Ok
                            ( Lambda
                                Canonical.Type.int
                                Canonical.Type.float
                            , True
                            , Dict.fromList
                                [ ( "int", Canonical.Type.int )
                                ]
                            )
                        )
        , test "two pipe operators" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : Int -> Float
bar int =
    int
        |> toFloat
        |> foo
"""
                    , range =
                        { start = { column = 12, row = 5 }
                        , end = { column = 15, row = 5 }
                        }
                    }
                    |> Expect.equal
                        (Ok
                            ( Lambda
                                Canonical.Type.float
                                Canonical.Type.float
                            , True
                            , Dict.fromList
                                [ ( "int", Canonical.Type.int )
                                ]
                            )
                        )
        , test "using one value twice" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : List Int -> List Float
bar ints =
    ints
        |> List.map toFloat
        |> List.map identity
        |> foo
"""
                    , range =
                        { start = { column = 12, row = 6 }
                        , end = { column = 15, row = 6 }
                        }
                    }
                    |> Expect.equal
                        (Ok
                            ( Lambda
                                (Canonical.Type.list Canonical.Type.float)
                                (Canonical.Type.list Canonical.Type.float)
                            , True
                            , Dict.fromList
                                [ ( "ints", Canonical.Type.list Canonical.Type.int )
                                ]
                            )
                        )
        , test "record" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : Int -> { name : Float, count : Int }
bar num =
    { name = foo
    , count = 1
    }
"""
                    , range =
                        { start = { column = 14, row = 3 }
                        , end = { column = 17, row = 3 }
                        }
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.float
                            , False
                            , Dict.fromList
                                [ ( "num", Canonical.Type.int ) ]
                            )
                        )
        , test "record update" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : { name : Float, count : Int } -> { name : Float, count : Int }
bar data =
    { data | name = foo }
"""
                    , range =
                        { start = { column = 21, row = 3 }
                        , end = { column = 24, row = 3 }
                        }
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.float
                            , False
                            , Dict.fromList
                                [ ( "data"
                                  , Record
                                        [ ( "name", Canonical.Type.float )
                                        , ( "count", Canonical.Type.int )
                                        ]
                                        Nothing
                                  )
                                ]
                            )
                        )
        , test "case" <|
            \_ ->
                inferHelp
                    { src =
                        """foo : Int -> Float -> ( Int, Float )
foo int =
    \\float ->
        case int of
            0 ->
                ( 1, bar )

            _ ->
                ( 2, 0.5 )
"""
                    , range =
                        { start = { column = 22, row = 6 }
                        , end = { column = 25, row = 6 }
                        }
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.float
                            , False
                            , Dict.fromList
                                [ ( "int", Canonical.Type.int )
                                , ( "float", Canonical.Type.float )
                                ]
                            )
                        )
        , test "if" <|
            \_ ->
                inferHelp
                    { src =
                        """foo : Bool -> Float
foo bool =
    if bool then
        bar
    else
        0.5
"""
                    , range =
                        { start = { column = 9, row = 4 }
                        , end = { column = 12, row = 4 }
                        }
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.float
                            , False
                            , Dict.fromList
                                [ ( "bool", Canonical.Type.bool ) ]
                            )
                        )
        , test "with known value" <|
            \_ ->
                inferHelp
                    { src =
                        """foo : Int -> List Float
foo int =
    List.repeat int bar
"""
                    , range =
                        { start = { column = 21, row = 3 }
                        , end = { column = 24, row = 3 }
                        }
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.float
                            , True
                            , Dict.fromList
                                [ ( "int", Canonical.Type.int ) ]
                            )
                        )
        , test "record accessor" <|
            \_ ->
                inferHelp
                    { src =
                        """update : Maybe Int -> { count : Int } -> { count : Int }
update maybeInt model =
    case maybeInt of
        Nothing ->
            { model | count = model.count }

        Just int ->
            foo
"""
                    , range =
                        { start = { column = 13, row = 8 }
                        , end = { column = 16, row = 8 }
                        }
                    }
                    |> Expect.equal
                        (Ok
                            ( Record
                                [ ( "count", Canonical.Type.int ) ]
                                Nothing
                            , False
                            , Dict.fromList
                                [ ( "int", Canonical.Type.int )
                                , ( "maybeInt", Canonical.Type.maybe Canonical.Type.int )
                                , ( "model", Record [ ( "count", Canonical.Type.int ) ] Nothing )
                                ]
                            )
                        )
        , test "empty list pattern" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : List Int -> Float
bar nums =
    case nums of
        [] ->
            foo
"""
                    , range =
                        { start = { column = 13, row = 5 }
                        , end = { column = 16, row = 5 }
                        }
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.float
                            , False
                            , Dict.fromList
                                [ ( "nums", Canonical.Type.list Canonical.Type.int )
                                ]
                            )
                        )
        , test "uncons pattern" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : List Int -> Float
bar nums =
    case nums of
        first :: rest ->
            foo
"""
                    , range =
                        { start = { column = 13, row = 5 }
                        , end = { column = 16, row = 5 }
                        }
                    }
                    |> Expect.equal
                        (Ok
                            ( Canonical.Type.float
                            , False
                            , Dict.fromList
                                [ ( "first", Canonical.Type.int )
                                , ( "nums", Canonical.Type.list Canonical.Type.int )
                                , ( "rest", Canonical.Type.list Canonical.Type.int )
                                ]
                            )
                        )
        , test "record pattern" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : { count : Int } -> Float
bar { count } =
    foo count
"""
                    , range =
                        { start = { column = 5, row = 3 }
                        , end = { column = 8, row = 3 }
                        }
                    }
                    |> Expect.equal
                        (Ok
                            ( Lambda Canonical.Type.int Canonical.Type.float
                            , True
                            , Dict.fromList
                                [ ( "count", Canonical.Type.int )
                                ]
                            )
                        )
        , test "as pattern" <|
            \_ ->
                inferHelp
                    { src =
                        """bar : { count : Int } -> Float
bar ({ count } as stuff) =
    foo stuff count
"""
                    , range =
                        { start = { column = 5, row = 3 }
                        , end = { column = 8, row = 3 }
                        }
                    }
                    |> Expect.equal
                        (Ok
                            ( Lambda
                                (Record
                                    [ ( "count", Canonical.Type.int ) ]
                                    Nothing
                                )
                                (Lambda
                                    Canonical.Type.int
                                    Canonical.Type.float
                                )
                            , True
                            , Dict.fromList
                                [ ( "count", Canonical.Type.int )
                                , ( "stuff"
                                  , Record
                                        [ ( "count", Canonical.Type.int ) ]
                                        Nothing
                                  )
                                ]
                            )
                        )
        , test "let binding" <|
            \_ ->
                inferHelp
                    { src = """bar : Int -> Float
bar int =
    let
        foo =
            1.0
    in
    foo + toFloat int
"""
                    , range =
                        { start = { column = 11, row = 7 }
                        , end = { column = 18, row = 7 }
                        }
                    }
                    |> Expect.equal
                        (Ok
                            ( Lambda
                                Canonical.Type.int
                                Canonical.Type.float
                            , True
                            , Dict.fromList
                                [ ( "int", Canonical.Type.int )
                                , ( "foo", Canonical.Type.float )
                                ]
                            )
                        )
        ]


inferHelp :
    { src : String
    , range : Range
    }
    -> Result Inference.Error ( Type, Bool, Dict String Type )
inferHelp { src, range } =
    let
        actualSrc =
            "module Main exposing (..)\n" ++ src
    in
    case Elm.Parser.parse actualSrc of
        Err error ->
            Debug.todo ("Could not parse src:\n" ++ actualSrc)

        Ok rawFile ->
            let
                file =
                    Elm.Processing.process Elm.Processing.init rawFile

                actualRange =
                    { start =
                        { column = range.start.column
                        , row = range.start.row + 1
                        }
                    , end =
                        { column = range.end.column
                        , row = range.end.row + 1
                        }
                    }

                actualStore =
                    unsafeAdd
                        { moduleName = [ "Main" ]
                        , fileName = "Main.elm"
                        , file = file
                        , imports = Elm.RawFile.imports rawFile
                        , interface = Elm.Interface.build rawFile
                        }
                        store
            in
            case Src.functionDeclarationAt actualRange file of
                Nothing ->
                    Debug.todo "No function declaration at the specified range"

                Just function ->
                    case Dict.get [ "Main" ] actualStore.done of
                        Nothing ->
                            Debug.todo "Main module is not done"

                        Just mainModule ->
                            Inference.inferHole
                                { function = function
                                , range = actualRange
                                , moduleName = [ "Main" ]
                                , currentModule = mainModule
                                }



---- FIXTURES


store : Store ModuleName ModuleData Module
store =
    Canonical.emptyStore
        |> unsafeAdd
            { moduleName = [ "Basics" ]
            , fileName = "Basics.elm"
            , file = Elm.Processing.process Elm.Processing.init rawBasics
            , imports = Elm.RawFile.imports rawBasics
            , interface = Elm.Interface.build rawBasics
            }
        |> unsafeAdd
            { moduleName = [ "Maybe" ]
            , fileName = "Maybe.elm"
            , file = Elm.Processing.process Elm.Processing.init rawMaybe
            , imports = Elm.RawFile.imports rawMaybe
            , interface = Elm.Interface.build rawMaybe
            }
        |> unsafeAdd
            { moduleName = [ "List" ]
            , fileName = "List.elm"
            , file = Elm.Processing.process Elm.Processing.init rawList
            , imports = Elm.RawFile.imports rawList
            , interface = Elm.Interface.build rawList
            }
        |> addMock [ "Result" ]
        |> addMock [ "String" ]
        |> addMock [ "Char" ]
        |> addMock [ "Tuple" ]
        |> addMock [ "Debug" ]
        |> addMock [ "Platform" ]
        |> addMock [ "Platform", "Sub" ]
        |> addMock [ "Platform", "Cmd" ]


rawBasics : RawFile
rawBasics =
    parse Fixtures.Basics.src


rawList : RawFile
rawList =
    parse Fixtures.List.src


rawMaybe : RawFile
rawMaybe =
    parse Fixtures.Maybe.src


parse : String -> RawFile
parse src =
    case Elm.Parser.parse src of
        Err error ->
            Debug.todo ("Could not parse src:\n" ++ src)

        Ok rawFile ->
            rawFile


rawMock : ModuleName -> RawFile
rawMock moduleName =
    parse <|
        String.concat
            [ "module "
            , String.join "." moduleName
            , " exposing ()\n"
            ]


addMock :
    ModuleName
    -> Store ModuleName ModuleData Module
    -> Store ModuleName ModuleData Module
addMock moduleName =
    let
        rawFile =
            rawMock moduleName
    in
    unsafeAdd
        { moduleName = moduleName
        , fileName = String.join "/" moduleName ++ ".elm"
        , file =
            Elm.Processing.process Elm.Processing.init rawFile
        , imports = Elm.RawFile.imports rawFile
        , interface = Elm.Interface.build rawFile
        }


unsafeAdd data currentStore =
    let
        config =
            { required = Canonical.requiredModules
            , process =
                \done moduleData ->
                    Canonical.canonicalizeModule
                        done
                        moduleData.moduleName
                        moduleData.file
                        moduleData.interface
            }
    in
    case Canonical.add config data.moduleName data currentStore of
        Err error ->
            Debug.todo (Canonical.errorToString error)

        Ok newStore ->
            newStore
