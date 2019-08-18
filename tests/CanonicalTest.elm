module CanonicalTest exposing (suite)

import Canonical exposing (Associativity(..), Module, Store)
import Canonical.Annotation exposing (Annotation(..))
import Canonical.Type exposing (Type(..))
import Dict exposing (Dict)
import Elm.Interface
import Elm.Parser
import Elm.Processing
import Elm.RawFile exposing (RawFile)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Expect exposing (Expectation)
import Fixtures.Basics
import Fixtures.List
import Fixtures.Maybe
import Fuzz exposing (Fuzzer)
import Set exposing (Set)
import Test exposing (..)


suite : Test
suite =
    concat
        [ testBasics
        , testMaybe
        , testList
        ]


testBasics : Test
testBasics =
    test "Basics" <|
        \_ ->
            Canonical.emptyStore
                |> Canonical.add
                    { moduleName = [ "Basics" ]
                    , fileName = "Basics.elm"
                    , file = Elm.Processing.process Elm.Processing.init rawBasics
                    , imports = Elm.RawFile.imports rawBasics
                    , interface = Elm.Interface.build rawBasics
                    }
                |> Expect.all
                    [ .todo >> Expect.equal []
                    , exposedValues [ "Basics" ]
                        >> Expect.equal Fixtures.Basics.exposedValues
                    , exposedUnions [ "Basics" ]
                        >> Expect.equal Fixtures.Basics.exposedUnions
                    ]


testMaybe : Test
testMaybe =
    test "Maybe" <|
        \_ ->
            Canonical.emptyStore
                |> Canonical.add
                    { moduleName = [ "Basics" ]
                    , fileName = "Basics.elm"
                    , file = Elm.Processing.process Elm.Processing.init rawBasics
                    , imports = Elm.RawFile.imports rawBasics
                    , interface = Elm.Interface.build rawBasics
                    }
                |> Canonical.add
                    { moduleName = [ "Maybe" ]
                    , fileName = "Maybe.elm"
                    , file = Elm.Processing.process Elm.Processing.init rawMaybe
                    , imports = Elm.RawFile.imports rawMaybe
                    , interface = Elm.Interface.build rawMaybe
                    }
                |> Expect.all
                    [ .todo >> Expect.equal []
                    , exposedValues [ "Maybe" ]
                        >> Expect.equal Fixtures.Maybe.exposedValues
                    , exposedUnions [ "Maybe" ]
                        >> Expect.equal Fixtures.Maybe.exposedUnions
                    , values [ "Maybe" ]
                        >> Expect.equal Fixtures.Maybe.values
                    ]


testList : Test
testList =
    test "List" <|
        \_ ->
            Canonical.emptyStore
                |> Canonical.add
                    { moduleName = [ "Basics" ]
                    , fileName = "Basics.elm"
                    , file = Elm.Processing.process Elm.Processing.init rawBasics
                    , imports = Elm.RawFile.imports rawBasics
                    , interface = Elm.Interface.build rawBasics
                    }
                |> Canonical.add
                    { moduleName = [ "Maybe" ]
                    , fileName = "Maybe.elm"
                    , file = Elm.Processing.process Elm.Processing.init rawMaybe
                    , imports = Elm.RawFile.imports rawMaybe
                    , interface = Elm.Interface.build rawMaybe
                    }
                |> Canonical.add
                    { moduleName = [ "List" ]
                    , fileName = "List.elm"
                    , file = Elm.Processing.process Elm.Processing.init rawList
                    , imports = Elm.RawFile.imports rawList
                    , interface = Elm.Interface.build rawList
                    }
                |> Expect.all
                    [ .todo >> Expect.equal []
                    , exposedValues [ "List" ]
                        >> Expect.equal Fixtures.List.exposedValues
                    , exposedUnions [ "List" ]
                        >> Expect.equal Fixtures.List.exposedUnions
                    ]



---- HELPER


exposedValues : ModuleName -> Store -> Set String
exposedValues moduleName store =
    case Dict.get moduleName store.done of
        Nothing ->
            Debug.todo "no done module"

        Just module_ ->
            module_.exposedValues
                |> Dict.keys
                |> Set.fromList


exposedUnions : ModuleName -> Store -> Set String
exposedUnions moduleName store =
    case Dict.get moduleName store.done of
        Nothing ->
            Debug.todo "no done module"

        Just module_ ->
            module_.exposedUnions
                |> Dict.keys
                |> Set.fromList


values : ModuleName -> Store -> Set String
values moduleName store =
    case Dict.get moduleName store.done of
        Nothing ->
            Debug.todo "no done module"

        Just module_ ->
            module_.values
                |> Dict.keys
                |> Set.fromList


parse : String -> RawFile
parse src =
    case Elm.Parser.parse src of
        Err error ->
            Debug.todo ("Could not parse src:\n" ++ src)

        Ok rawFile ->
            rawFile



---- FIXTURES


rawBasics : RawFile
rawBasics =
    parse Fixtures.Basics.src


rawList : RawFile
rawList =
    parse Fixtures.List.src


rawMaybe : RawFile
rawMaybe =
    parse Fixtures.Maybe.src
