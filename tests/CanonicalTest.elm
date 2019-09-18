module CanonicalTest exposing (suite)

import Canonical
    exposing
        ( Associativity(..)
        , Constructor(..)
        , Module
        , ModuleData
        , Store
        )
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
import Fixtures.Char
import Fixtures.List
import Fixtures.Main
import Fixtures.Maybe
import Fixtures.Platform.Cmd
import Fixtures.Platform.Sub
import Fixtures.Result
import Fixtures.String
import Fuzz exposing (Fuzzer)
import Set exposing (Set)
import Test exposing (..)


suite : Test
suite =
    concat
        [ testBasics
        , testMaybe
        , testList
        , testMain
        ]


testBasics : Test
testBasics =
    test "Basics" <|
        \_ ->
            Canonical.emptyStore
                |> unsafeAdd
                    { moduleName = [ "Basics" ]
                    , fileName = "Basics.elm"
                    , file = Elm.Processing.process Elm.Processing.init rawBasics
                    , imports = Elm.RawFile.imports rawBasics
                    , interface = Elm.Interface.build rawBasics
                    }
                |> Expect.all
                    [ .todo >> Expect.equal []
                    , exposedTypes [ "Basics" ]
                        >> Expect.equal Fixtures.Basics.exposedTypes
                    , exposedConstructors [ "Basics" ]
                        >> Expect.equal Fixtures.Basics.exposedConstructors
                    , exposedValues [ "Basics" ]
                        >> Expect.equal Fixtures.Basics.exposedValues
                    ]


testMaybe : Test
testMaybe =
    test "Maybe" <|
        \_ ->
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
                |> Expect.all
                    [ .todo >> Expect.equal []
                    , exposedTypes [ "Maybe" ]
                        >> Expect.equal Fixtures.Maybe.exposedTypes
                    , exposedConstructors [ "Maybe" ]
                        >> Expect.equal Fixtures.Maybe.exposedConstructors
                    , exposedValues [ "Maybe" ]
                        >> Expect.equal Fixtures.Maybe.exposedValues
                    , values [ "Maybe" ]
                        >> Expect.equal Fixtures.Maybe.values
                    ]


testList : Test
testList =
    test "List" <|
        \_ ->
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
                |> Expect.all
                    [ .todo >> Expect.equal []
                    , exposedTypes [ "List" ]
                        >> Expect.equal Fixtures.List.exposedTypes
                    , exposedConstructors [ "List" ]
                        >> Expect.equal Fixtures.List.exposedConstructors
                    , exposedValues [ "List" ]
                        >> Expect.equal Fixtures.List.exposedValues
                    ]


testMain : Test
testMain =
    test "Main" <|
        \_ ->
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
                |> unsafeAdd
                    { moduleName = [ "Platform", "Cmd" ]
                    , fileName = "Platform/Cmd.elm"
                    , file = Elm.Processing.process Elm.Processing.init rawPlatformCmd
                    , imports = Elm.RawFile.imports rawPlatformCmd
                    , interface = Elm.Interface.build rawPlatformCmd
                    }
                |> unsafeAdd
                    { moduleName = [ "Platform", "Sub" ]
                    , fileName = "Platform/Sub.elm"
                    , file = Elm.Processing.process Elm.Processing.init rawPlatformSub
                    , imports = Elm.RawFile.imports rawPlatformSub
                    , interface = Elm.Interface.build rawPlatformSub
                    }
                |> unsafeAdd
                    { moduleName = [ "Char" ]
                    , fileName = "Char.elm"
                    , file = Elm.Processing.process Elm.Processing.init rawChar
                    , imports = Elm.RawFile.imports rawChar
                    , interface = Elm.Interface.build rawChar
                    }
                |> unsafeAdd
                    { moduleName = [ "Result" ]
                    , fileName = "Result.elm"
                    , file = Elm.Processing.process Elm.Processing.init rawResult
                    , imports = Elm.RawFile.imports rawResult
                    , interface = Elm.Interface.build rawResult
                    }
                |> unsafeAdd
                    { moduleName = [ "String" ]
                    , fileName = "String.elm"
                    , file = Elm.Processing.process Elm.Processing.init rawString
                    , imports = Elm.RawFile.imports rawString
                    , interface = Elm.Interface.build rawString
                    }
                |> addMock [ "Bitwise" ]
                |> addMock [ "Result" ]
                |> addMock [ "Char" ]
                |> addMock [ "Tuple" ]
                |> addMock [ "Debug" ]
                |> addMock [ "Platform" ]
                |> unsafeAdd
                    { moduleName = [ "Main" ]
                    , fileName = "Main.elm"
                    , file = Elm.Processing.process Elm.Processing.init rawMain
                    , imports = Elm.RawFile.imports rawMain
                    , interface = Elm.Interface.build rawMain
                    }
                |> Expect.all
                    [ .todo >> List.map .name >> Expect.equal []
                    , exposedTypes [ "Main" ]
                        >> Expect.equal Fixtures.Main.exposedTypes
                    , exposedConstructors [ "Main" ]
                        >> Expect.equal Fixtures.Main.exposedConstructors
                    , exposedValues [ "Main" ]
                        >> Expect.equal Fixtures.Main.exposedValues
                    , values [ "Main" ]
                        >> Expect.equal Fixtures.Main.values
                    , value [ "Main" ] "update"
                        >> Expect.equal
                            (ForAll [] <|
                                Lambda
                                    (Type [ "Main" ] "Msg" [])
                                    (Lambda
                                        (Record
                                            [ ( "users"
                                              , Type [ "List" ]
                                                    "List"
                                                    [ Record
                                                        [ ( "name", Type [ "String" ] "String" [] )
                                                        , ( "age", Type [ "Basics" ] "Int" [] )
                                                        ]
                                                        Nothing
                                                    ]
                                              )
                                            ]
                                            Nothing
                                        )
                                        (Tuple
                                            [ Record
                                                [ ( "users"
                                                  , Type [ "List" ]
                                                        "List"
                                                        [ Record
                                                            [ ( "name", Type [ "String" ] "String" [] )
                                                            , ( "age", Type [ "Basics" ] "Int" [] )
                                                            ]
                                                            Nothing
                                                        ]
                                                  )
                                                ]
                                                Nothing
                                            , Type [ "Platform", "Cmd" ]
                                                "Cmd"
                                                [ Type [ "Main" ] "Msg" [] ]
                                            ]
                                        )
                                    )
                            )
                    , qualifiedConstructor [ "Main" ] [ "Maybe" ] "Just"
                        >> Expect.equal
                            (Constructor "Maybe"
                                [ Var "a" ]
                                (Type [ "Maybe" ] "Maybe" [ Var "a" ])
                            )
                    , qualifiedConstructor [ "Main" ] [ "Maybe" ] "Nothing"
                        >> Expect.equal
                            (Constructor "Maybe"
                                []
                                (Type [ "Maybe" ] "Maybe" [ Var "a" ])
                            )
                    ]



---- HELPER


exposedTypes : ModuleName -> Store ModuleName ModuleData Module -> Set String
exposedTypes moduleName store =
    case Dict.get moduleName store.done of
        Nothing ->
            Debug.todo "no done module"

        Just module_ ->
            module_.exposedTypes
                |> Dict.keys
                |> Set.fromList


exposedConstructors : ModuleName -> Store ModuleName ModuleData Module -> Set String
exposedConstructors moduleName store =
    case Dict.get moduleName store.done of
        Nothing ->
            Debug.todo "no done module"

        Just module_ ->
            module_.exposedConstructors
                |> Dict.keys
                |> Set.fromList


exposedValues : ModuleName -> Store ModuleName ModuleData Module -> Set String
exposedValues moduleName store =
    case Dict.get moduleName store.done of
        Nothing ->
            Debug.todo "no done module"

        Just module_ ->
            module_.exposedValues
                |> Dict.keys
                |> Set.fromList


values : ModuleName -> Store ModuleName ModuleData Module -> Set String
values moduleName store =
    case Dict.get moduleName store.done of
        Nothing ->
            Debug.todo "no done module"

        Just module_ ->
            module_.values
                |> Dict.keys
                |> Set.fromList


value : ModuleName -> String -> Store ModuleName ModuleData Module -> Annotation
value moduleName name store =
    case Dict.get moduleName store.done of
        Nothing ->
            Debug.todo "no done module"

        Just module_ ->
            case Dict.get name module_.values of
                Nothing ->
                    Debug.todo "value not found"

                Just v ->
                    v


qualifiedConstructor :
    ModuleName
    -> ModuleName
    -> String
    -> Store ModuleName ModuleData Module
    -> Constructor
qualifiedConstructor moduleName qualifier name store =
    case Dict.get moduleName store.done of
        Nothing ->
            Debug.todo "no done module"

        Just module_ ->
            case Dict.get qualifier module_.qualifiedConstructors of
                Nothing ->
                    Debug.todo "qualifier not found"

                Just constructors ->
                    case Dict.get name constructors of
                        Nothing ->
                            Debug.todo "constructor not found"

                        Just constructor ->
                            constructor


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


rawPlatformCmd : RawFile
rawPlatformCmd =
    parse Fixtures.Platform.Cmd.src


rawPlatformSub : RawFile
rawPlatformSub =
    parse Fixtures.Platform.Sub.src


rawString : RawFile
rawString =
    parse Fixtures.String.src


rawChar : RawFile
rawChar =
    parse Fixtures.Char.src


rawResult : RawFile
rawResult =
    parse Fixtures.Result.src


rawMain : RawFile
rawMain =
    parse Fixtures.Main.src


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
