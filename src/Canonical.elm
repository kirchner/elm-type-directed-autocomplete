module Canonical exposing
    ( Alias
    , Associativity(..)
    , Binop
    , Imports
    , Module
    , ModuleData
    , Store
    , Union
    , add
    , canonicalizeModule
    , canonicalizeTypeAnnotation
    , collectImports
    , emptyStore
    , replace
    )

import Canonical.Annotation exposing (Annotation(..))
import Canonical.Type exposing (Type(..))
import Dict exposing (Dict)
import Elm.Interface exposing (Interface)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix as Src
import Elm.Syntax.Module
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (emptyRange)
import Elm.Syntax.TypeAnnotation as Src
import List.Extra as List
import Set exposing (Set)



---- MODULE


type alias Module =
    { imports : Imports
    , exposed : List String
    , values : Dict String Type
    , unions : Dict String Union
    , aliases : Dict String Alias
    , binops : Dict String Binop
    }


type alias Union =
    { vars : List String
    , constructors : Dict String (List Type)
    }


type alias Alias =
    { vars : List String
    , tipe : Type
    }


type alias Binop =
    { function : String
    , tipe : Annotation
    , precedence : Int
    , associativity : Associativity
    }


type Associativity
    = Left
    | Non
    | Right



---- IMPORTS


type alias Imports =
    Dict String (Dict String String)



---- CANONICALIZE


canonicalizeModule : Dict String Module -> String -> File -> Interface -> Module
canonicalizeModule importedModules name file interface =
    let
        initialModule =
            { imports = imports
            , exposed = []
            , values = Dict.empty
            , unions = Dict.empty
            , aliases = Dict.empty
            , binops = Dict.empty
            }

        imports =
            if Set.member name defaultImportModules then
                collectImports importedModules file.imports

            else
                collectImports importedModules
                    (file.imports ++ defaultImports)

        exposingList =
            Elm.Syntax.Module.exposingList (Node.value file.moduleDefinition)
    in
    file.declarations
        |> List.foldl
            (canonicalizeDeclaration
                file.declarations
                imports
                name
            )
            initialModule
        |> computeExposed exposingList


canonicalizeDeclaration :
    List (Node Declaration)
    -> Imports
    -> String
    -> Node Declaration
    -> Module
    -> Module
canonicalizeDeclaration declarations imports homeModuleName declaration currentModule =
    case Node.value declaration of
        FunctionDeclaration f ->
            case f.signature of
                Nothing ->
                    currentModule

                Just signature ->
                    let
                        name =
                            f.declaration
                                |> Node.value
                                |> .name
                                |> Node.value

                        tipe =
                            signature
                                |> Node.value
                                |> .typeAnnotation
                                |> canonicalizeTypeAnnotation imports homeModuleName
                    in
                    { currentModule
                        | values = Dict.insert name tipe currentModule.values
                    }

        AliasDeclaration a ->
            { currentModule
                | aliases =
                    Dict.insert
                        (Node.value a.name)
                        { vars = List.map Node.value a.generics
                        , tipe =
                            canonicalizeTypeAnnotation
                                imports
                                homeModuleName
                                a.typeAnnotation
                        }
                        currentModule.aliases
            }

        CustomTypeDeclaration c ->
            let
                toConstructor valueConstructor =
                    ( Node.value valueConstructor.name
                    , List.map
                        (canonicalizeTypeAnnotation imports homeModuleName)
                        valueConstructor.arguments
                    )
            in
            { currentModule
                | unions =
                    Dict.insert
                        (Node.value c.name)
                        { vars = List.map Node.value c.generics
                        , constructors =
                            List.map (Node.value >> toConstructor) c.constructors
                                |> Dict.fromList
                        }
                        currentModule.unions
            }

        PortDeclaration p ->
            currentModule

        InfixDeclaration i ->
            let
                associativity =
                    case Node.value i.direction of
                        Src.Left ->
                            Left

                        Src.Non ->
                            Non

                        Src.Right ->
                            Right

                function =
                    Node.value i.function

                infixFunction d =
                    case Node.value d of
                        FunctionDeclaration f ->
                            function
                                == (f.declaration
                                        |> Node.value
                                        |> .name
                                        |> Node.value
                                   )

                        _ ->
                            False
            in
            case
                List.find infixFunction declarations
                    |> Maybe.map Node.value
            of
                Nothing ->
                    currentModule

                Just (FunctionDeclaration f) ->
                    case f.signature of
                        Nothing ->
                            currentModule

                        Just signature ->
                            let
                                tipe =
                                    signature
                                        |> Node.value
                                        |> .typeAnnotation
                                        |> canonicalizeTypeAnnotation imports homeModuleName
                            in
                            { currentModule
                                | binops =
                                    Dict.insert
                                        (Node.value i.operator)
                                        { function = function
                                        , tipe = Canonical.Annotation.fromType tipe
                                        , precedence = Node.value i.precedence
                                        , associativity = associativity
                                        }
                                        currentModule.binops
                            }

                Just _ ->
                    currentModule

        Destructuring _ _ ->
            currentModule


canonicalizeTypeAnnotation : Imports -> String -> Node Src.TypeAnnotation -> Type
canonicalizeTypeAnnotation imports homeModuleName typeAnnotation =
    case Node.value typeAnnotation of
        Src.GenericType var ->
            Var var

        Src.Typed qualifiedName typeAnnotations ->
            let
                ( qualifier, name ) =
                    Node.value qualifiedName
            in
            case lookupModuleName imports (String.join "." qualifier) name of
                Nothing ->
                    Type
                        homeModuleName
                        name
                        (List.map
                            (canonicalizeTypeAnnotation imports homeModuleName)
                            typeAnnotations
                        )

                Just moduleName ->
                    Type
                        moduleName
                        name
                        (List.map
                            (canonicalizeTypeAnnotation imports homeModuleName)
                            typeAnnotations
                        )

        Src.Unit ->
            Tuple []

        Src.Tupled typeAnnotations ->
            Tuple
                (List.map
                    (canonicalizeTypeAnnotation imports homeModuleName)
                    typeAnnotations
                )

        Src.Record recordFields ->
            Record
                (List.map (canonicalizeRecordField imports homeModuleName) recordFields)
                Nothing

        Src.GenericRecord var recordFields ->
            Record
                (List.map
                    (canonicalizeRecordField imports homeModuleName)
                    (Node.value recordFields)
                )
                (Just (Node.value var))

        Src.FunctionTypeAnnotation from to ->
            Lambda
                (canonicalizeTypeAnnotation imports homeModuleName from)
                (canonicalizeTypeAnnotation imports homeModuleName to)


canonicalizeRecordField : Imports -> String -> Node Src.RecordField -> ( String, Type )
canonicalizeRecordField imports homeModuleName recordField =
    let
        ( name, annotation ) =
            Node.value recordField
    in
    ( Node.value name
    , canonicalizeTypeAnnotation imports homeModuleName annotation
    )


computeExposed : Exposing -> Module -> Module
computeExposed exposingList currentModule =
    case exposingList of
        All _ ->
            { currentModule
                | exposed =
                    List.concat
                        [ Dict.keys currentModule.values
                        , Dict.keys currentModule.unions
                        , Dict.values currentModule.unions
                            |> List.concatMap (.constructors >> Dict.keys)
                        , Dict.keys currentModule.aliases
                        , Dict.keys currentModule.binops
                        ]
            }

        Explicit topLevelExposes ->
            { currentModule
                | exposed =
                    List.concatMap
                        (Node.value >> computeTopLevelExpose currentModule.unions)
                        topLevelExposes
            }


computeTopLevelExpose : Dict String Union -> TopLevelExpose -> List String
computeTopLevelExpose unions topLevelExpose =
    case topLevelExpose of
        InfixExpose name ->
            [ name ]

        FunctionExpose name ->
            [ name ]

        TypeOrAliasExpose name ->
            [ name ]

        TypeExpose exposedType ->
            case exposedType.open of
                Nothing ->
                    [ exposedType.name ]

                Just _ ->
                    case Dict.get exposedType.name unions of
                        Nothing ->
                            []

                        Just union ->
                            exposedType.name
                                :: Dict.keys union.constructors



---- LOOKUP


lookupModuleName : Imports -> String -> String -> Maybe String
lookupModuleName imports qualifier name =
    Dict.get qualifier imports
        |> Maybe.andThen (Dict.get name)



---- COLLECT IMPORTS


collectImports : Dict String Module -> List (Node Import) -> Imports
collectImports importedModules imports =
    let
        initialImports =
            Dict.empty
    in
    List.foldl
        (\import_ currentImports ->
            let
                moduleName =
                    import_
                        |> Node.value
                        |> .moduleName
                        |> Node.value
                        |> String.join "."
            in
            case Dict.get moduleName importedModules of
                Nothing ->
                    currentImports

                Just importedModule ->
                    collectExposed importedModule (Node.value import_) currentImports
        )
        initialImports
        imports


collectExposed : Module -> Import -> Imports -> Imports
collectExposed importedModule { moduleName, moduleAlias, exposingList } currentImports =
    let
        qualifier =
            moduleAlias
                |> Maybe.withDefault moduleName
                |> Node.value
                |> String.join "."

        actualModuleName =
            moduleName
                |> Node.value
                |> String.join "."
    in
    case exposingList of
        Nothing ->
            List.foldl
                (collectExposedValue qualifier actualModuleName)
                currentImports
                importedModule.exposed

        Just exposing_ ->
            case Node.value exposing_ of
                All _ ->
                    List.foldl
                        (collectExposedValue "" actualModuleName)
                        currentImports
                        importedModule.exposed

                Explicit topLevelExposes ->
                    List.foldl
                        (collectTopLevelExpose importedModule.unions "" actualModuleName)
                        currentImports
                        topLevelExposes


collectTopLevelExpose :
    Dict String Union
    -> String
    -> String
    -> Node TopLevelExpose
    -> Imports
    -> Imports
collectTopLevelExpose unions qualifier actualModuleName topLevelExpose currentImports =
    case Node.value topLevelExpose of
        InfixExpose name ->
            collectExposedValue qualifier actualModuleName name currentImports

        FunctionExpose name ->
            collectExposedValue qualifier actualModuleName name currentImports

        TypeOrAliasExpose name ->
            collectExposedValue qualifier actualModuleName name currentImports

        TypeExpose exposedType ->
            let
                importsWithType =
                    collectExposedValue qualifier
                        actualModuleName
                        exposedType.name
                        currentImports
            in
            case exposedType.open of
                Nothing ->
                    importsWithType

                Just _ ->
                    case Dict.get exposedType.name unions of
                        Nothing ->
                            currentImports

                        Just union ->
                            List.foldl
                                (collectExposedValue qualifier actualModuleName)
                                importsWithType
                                (Dict.keys union.constructors)


collectExposedValue : String -> String -> String -> Imports -> Imports
collectExposedValue qualifier actualModuleName name currentImports =
    Dict.update qualifier
        (\maybeImports ->
            case maybeImports of
                Nothing ->
                    Just (Dict.singleton name actualModuleName)

                Just imports ->
                    Just (Dict.insert name actualModuleName imports)
        )
        currentImports



---- BUILD


type alias ModuleData =
    { name : String
    , fileName : String
    , file : File
    , imports : List Import
    , interface : Interface
    }


type alias Store =
    { todo : List TodoItem
    , done : Dict String Module
    }


emptyStore : Store
emptyStore =
    { todo = []
    , done = Dict.empty
    }


type alias TodoItem =
    { blockedBy : Set String
    , moduleData : ModuleData
    }


add : ModuleData -> Store -> Store
add moduleData store =
    { store | todo = TodoItem (requiredModules moduleData store) moduleData :: store.todo }
        |> canonicalizeTodo Set.empty


replace : ModuleData -> Store -> Store
replace moduleData store =
    { store
        | todo = TodoItem (requiredModules moduleData store) moduleData :: store.todo
        , done = Dict.remove moduleData.name store.done
    }
        |> canonicalizeTodo Set.empty


canonicalizeTodo : Set String -> Store -> Store
canonicalizeTodo nowDone store =
    let
        ( newlyDone, newTodo ) =
            List.foldl
                (\{ blockedBy, moduleData } ( done, todo ) ->
                    let
                        newBlockers =
                            Set.diff blockedBy nowDone
                    in
                    if Set.isEmpty newBlockers then
                        ( Dict.insert moduleData.name
                            (canonicalizeModule
                                (Dict.union store.done done)
                                moduleData.name
                                moduleData.file
                                moduleData.interface
                            )
                            done
                        , todo
                        )

                    else
                        ( done
                        , { blockedBy = newBlockers
                          , moduleData = moduleData
                          }
                            :: todo
                        )
                )
                ( Dict.empty, [] )
                store.todo

        newStore =
            { store
                | todo = newTodo
                , done = Dict.union store.done newlyDone
            }
    in
    if Dict.isEmpty newlyDone then
        newStore

    else
        canonicalizeTodo
            (Set.fromList (Dict.keys newlyDone))
            newStore



-- REQUIRED MODULES


requiredModules : ModuleData -> Store -> Set String
requiredModules moduleData store =
    let
        needed =
            List.map (.moduleName >> Node.value) moduleData.imports
                |> List.filter (not << isNative)
                |> List.map (String.join ".")
                |> Set.fromList

        isNative =
            List.head
                >> Maybe.map ((==) "Native")
                >> Maybe.withDefault False

        availableModules =
            Dict.keys store.done
                |> Set.fromList
    in
    Set.diff needed availableModules



---- DEFAULT IMPORTS


defaultImportModules : Set String
defaultImportModules =
    Set.fromList
        [ "Basics"
        , "List"
        , "Maybe"
        , "Result"
        , "String"
        , "Char"
        , "Tuple"
        , "Debug"
        , "Platform"
        , "Platform.Cmd"
        , "Platform.Sub"
        ]


defaultImports : List (Node Import)
defaultImports =
    -- import Basics exposing (..)
    -- import List exposing (List, (::))
    -- import Maybe exposing (Maybe(..))
    -- import Result exposing (Result(..))
    -- import String exposing (String)
    -- import Char exposing (Char)
    -- import Tuple
    --
    -- import Debug
    --
    -- import Platform exposing ( Program )
    -- import Platform.Cmd as Cmd exposing ( Cmd )
    -- import Platform.Sub as Sub exposing ( Sub )
    [ { moduleName = Node emptyRange [ "Basics" ]
      , moduleAlias = Nothing
      , exposingList = Just (Node emptyRange (All emptyRange))
      }
    , { moduleName = Node emptyRange [ "List" ]
      , moduleAlias = Nothing
      , exposingList =
            Just <|
                Node emptyRange <|
                    Explicit
                        [ Node emptyRange (TypeOrAliasExpose "List")
                        , Node emptyRange (InfixExpose "(::)")
                        ]
      }
    , { moduleName = Node emptyRange [ "Maybe" ]
      , moduleAlias = Nothing
      , exposingList =
            Just <|
                Node emptyRange <|
                    Explicit
                        [ Node emptyRange <|
                            TypeExpose
                                { name = "Maybe"
                                , open = Just emptyRange
                                }
                        ]
      }
    , { moduleName = Node emptyRange [ "Result" ]
      , moduleAlias = Nothing
      , exposingList =
            Just <|
                Node emptyRange <|
                    Explicit
                        [ Node emptyRange <|
                            TypeExpose
                                { name = "Result"
                                , open = Just emptyRange
                                }
                        ]
      }
    , { moduleName = Node emptyRange [ "String" ]
      , moduleAlias = Nothing
      , exposingList =
            Just <|
                Node emptyRange <|
                    Explicit
                        [ Node emptyRange (TypeOrAliasExpose "String") ]
      }
    , { moduleName = Node emptyRange [ "Char" ]
      , moduleAlias = Nothing
      , exposingList =
            Just <|
                Node emptyRange <|
                    Explicit
                        [ Node emptyRange (TypeOrAliasExpose "Char") ]
      }
    , { moduleName = Node emptyRange [ "Tuple" ]
      , moduleAlias = Nothing
      , exposingList = Nothing
      }
    , { moduleName = Node emptyRange [ "Debug" ]
      , moduleAlias = Nothing
      , exposingList = Nothing
      }
    , { moduleName = Node emptyRange [ "Platform" ]
      , moduleAlias = Nothing
      , exposingList =
            Just <|
                Node emptyRange <|
                    Explicit
                        [ Node emptyRange (TypeOrAliasExpose "Platform") ]
      }
    , { moduleName = Node emptyRange [ "Platform", "Cmd" ]
      , moduleAlias = Just (Node emptyRange [ "Cmd" ])
      , exposingList =
            Just <|
                Node emptyRange <|
                    Explicit
                        [ Node emptyRange (TypeOrAliasExpose "Cmd") ]
      }
    , { moduleName = Node emptyRange [ "Platform", "Sub" ]
      , moduleAlias = Just (Node emptyRange [ "Sub" ])
      , exposingList =
            Just <|
                Node emptyRange <|
                    Explicit
                        [ Node emptyRange (TypeOrAliasExpose "Sub") ]
      }
    ]
        |> List.map (Node emptyRange)
