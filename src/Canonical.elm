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
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (emptyRange)
import Elm.Syntax.TypeAnnotation as Src
import List.Extra as List
import Set exposing (Set)



---- MODULE


type alias Module =
    { exposedValues : Dict String Annotation
    , exposedUnions : Dict String Union
    , exposedBinops : Dict String Binop
    , values : Dict String Annotation
    , unions : Dict String Union
    , aliases : Dict String Alias
    , binops : Dict String Binop
    , qualifiedValues : Dict ModuleName (Dict String Annotation)
    , qualifiedUnions : Dict ModuleName (Dict String Union)
    }


type alias Union =
    { moduleName : ModuleName
    , vars : List String
    , constructors : List ( String, List Type )
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



---- CANONICALIZE


canonicalizeModule : Dict ModuleName Module -> ModuleName -> File -> Interface -> Module
canonicalizeModule importedModules moduleName file interface =
    let
        initialModule =
            { exposedValues = Dict.empty
            , exposedUnions =
                if moduleName == [ "List" ] then
                    Dict.singleton "List"
                        { moduleName = [ "List" ]
                        , vars = [ "a" ]
                        , constructors = []
                        }

                else
                    Dict.empty
            , exposedBinops = Dict.empty
            , values = Dict.empty
            , unions =
                if moduleName == [ "List" ] then
                    Dict.singleton "List"
                        { moduleName = [ "List" ]
                        , vars = [ "a" ]
                        , constructors = []
                        }

                else
                    Dict.empty
            , aliases = Dict.empty
            , binops = binops
            , qualifiedValues = qualifiedValues
            , qualifiedUnions = qualifiedUnions
            }

        { binops, qualifiedValues, qualifiedUnions } =
            if Set.member moduleName defaultImportModules then
                collectImports importedModules file.imports

            else
                collectImports importedModules (file.imports ++ defaultImports)

        exposingList =
            Elm.Syntax.Module.exposingList (Node.value file.moduleDefinition)
    in
    file.declarations
        |> List.foldl (canonicalizeDeclaration moduleName file.declarations) initialModule
        |> computeExposed exposingList


canonicalizeDeclaration :
    ModuleName
    -> List (Node Declaration)
    -> Node Declaration
    -> Module
    -> Module
canonicalizeDeclaration moduleName declarations declaration currentModule =
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
                                |> canonicalizeTypeAnnotation moduleName currentModule
                    in
                    { currentModule
                        | values =
                            Dict.insert name
                                (Canonical.Annotation.fromType tipe)
                                currentModule.values
                    }

        AliasDeclaration a ->
            let
                tipe =
                    canonicalizeTypeAnnotation moduleName currentModule a.typeAnnotation
            in
            { currentModule
                | aliases =
                    Dict.insert
                        (Node.value a.name)
                        { vars = List.map Node.value a.generics
                        , tipe = tipe
                        }
                        currentModule.aliases
            }

        CustomTypeDeclaration c ->
            let
                typeName =
                    Node.value c.name

                vars =
                    List.map Node.value c.generics

                constructors =
                    List.map (Node.value >> toConstructor) c.constructors

                toConstructor valueConstructor =
                    ( Node.value valueConstructor.name
                    , List.map
                        (canonicalizeTypeAnnotation moduleName currentModule)
                        valueConstructor.arguments
                    )

                union =
                    { moduleName = moduleName
                    , vars = vars
                    , constructors = constructors
                    }
            in
            { currentModule
                | unions = Dict.insert typeName union currentModule.unions
                , values = insertConstructors typeName union currentModule.values
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
                                        |> canonicalizeTypeAnnotation
                                            moduleName
                                            currentModule
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


canonicalizeTypeAnnotation : ModuleName -> Module -> Node Src.TypeAnnotation -> Type
canonicalizeTypeAnnotation moduleName currentModule typeAnnotation =
    case Node.value typeAnnotation of
        Src.GenericType var ->
            Var var

        Src.Typed qualifiedName typeAnnotations ->
            let
                ( qualifier, name ) =
                    Node.value qualifiedName
            in
            case lookupUnion currentModule qualifier name of
                Nothing ->
                    -- TODO we just assume its a localy defined type here
                    Type
                        moduleName
                        name
                        (List.map
                            (canonicalizeTypeAnnotation moduleName currentModule)
                            typeAnnotations
                        )

                Just union ->
                    Type
                        union.moduleName
                        name
                        (List.map
                            (canonicalizeTypeAnnotation moduleName currentModule)
                            typeAnnotations
                        )

        Src.Unit ->
            Tuple []

        Src.Tupled typeAnnotations ->
            Tuple
                (List.map
                    (canonicalizeTypeAnnotation moduleName currentModule)
                    typeAnnotations
                )

        Src.Record recordFields ->
            Record
                (List.map (canonicalizeRecordField moduleName currentModule) recordFields)
                Nothing

        Src.GenericRecord var recordFields ->
            Record
                (List.map
                    (canonicalizeRecordField moduleName currentModule)
                    (Node.value recordFields)
                )
                (Just (Node.value var))

        Src.FunctionTypeAnnotation from to ->
            Lambda
                (canonicalizeTypeAnnotation moduleName currentModule from)
                (canonicalizeTypeAnnotation moduleName currentModule to)


canonicalizeRecordField : ModuleName -> Module -> Node Src.RecordField -> ( String, Type )
canonicalizeRecordField moduleName currentModule recordField =
    let
        ( name, annotation ) =
            Node.value recordField
    in
    ( Node.value name
    , canonicalizeTypeAnnotation moduleName currentModule annotation
    )


lookupValue : Module -> ModuleName -> String -> Maybe Annotation
lookupValue currentModule moduleName name =
    Dict.get moduleName currentModule.qualifiedValues
        |> Maybe.andThen (Dict.get name)


lookupUnion : Module -> ModuleName -> String -> Maybe Union
lookupUnion currentModule moduleName name =
    Dict.get moduleName currentModule.qualifiedUnions
        |> Maybe.andThen (Dict.get name)


computeExposed : Exposing -> Module -> Module
computeExposed exposingList currentModule =
    case exposingList of
        All _ ->
            { currentModule
                | exposedValues = currentModule.values
                , exposedUnions = currentModule.unions
            }

        Explicit topLevelExposes ->
            List.foldl
                computeTopLevelExpose
                currentModule
                topLevelExposes


computeTopLevelExpose : Node TopLevelExpose -> Module -> Module
computeTopLevelExpose topLevelExpose currentModule =
    case Node.value topLevelExpose of
        InfixExpose name ->
            case Dict.get name currentModule.binops of
                Nothing ->
                    currentModule

                Just binop ->
                    { currentModule
                        | exposedBinops =
                            Dict.insert name binop currentModule.exposedBinops
                    }

        FunctionExpose name ->
            case Dict.get name currentModule.values of
                Nothing ->
                    currentModule

                Just annotation ->
                    { currentModule
                        | exposedValues =
                            Dict.insert name annotation currentModule.exposedValues
                    }

        TypeOrAliasExpose name ->
            case Dict.get name currentModule.unions of
                Nothing ->
                    currentModule

                Just union ->
                    { currentModule
                        | exposedUnions =
                            Dict.insert name union currentModule.exposedUnions
                    }

        TypeExpose exposedType ->
            case exposedType.open of
                Nothing ->
                    case Dict.get exposedType.name currentModule.unions of
                        Nothing ->
                            currentModule

                        Just union ->
                            { currentModule
                                | exposedUnions =
                                    Dict.insert exposedType.name
                                        union
                                        currentModule.exposedUnions
                            }

                Just _ ->
                    case Dict.get exposedType.name currentModule.unions of
                        Nothing ->
                            currentModule

                        Just union ->
                            { currentModule
                                | exposedUnions =
                                    Dict.insert exposedType.name
                                        union
                                        currentModule.exposedUnions
                                , exposedValues =
                                    List.foldl
                                        (\( name, types ) ->
                                            Dict.insert name
                                                (Canonical.Annotation.fromType <|
                                                    List.foldl Lambda
                                                        (Type union.moduleName
                                                            exposedType.name
                                                            (List.map Var union.vars)
                                                        )
                                                        types
                                                )
                                        )
                                        currentModule.exposedValues
                                        union.constructors
                            }



---- COLLECT IMPORTS


type alias Imports =
    { binops : Dict String Binop
    , qualifiedValues : Dict ModuleName (Dict String Annotation)
    , qualifiedUnions : Dict ModuleName (Dict String Union)
    }


collectImports :
    Dict ModuleName Module
    -> List (Node Import)
    -> Imports
collectImports importedModules imports =
    let
        initialImports =
            { binops = Dict.empty
            , qualifiedValues = Dict.empty
            , qualifiedUnions = Dict.empty
            }
    in
    List.foldl
        (\import_ imports_ ->
            let
                moduleName =
                    import_
                        |> Node.value
                        |> .moduleName
                        |> Node.value
            in
            case Dict.get moduleName importedModules of
                Nothing ->
                    imports_

                Just importedModule ->
                    let
                        qualifier =
                            moduleAlias
                                |> Maybe.map Node.value
                                |> Maybe.withDefault moduleName

                        { moduleAlias, exposingList } =
                            Node.value import_
                    in
                    imports_
                        |> collectQualifiedExposed importedModule qualifier
                        |> collectExposed
                            importedModule
                            qualifier
                            moduleName
                            exposingList
        )
        initialImports
        imports


collectQualifiedExposed : Module -> ModuleName -> Imports -> Imports
collectQualifiedExposed importedModule qualifier imports =
    { imports
        | qualifiedValues =
            Dict.update qualifier
                (\maybeValues ->
                    case maybeValues of
                        Nothing ->
                            Just importedModule.exposedValues

                        Just values ->
                            Just <|
                                Dict.union values
                                    importedModule.exposedValues
                )
                imports.qualifiedValues
        , qualifiedUnions =
            Dict.update qualifier
                (\maybeUnions ->
                    case maybeUnions of
                        Nothing ->
                            Just importedModule.exposedUnions

                        Just unions ->
                            Just <|
                                Dict.union unions
                                    importedModule.exposedUnions
                )
                imports.qualifiedUnions
    }


collectExposed :
    Module
    -> ModuleName
    -> ModuleName
    -> Maybe (Node Exposing)
    -> Imports
    -> Imports
collectExposed importedModule qualifier actualModuleName exposingList imports =
    case exposingList of
        Nothing ->
            imports

        Just exposing_ ->
            case Node.value exposing_ of
                All _ ->
                    { imports
                        | qualifiedValues =
                            Dict.update []
                                (\maybeValues ->
                                    case maybeValues of
                                        Nothing ->
                                            Just importedModule.exposedValues

                                        Just values ->
                                            Just <|
                                                Dict.union values
                                                    importedModule.exposedValues
                                )
                                imports.qualifiedValues
                        , qualifiedUnions =
                            Dict.update []
                                (\maybeUnions ->
                                    case maybeUnions of
                                        Nothing ->
                                            Just importedModule.exposedUnions

                                        Just unions ->
                                            Just <|
                                                Dict.union unions
                                                    importedModule.exposedUnions
                                )
                                imports.qualifiedUnions
                        , binops = Dict.union imports.binops importedModule.exposedBinops
                    }

                Explicit topLevelExposes ->
                    List.foldl
                        (collectTopLevelExpose
                            importedModule
                            []
                            actualModuleName
                        )
                        imports
                        topLevelExposes


collectTopLevelExpose :
    Module
    -> ModuleName
    -> ModuleName
    -> Node TopLevelExpose
    -> Imports
    -> Imports
collectTopLevelExpose importedModule qualifier moduleName topLevelExpose currentImports =
    case Node.value topLevelExpose of
        InfixExpose name ->
            let
                actualName =
                    String.dropLeft 1 (String.dropRight 1 name)
            in
            case Dict.get actualName importedModule.exposedBinops of
                Nothing ->
                    currentImports

                Just binop ->
                    { currentImports
                        | binops = Dict.insert actualName binop currentImports.binops
                    }

        FunctionExpose name ->
            collectExposedValue importedModule.values
                qualifier
                moduleName
                name
                currentImports

        TypeOrAliasExpose name ->
            case Dict.get name importedModule.unions of
                Nothing ->
                    currentImports

                Just union ->
                    let
                        newQualifiedUnions =
                            Dict.update qualifier
                                (\maybeImports ->
                                    case maybeImports of
                                        Nothing ->
                                            Just (Dict.singleton name union)

                                        Just imports ->
                                            Just (Dict.insert name union imports)
                                )
                                currentImports.qualifiedUnions
                    in
                    { currentImports | qualifiedUnions = newQualifiedUnions }

        TypeExpose exposedType ->
            case Dict.get exposedType.name importedModule.unions of
                Nothing ->
                    currentImports

                Just union ->
                    let
                        newQualifiedUnions =
                            Dict.update qualifier
                                (\maybeImports ->
                                    case maybeImports of
                                        Nothing ->
                                            Just (Dict.singleton exposedType.name union)

                                        Just imports ->
                                            Just
                                                (Dict.insert exposedType.name
                                                    union
                                                    imports
                                                )
                                )
                                currentImports.qualifiedUnions
                    in
                    case exposedType.open of
                        Nothing ->
                            { currentImports | qualifiedUnions = newQualifiedUnions }

                        Just _ ->
                            { currentImports
                                | qualifiedUnions = newQualifiedUnions
                                , qualifiedValues =
                                    Dict.update qualifier
                                        (\maybeValues ->
                                            case maybeValues of
                                                Nothing ->
                                                    Just (insertConstructors exposedType.name union Dict.empty)

                                                Just values ->
                                                    Just (insertConstructors exposedType.name union values)
                                        )
                                        currentImports.qualifiedValues
                            }


insertConstructors : String -> Union -> Dict String Annotation -> Dict String Annotation
insertConstructors typeName union values =
    List.foldl
        (\( name, types ) ->
            Dict.insert name
                (Canonical.Annotation.fromType <|
                    List.foldl Lambda
                        (Type union.moduleName
                            typeName
                            (List.map Var union.vars)
                        )
                        types
                )
        )
        values
        union.constructors


collectExposedValue :
    Dict String Annotation
    -> ModuleName
    -> ModuleName
    -> String
    -> Imports
    -> Imports
collectExposedValue values qualifer moduleName name imports =
    case Dict.get name values of
        Nothing ->
            imports

        Just annotation ->
            { imports
                | qualifiedValues =
                    Dict.update
                        qualifer
                        (\maybeValues ->
                            case maybeValues of
                                Nothing ->
                                    Just (Dict.singleton name annotation)

                                Just values_ ->
                                    Just (Dict.insert name annotation values_)
                        )
                        imports.qualifiedValues
            }



---- BUILD


type alias ModuleData =
    { moduleName : ModuleName
    , fileName : String
    , file : File
    , imports : List Import
    , interface : Interface
    }


type alias Store =
    { todo : List TodoItem
    , done : Dict ModuleName Module
    }


emptyStore : Store
emptyStore =
    { todo = []
    , done = Dict.empty
    }


type alias TodoItem =
    { blockedBy : Set ModuleName
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
        , done = Dict.remove moduleData.moduleName store.done
    }
        |> canonicalizeTodo Set.empty


canonicalizeTodo : Set ModuleName -> Store -> Store
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
                        ( Dict.insert moduleData.moduleName
                            (canonicalizeModule
                                (Dict.union store.done done)
                                moduleData.moduleName
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


requiredModules : ModuleData -> Store -> Set ModuleName
requiredModules moduleData store =
    let
        needed =
            List.map (.moduleName >> Node.value) moduleData.imports
                |> List.filter (not << isNative)
                |> Set.fromList

        isNative moduleName =
            case moduleName of
                "Native" :: _ ->
                    True

                "Elm" :: "Kernel" :: _ ->
                    True

                _ ->
                    False

        availableModules =
            Dict.keys store.done
                |> Set.fromList
    in
    Set.diff needed availableModules



---- DEFAULT IMPORTS


defaultImportModules : Set ModuleName
defaultImportModules =
    Set.fromList
        [ [ "Basics" ]
        , [ "List" ]
        , [ "Maybe" ]
        , [ "Result" ]
        , [ "String" ]
        , [ "Char" ]
        , [ "Tuple" ]
        , [ "Debug" ]
        , [ "Platform" ]
        , [ "Platform", "Cmd" ]
        , [ "Platform", "Sub" ]
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
