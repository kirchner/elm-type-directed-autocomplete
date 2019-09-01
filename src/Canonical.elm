module Canonical exposing
    ( Alias
    , Associativity(..)
    , Binop
    , Constructor
    , Imports
    , Module
    , ModuleData
    , Store
    , TodoItem
    , Type
    , add
    , addImports
    , canonicalizeModule
    , canonicalizeTypeAnnotation
    , emptyStore
    , replace
    )

import Canonical.Annotation exposing (Annotation(..))
import Canonical.Type as Can
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
    { exposedTypes : Exposed Type
    , exposedAliases : Exposed Alias
    , exposedConstructors : Exposed Constructor
    , exposedBinops : Exposed Binop
    , exposedValues : Exposed Annotation
    , types : Local Type
    , aliases : Local Alias
    , constructors : Local Constructor
    , binops : Local Binop
    , values : Local Annotation
    , qualifiedTypes : Qualified Type
    , qualifiedAliases : Qualified Alias
    , qualifiedConstructors : Qualified Constructor
    , qualifiedValues : Qualified Annotation
    }


type alias Exposed a =
    Dict String a


type alias Local a =
    Dict String a


type alias Qualified a =
    Dict ModuleName (Dict String a)


type alias Type =
    { moduleName : ModuleName
    , vars : List String
    , tags : List String
    }


type alias Constructor =
    { args : List Can.Type
    , tipe : Can.Type
    }


type alias Alias =
    { vars : List String
    , tipe : Can.Type
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
            { exposedTypes =
                if moduleName == [ "List" ] then
                    Dict.singleton "List"
                        { moduleName = [ "List" ]
                        , vars = [ "a" ]
                        , tags = []
                        }

                else
                    Dict.empty
            , exposedAliases = Dict.empty
            , exposedConstructors = Dict.empty
            , exposedBinops = Dict.empty
            , exposedValues = Dict.empty
            , types =
                if moduleName == [ "List" ] then
                    Dict.singleton "List"
                        { moduleName = [ "List" ]
                        , vars = [ "a" ]
                        , tags = []
                        }

                else
                    Dict.empty
            , aliases = Dict.empty
            , constructors = Dict.empty
            , binops = imports.binops
            , values = Dict.empty
            , qualifiedTypes = imports.qualifiedTypes
            , qualifiedAliases = imports.qualifiedAliases
            , qualifiedConstructors = imports.qualifiedConstructors
            , qualifiedValues = imports.qualifiedValues
            }

        imports =
            if Set.member moduleName coreModules then
                addImports importedModules file.imports

            else
                addImports importedModules (file.imports ++ defaultImports)

        exposingList =
            Elm.Syntax.Module.exposingList (Node.value file.moduleDefinition)
    in
    file.declarations
        |> List.foldl
            (canonicalizeDeclaration moduleName file.declarations)
            initialModule
        |> expose exposingList


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
            in
            { currentModule
                | types =
                    Dict.insert typeName
                        { moduleName = moduleName
                        , vars = vars
                        , tags = List.map Tuple.first constructors
                        }
                        currentModule.types
                , constructors =
                    List.foldl
                        (\( name, args ) ->
                            Dict.insert name
                                { args = args
                                , tipe =
                                    Can.Type moduleName
                                        typeName
                                        (List.map Can.Var vars)
                                }
                        )
                        currentModule.constructors
                        constructors
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


canonicalizeTypeAnnotation : ModuleName -> Module -> Node Src.TypeAnnotation -> Can.Type
canonicalizeTypeAnnotation moduleName currentModule typeAnnotation =
    case Node.value typeAnnotation of
        Src.GenericType var ->
            Can.Var var

        Src.Typed qualifiedName typeAnnotations ->
            let
                ( qualifier, name ) =
                    Node.value qualifiedName
            in
            case lookupType currentModule qualifier name of
                Nothing ->
                    case lookupAlias currentModule qualifier name of
                        Nothing ->
                            -- TODO If we implemented everything correct and
                            -- the Elm file compiles, then this should actually
                            -- not happen. So, for simplicity, we just assume
                            -- the type is defined within the current module
                            Can.Type
                                moduleName
                                name
                                (List.map
                                    (canonicalizeTypeAnnotation moduleName currentModule)
                                    typeAnnotations
                                )

                        Just alias_ ->
                            substituteVars alias_.tipe
                                alias_.vars
                                (List.map
                                    (canonicalizeTypeAnnotation moduleName currentModule)
                                    typeAnnotations
                                )

                Just tipe ->
                    Can.Type
                        tipe.moduleName
                        name
                        (List.map
                            (canonicalizeTypeAnnotation moduleName currentModule)
                            typeAnnotations
                        )

        Src.Unit ->
            Can.Tuple []

        Src.Tupled typeAnnotations ->
            Can.Tuple
                (List.map
                    (canonicalizeTypeAnnotation moduleName currentModule)
                    typeAnnotations
                )

        Src.Record recordFields ->
            Can.Record
                (List.map (canonicalizeRecordField moduleName currentModule) recordFields)
                Nothing

        Src.GenericRecord var recordFields ->
            Can.Record
                (List.map
                    (canonicalizeRecordField moduleName currentModule)
                    (Node.value recordFields)
                )
                (Just (Node.value var))

        Src.FunctionTypeAnnotation from to ->
            Can.Lambda
                (canonicalizeTypeAnnotation moduleName currentModule from)
                (canonicalizeTypeAnnotation moduleName currentModule to)


substituteVars : Can.Type -> List String -> List Can.Type -> Can.Type
substituteVars tipe vars types =
    let
        subst =
            Dict.fromList (zip [] vars types)

        zip zipped listA listB =
            case ( listA, listB ) of
                ( a :: restA, b :: restB ) ->
                    zip (( a, b ) :: zipped) restA restB

                _ ->
                    List.reverse zipped
    in
    substituteVarsHelp subst tipe


substituteVarsHelp : Dict String Can.Type -> Can.Type -> Can.Type
substituteVarsHelp subst tipe =
    case tipe of
        Can.Lambda from to ->
            Can.Lambda
                (substituteVarsHelp subst from)
                (substituteVarsHelp subst to)

        Can.Var name ->
            case Dict.get name subst of
                Nothing ->
                    tipe

                Just newType ->
                    newType

        Can.Type moduleName name types ->
            Can.Type moduleName
                name
                (List.map (substituteVarsHelp subst) types)

        Can.Record fields maybeVar ->
            case maybeVar of
                Nothing ->
                    Can.Record
                        (List.map (Tuple.mapSecond (substituteVarsHelp subst)) fields)
                        maybeVar

                Just name ->
                    case Dict.get name subst of
                        Nothing ->
                            Can.Record
                                (List.map (Tuple.mapSecond (substituteVarsHelp subst))
                                    fields
                                )
                                maybeVar

                        Just (Can.Record newFields newMaybeVar) ->
                            Can.Record (fields ++ newFields) newMaybeVar

                        _ ->
                            Can.Record
                                (List.map (Tuple.mapSecond (substituteVarsHelp subst))
                                    fields
                                )
                                maybeVar

        Can.Unit ->
            tipe

        Can.Tuple types ->
            Can.Tuple (List.map (substituteVarsHelp subst) types)


canonicalizeRecordField : ModuleName -> Module -> Node Src.RecordField -> ( String, Can.Type )
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


lookupType : Module -> ModuleName -> String -> Maybe Type
lookupType currentModule moduleName name =
    Dict.get moduleName currentModule.qualifiedTypes
        |> Maybe.andThen (Dict.get name)


lookupAlias : Module -> ModuleName -> String -> Maybe Alias
lookupAlias currentModule moduleName name =
    if List.isEmpty moduleName then
        case Dict.get name currentModule.aliases of
            Nothing ->
                Dict.get moduleName currentModule.qualifiedAliases
                    |> Maybe.andThen (Dict.get name)

            maybeAlias ->
                maybeAlias

    else
        Dict.get moduleName currentModule.qualifiedAliases
            |> Maybe.andThen (Dict.get name)


expose : Exposing -> Module -> Module
expose exposingList currentModule =
    case exposingList of
        All _ ->
            { currentModule
                | exposedTypes = currentModule.types
                , exposedAliases = currentModule.aliases
                , exposedConstructors = currentModule.constructors
                , exposedValues = currentModule.values
            }

        Explicit topLevelExposes ->
            List.foldl exposeHelp currentModule topLevelExposes


exposeHelp : Node TopLevelExpose -> Module -> Module
exposeHelp topLevelExpose currentModule =
    case Node.value topLevelExpose of
        InfixExpose name ->
            exposeBinop name currentModule

        FunctionExpose name ->
            exposeValue name currentModule

        TypeOrAliasExpose name ->
            currentModule
                |> exposeType name
                |> exposeAlias name

        TypeExpose exposedType ->
            case exposedType.open of
                Nothing ->
                    currentModule
                        |> exposeType exposedType.name

                Just _ ->
                    currentModule
                        |> exposeType exposedType.name
                        |> exposeConstructors exposedType.name


exposeBinop : String -> Module -> Module
exposeBinop name currentModule =
    case Dict.get name currentModule.binops of
        Nothing ->
            currentModule

        Just binop ->
            { currentModule
                | exposedBinops =
                    Dict.insert name binop currentModule.exposedBinops
            }


exposeValue : String -> Module -> Module
exposeValue name currentModule =
    case Dict.get name currentModule.values of
        Nothing ->
            currentModule

        Just annotation ->
            { currentModule
                | exposedValues =
                    Dict.insert name annotation currentModule.exposedValues
            }


exposeType : String -> Module -> Module
exposeType name currentModule =
    case Dict.get name currentModule.types of
        Nothing ->
            currentModule

        Just tipe ->
            { currentModule
                | exposedTypes =
                    Dict.insert name tipe currentModule.exposedTypes
            }


exposeAlias : String -> Module -> Module
exposeAlias name currentModule =
    case Dict.get name currentModule.aliases of
        Nothing ->
            currentModule

        Just alias_ ->
            { currentModule
                | exposedAliases =
                    Dict.insert name alias_ currentModule.exposedAliases
            }


exposeConstructors : String -> Module -> Module
exposeConstructors name currentModule =
    case Dict.get name currentModule.types of
        Nothing ->
            currentModule

        Just tipe ->
            { currentModule
                | exposedConstructors =
                    List.foldl
                        (\tag ->
                            case Dict.get tag currentModule.constructors of
                                Nothing ->
                                    identity

                                Just constructor ->
                                    Dict.insert tag constructor
                        )
                        currentModule.exposedConstructors
                        tipe.tags
            }



---- ADD IMPORTS


type alias Imports =
    { binops : Local Binop
    , qualifiedTypes : Qualified Type
    , qualifiedAliases : Qualified Alias
    , qualifiedConstructors : Qualified Constructor
    , qualifiedValues : Qualified Annotation
    }


addImports : Dict ModuleName Module -> List (Node Import) -> Imports
addImports importedModules =
    let
        initialImports =
            { binops = Dict.empty
            , qualifiedTypes =
                Dict.singleton []
                    (Dict.singleton "List"
                        { moduleName = [ "List" ]
                        , vars = [ "a" ]
                        , tags = []
                        }
                    )
            , qualifiedAliases = Dict.empty
            , qualifiedConstructors = Dict.empty
            , qualifiedValues = Dict.empty
            }
    in
    List.foldl
        (\importStatement imports ->
            let
                moduleName =
                    importStatement
                        |> Node.value
                        |> .moduleName
                        |> Node.value
            in
            case Dict.get moduleName importedModules of
                Nothing ->
                    imports

                Just importedModule ->
                    addImport importStatement moduleName importedModule imports
        )
        initialImports


addImport : Node Import -> ModuleName -> Module -> Imports -> Imports
addImport importStatement moduleName importedModule imports =
    let
        qualifier =
            moduleAlias
                |> Maybe.map Node.value
                |> Maybe.withDefault moduleName

        { moduleAlias, exposingList } =
            Node.value importStatement
    in
    imports
        |> addTypes qualifier importedModule
        |> addAliases qualifier importedModule
        |> addConstructors qualifier importedModule
        |> addValues qualifier importedModule
        |> addExposings qualifier importedModule exposingList


addExposings : ModuleName -> Module -> Maybe (Node Exposing) -> Imports -> Imports
addExposings qualifier importedModule exposingList imports =
    case exposingList of
        Nothing ->
            imports

        Just exposings ->
            case Node.value exposings of
                All _ ->
                    imports
                        |> addTypes [] importedModule
                        |> addAliases [] importedModule
                        |> addConstructors [] importedModule
                        |> addValues [] importedModule
                        |> addBinops importedModule

                Explicit topLevelExposes ->
                    List.foldl (addExposing [] importedModule)
                        imports
                        topLevelExposes


addExposing : ModuleName -> Module -> Node TopLevelExpose -> Imports -> Imports
addExposing qualifier importedModule topLevelExpose imports =
    case Node.value topLevelExpose of
        InfixExpose name ->
            addBinop (String.dropLeft 1 (String.dropRight 1 name))
                importedModule
                imports

        FunctionExpose name ->
            addValue name qualifier importedModule imports

        TypeOrAliasExpose name ->
            imports
                |> addType name qualifier importedModule
                |> addAlias name qualifier importedModule

        TypeExpose exposedType ->
            case exposedType.open of
                Nothing ->
                    imports
                        |> addType exposedType.name qualifier importedModule

                Just _ ->
                    imports
                        |> addType exposedType.name qualifier importedModule
                        |> addConstructorsFor exposedType.name
                            qualifier
                            importedModule


addTypes : ModuleName -> Module -> Imports -> Imports
addTypes qualifier { exposedTypes } ({ qualifiedTypes } as imports) =
    { imports
        | qualifiedTypes = addQualified qualifier exposedTypes qualifiedTypes
    }


addType : String -> ModuleName -> Module -> Imports -> Imports
addType name qualifer { exposedTypes } ({ qualifiedTypes } as imports) =
    case Dict.get name exposedTypes of
        Nothing ->
            imports

        Just tipe ->
            { imports
                | qualifiedTypes =
                    addSingleQualified qualifer name tipe qualifiedTypes
            }


addConstructorsFor : String -> ModuleName -> Module -> Imports -> Imports
addConstructorsFor name qualifer { exposedTypes, exposedConstructors } ({ qualifiedConstructors } as imports) =
    case Dict.get name exposedTypes of
        Nothing ->
            imports

        Just tipe ->
            { imports
                | qualifiedConstructors =
                    List.foldl
                        (\tag ->
                            case Dict.get tag exposedConstructors of
                                Nothing ->
                                    identity

                                Just constructor ->
                                    addSingleQualified qualifer tag constructor
                        )
                        qualifiedConstructors
                        tipe.tags
            }


addAliases : ModuleName -> Module -> Imports -> Imports
addAliases qualifier { exposedAliases } ({ qualifiedAliases } as imports) =
    { imports
        | qualifiedAliases = addQualified qualifier exposedAliases qualifiedAliases
    }


addAlias : String -> ModuleName -> Module -> Imports -> Imports
addAlias name qualifer { exposedAliases } ({ qualifiedAliases } as imports) =
    case Dict.get name exposedAliases of
        Nothing ->
            imports

        Just tipe ->
            { imports
                | qualifiedAliases =
                    addSingleQualified qualifer name tipe qualifiedAliases
            }


addConstructors : ModuleName -> Module -> Imports -> Imports
addConstructors qualifier { exposedConstructors } ({ qualifiedConstructors } as imports) =
    { imports
        | qualifiedConstructors =
            addQualified qualifier exposedConstructors qualifiedConstructors
    }


addValues : ModuleName -> Module -> Imports -> Imports
addValues qualifier { exposedValues } ({ qualifiedValues } as imports) =
    { imports
        | qualifiedValues = addQualified qualifier exposedValues qualifiedValues
    }


addValue : String -> ModuleName -> Module -> Imports -> Imports
addValue name qualifer { exposedValues } ({ qualifiedValues } as imports) =
    case Dict.get name exposedValues of
        Nothing ->
            imports

        Just annotation ->
            { imports
                | qualifiedValues =
                    addSingleQualified qualifer name annotation qualifiedValues
            }


addQualified : ModuleName -> Exposed a -> Qualified a -> Qualified a
addQualified qualifier exposed qualified =
    Dict.update qualifier (merge exposed) qualified


addSingleQualified : ModuleName -> String -> a -> Qualified a -> Qualified a
addSingleQualified qualifer name a =
    Dict.update qualifer
        (\maybeDictA ->
            case maybeDictA of
                Nothing ->
                    Just (Dict.singleton name a)

                Just dictA ->
                    Just (Dict.insert name a dictA)
        )


addBinops : Module -> Imports -> Imports
addBinops { exposedBinops } imports =
    { imports | binops = Dict.union imports.binops exposedBinops }


addBinop : String -> Module -> Imports -> Imports
addBinop name { exposedBinops } imports =
    case Dict.get name exposedBinops of
        Nothing ->
            imports

        Just binop ->
            { imports | binops = Dict.insert name binop imports.binops }


merge : Dict comparable a -> Maybe (Dict comparable a) -> Maybe (Dict comparable a)
merge dictA maybeDictA =
    case maybeDictA of
        Nothing ->
            Just dictA

        Just originalDictA ->
            Just (Dict.union originalDictA dictA)



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
            if Set.member moduleData.moduleName coreModules then
                List.map (.moduleName >> Node.value) moduleData.imports
                    |> List.filter (not << isNative)
                    |> Set.fromList

            else
                List.map (.moduleName >> Node.value)
                    (moduleData.imports
                        ++ List.map Node.value defaultImports
                    )
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


coreModules : Set ModuleName
coreModules =
    Set.fromList
        [ [ "Array" ]
        , [ "Basics" ]
        , [ "Bitwise" ]
        , [ "Char" ]
        , [ "Debug" ]
        , [ "Dict" ]
        , [ "List" ]
        , [ "Maybe" ]
        , [ "Platform" ]
        , [ "Platform", "Cmd" ]
        , [ "Platform", "Sub" ]
        , [ "Process" ]
        , [ "Result" ]
        , [ "Set" ]
        , [ "String" ]
        , [ "Task" ]
        , [ "Tuple" ]
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
                        [ Node emptyRange (TypeOrAliasExpose "Program") ]
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
