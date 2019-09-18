module Canonical exposing
    ( Associativity(..)
    , Binop
    , Constructor(..)
    , Error(..)
    , Imports
    , Module
    , ModuleData
    , Store
    , TodoItem
    , Type(..)
    , add
    , addImports
    , canonicalizeModule
    , canonicalizeTypeAnnotation
    , emptyStore
    , errorToString
    , replace
    , requiredModules
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
import Elm.Syntax.TypeAlias as Src
import Elm.Syntax.TypeAnnotation as Src
import List.Extra as List
import Result.Extra as Result
import Set exposing (Set)



---- MODULE


type alias Module =
    { exposedTypes : Exposed Type
    , exposedConstructors : Exposed Constructor
    , exposedValues : Exposed Annotation
    , exposedBinops : Exposed Binop
    , types : Local Type
    , constructors : Local Constructor
    , values : Local Annotation
    , binops : Local Binop
    , qualifiedTypes : Qualified Type
    , qualifiedConstructors : Qualified Constructor
    , qualifiedValues : Qualified Annotation
    }


type alias Exposed a =
    Dict String a


type alias Local a =
    Dict String a


type alias Qualified a =
    Dict ModuleName (Dict String a)


type Type
    = Union ModuleName (List String) Can.Type
    | Alias ModuleName (List String) Can.Type


type Constructor
    = Constructor String (List Can.Type) Can.Type
    | RecordConstructor String Can.Type


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


type Error
    = UnknownType ModuleName Module ModuleName String


errorToString : Error -> String
errorToString error =
    let
        title text =
            "=== " ++ text ++ " " ++ String.repeat (75 - String.length text) "="

        wrap text =
            String.join "\n"
                [ ""
                , ""
                , ""
                , text
                ]
    in
    case error of
        UnknownType currentModuleName currentModule moduleName name ->
            wrap <|
                String.join "\n"
                    [ title "UNKNOWN TYPE"
                    , ""
                    , String.concat
                        [ "Could not find the type "
                        , String.join "." (moduleName ++ [ name ])
                        , " in the module "
                        , String.join "." currentModuleName
                        , "."
                        ]
                    , ""
                    , "Locally available types:"
                    , ""
                    , String.join "\n" (List.map (\s -> "  " ++ s) (Dict.keys currentModule.types))
                    , ""
                    , "Imported types:"
                    , ""
                    , String.join "\n" <|
                        (Dict.toList currentModule.qualifiedTypes
                            |> List.map
                                (\( qualifier, types ) ->
                                    String.concat
                                        [ "  From "
                                        , String.join "." qualifier
                                        , ": "
                                        , String.join ", " <|
                                            Dict.keys types
                                        ]
                                )
                        )
                    , ""
                    , ""
                    ]


canonicalizeModule :
    Dict ModuleName Module
    -> ModuleName
    -> File
    -> Interface
    -> Result Error Module
canonicalizeModule importedModules moduleName file interface =
    let
        initialModule =
            { exposedTypes = initialTypes
            , exposedConstructors = Dict.empty
            , exposedValues = Dict.empty
            , exposedBinops = Dict.empty
            , types = initialTypes
            , constructors = Dict.empty
            , values = Dict.empty
            , binops = imports.binops
            , qualifiedTypes = imports.qualifiedTypes
            , qualifiedConstructors = imports.qualifiedConstructors
            , qualifiedValues = imports.qualifiedValues
            }

        initialTypes =
            if moduleName == [ "List" ] then
                Dict.singleton "List" <|
                    Union [ "List" ] [] (Can.Type [ "List" ] "List" [ Can.Var "a" ])

            else
                Dict.empty

        imports =
            if Set.member moduleName coreModules then
                addImports importedModules file.imports

            else
                addImports importedModules (file.imports ++ defaultImports)

        exposingList =
            Elm.Syntax.Module.exposingList (Node.value file.moduleDefinition)
    in
    initialModule
        |> canonicalizeTypes moduleName file.declarations
        |> Result.andThen (canonicalizeConstructors moduleName file.declarations)
        |> Result.andThen (canonicalizeValues file.declarations moduleName)
        |> Result.map (canonicalizeBinops moduleName file.declarations)
        |> Result.map (expose exposingList)



-- CANONICALIZE TYPES


type SrcType
    = SrcUnion
        { name : String
        , tags : List String
        , vars : List String
        }
    | SrcAlias Src.TypeAlias


canonicalizeTypes : ModuleName -> List (Node Declaration) -> Module -> Result Error Module
canonicalizeTypes moduleName declarations currentModule =
    let
        step declaration =
            case Node.value declaration of
                CustomTypeDeclaration customType ->
                    Result.andThen <|
                        add config (Node.value customType.name) <|
                            SrcUnion
                                { name = Node.value customType.name
                                , tags =
                                    List.map
                                        (Node.value << .name << Node.value)
                                        customType.constructors
                                , vars = List.map Node.value customType.generics
                                }

                AliasDeclaration typeAlias ->
                    Result.andThen
                        (add config (Node.value typeAlias.name) (SrcAlias typeAlias))

                _ ->
                    identity

        config =
            { required = requiredTypes currentModule
            , process = processType moduleName currentModule
            }
    in
    declarations
        |> List.foldl step (Ok { todo = [], done = Dict.empty })
        |> Result.map (\{ done } -> { currentModule | types = done })


requiredTypes : Module -> SrcType -> Set String
requiredTypes currentModule srcType =
    case srcType of
        SrcUnion _ ->
            Set.empty

        SrcAlias typeAlias ->
            requiredTypesForTypeAnnotation currentModule typeAlias.typeAnnotation


requiredTypesForTypeAnnotation : Module -> Node Src.TypeAnnotation -> Set String
requiredTypesForTypeAnnotation currentModule typeAnnotation =
    case Node.value typeAnnotation of
        Src.GenericType var ->
            Set.empty

        Src.Typed qualifiedName typeAnnotations ->
            let
                ( qualifier, name ) =
                    Node.value qualifiedName
            in
            List.foldl (Set.union << requiredTypesForTypeAnnotation currentModule)
                (case Dict.get qualifier currentModule.qualifiedTypes of
                    Nothing ->
                        if List.isEmpty qualifier then
                            Set.singleton name

                        else
                            Set.empty

                    Just types ->
                        if Dict.member name types then
                            Set.empty

                        else
                            Set.singleton name
                )
                typeAnnotations

        Src.Unit ->
            Set.empty

        Src.Tupled typeAnnotations ->
            List.foldl (Set.union << requiredTypesForTypeAnnotation currentModule)
                Set.empty
                typeAnnotations

        Src.Record recordFields ->
            List.foldl
                (Set.union
                    << requiredTypesForTypeAnnotation currentModule
                    << Tuple.second
                    << Node.value
                )
                Set.empty
                recordFields

        Src.GenericRecord var recordFields ->
            List.foldl
                (Set.union
                    << requiredTypesForTypeAnnotation currentModule
                    << Tuple.second
                    << Node.value
                )
                Set.empty
                (Node.value recordFields)

        Src.FunctionTypeAnnotation from to ->
            Set.union
                (requiredTypesForTypeAnnotation currentModule from)
                (requiredTypesForTypeAnnotation currentModule to)


processType : ModuleName -> Module -> Local Type -> SrcType -> Result Error Type
processType moduleName currentModule localTypes srcType =
    case srcType of
        SrcUnion { name, tags, vars } ->
            Ok <|
                Union moduleName
                    tags
                    (Can.Type moduleName name (List.map Can.Var vars))

        SrcAlias { generics, typeAnnotation } ->
            Result.map (Alias moduleName (List.map Node.value generics)) <|
                canonicalizeTypeAnnotation moduleName currentModule localTypes typeAnnotation



-- CANONICALIZE CONSTRUCTORS


canonicalizeConstructors :
    ModuleName
    -> List (Node Declaration)
    -> Module
    -> Result Error Module
canonicalizeConstructors moduleName declarations currentModule =
    Result.map (\newConstructors -> { currentModule | constructors = newConstructors }) <|
        List.foldl
            (\constructor ->
                Result.andThen
                    (canonicalizeConstructor moduleName currentModule constructor)
            )
            (Ok currentModule.constructors)
            declarations


canonicalizeConstructor :
    ModuleName
    -> Module
    -> Node Declaration
    -> Local Constructor
    -> Result Error (Local Constructor)
canonicalizeConstructor moduleName currentModule declaration constructors =
    case Node.value declaration of
        CustomTypeDeclaration c ->
            let
                toConstructor valueConstructor =
                    Result.map (Tuple.pair (Node.value valueConstructor.name)) <|
                        Result.combine <|
                            List.map (canonicalizeTypeAnnotation moduleName currentModule Dict.empty)
                                valueConstructor.arguments

                tipe =
                    Can.Type moduleName (Node.value c.name) <|
                        List.map (Can.Var << Node.value) c.generics
            in
            Result.map
                (List.foldl
                    (\( name, args ) ->
                        Dict.insert name (Constructor (Node.value c.name) args tipe)
                    )
                    constructors
                )
                (Result.combine
                    (List.map (Node.value >> toConstructor) c.constructors)
                )

        AliasDeclaration typeAlias ->
            let
                name =
                    Node.value typeAlias.name
            in
            case Dict.get name currentModule.types of
                Nothing ->
                    Ok constructors

                Just (Union _ _ _) ->
                    Ok constructors

                Just (Alias _ _ ((Can.Record _ Nothing) as tipe)) ->
                    Ok (Dict.insert name (RecordConstructor name tipe) constructors)

                Just (Alias _ _ _) ->
                    Ok constructors

        _ ->
            Ok constructors



-- CANONICALIZE VALUES


canonicalizeValues : List (Node Declaration) -> ModuleName -> Module -> Result Error Module
canonicalizeValues declarations currentModuleName currentModule =
    Result.map (\newValues -> { currentModule | values = newValues }) <|
        List.foldl (\value -> Result.andThen (canonicalizeValue currentModuleName currentModule value))
            (Ok currentModule.values)
            declarations


canonicalizeValue :
    ModuleName
    -> Module
    -> Node Declaration
    -> Local Annotation
    -> Result Error (Local Annotation)
canonicalizeValue currentModuleName currentModule declaration values =
    case Node.value declaration of
        FunctionDeclaration f ->
            case f.signature of
                Nothing ->
                    Ok values

                Just signature ->
                    let
                        name =
                            f.declaration
                                |> Node.value
                                |> .name
                                |> Node.value
                    in
                    signature
                        |> Node.value
                        |> .typeAnnotation
                        |> canonicalizeTypeAnnotation currentModuleName currentModule Dict.empty
                        |> Result.map
                            (\tipe ->
                                Dict.insert name
                                    (Canonical.Annotation.fromType tipe)
                                    values
                            )

        _ ->
            Ok values



-- CANONICALIZE BINOPS


canonicalizeBinops : ModuleName -> List (Node Declaration) -> Module -> Module
canonicalizeBinops moduleName declarations currentModule =
    { currentModule
        | binops =
            List.foldl
                (canonicalizeBinop moduleName currentModule)
                currentModule.binops
                declarations
    }


canonicalizeBinop : ModuleName -> Module -> Node Declaration -> Local Binop -> Local Binop
canonicalizeBinop moduleName currentModule declaration binops =
    case Node.value declaration of
        InfixDeclaration i ->
            let
                function =
                    Node.value i.function
            in
            case Dict.get function currentModule.values of
                Nothing ->
                    binops

                Just annotation ->
                    Dict.insert (Node.value i.operator)
                        { function = function
                        , tipe = annotation
                        , precedence = Node.value i.precedence
                        , associativity =
                            case Node.value i.direction of
                                Src.Left ->
                                    Left

                                Src.Non ->
                                    Non

                                Src.Right ->
                                    Right
                        }
                        binops

        _ ->
            binops



-- CANONICALIZE TYPE ANNOTATION


canonicalizeTypeAnnotation :
    ModuleName
    -> Module
    -> Local Type
    -> Node Src.TypeAnnotation
    -> Result Error Can.Type
canonicalizeTypeAnnotation currentModuleName currentModule localTypes typeAnnotation =
    case Node.value typeAnnotation of
        Src.GenericType var ->
            Ok (Can.Var var)

        Src.Typed qualifiedName typeAnnotations ->
            let
                ( qualifier, name ) =
                    Node.value qualifiedName

                canonicalizeAlias vars tipe =
                    Result.map (substituteVars tipe vars) <|
                        Result.combine <|
                            List.map (canonicalizeTypeAnnotation currentModuleName currentModule localTypes)
                                typeAnnotations

                canonicalizeUnion moduleName =
                    Result.map (Can.Type moduleName name) <|
                        Result.combine <|
                            List.map (canonicalizeTypeAnnotation currentModuleName currentModule localTypes)
                                typeAnnotations

                withImported =
                    case lookupType currentModule qualifier name of
                        Nothing ->
                            Err (UnknownType currentModuleName currentModule qualifier name)

                        Just (Union moduleName _ _) ->
                            canonicalizeUnion moduleName

                        Just (Alias _ vars tipe) ->
                            canonicalizeAlias vars tipe
            in
            if List.isEmpty qualifier then
                case Dict.get name localTypes of
                    Nothing ->
                        withImported

                    Just (Union moduleName _ _) ->
                        canonicalizeUnion moduleName

                    Just (Alias _ vars tipe) ->
                        canonicalizeAlias vars tipe

            else
                withImported

        Src.Unit ->
            Ok (Can.Tuple [])

        Src.Tupled typeAnnotations ->
            Result.map Can.Tuple <|
                Result.combine <|
                    List.map (canonicalizeTypeAnnotation currentModuleName currentModule localTypes)
                        typeAnnotations

        Src.Record recordFields ->
            Result.map (\fields -> Can.Record fields Nothing) <|
                Result.combine <|
                    List.map (canonicalizeRecordField currentModuleName currentModule localTypes)
                        recordFields

        Src.GenericRecord var recordFields ->
            Result.map (\fields -> Can.Record fields (Just (Node.value var))) <|
                Result.combine <|
                    List.map (canonicalizeRecordField currentModuleName currentModule localTypes)
                        (Node.value recordFields)

        Src.FunctionTypeAnnotation from to ->
            Result.map2 Can.Lambda
                (canonicalizeTypeAnnotation currentModuleName currentModule localTypes from)
                (canonicalizeTypeAnnotation currentModuleName currentModule localTypes to)


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
            Can.Lambda (substituteVarsHelp subst from) (substituteVarsHelp subst to)

        Can.Var name ->
            case Dict.get name subst of
                Nothing ->
                    tipe

                Just newType ->
                    newType

        Can.Type moduleName name types ->
            Can.Type moduleName name (List.map (substituteVarsHelp subst) types)

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


canonicalizeRecordField :
    ModuleName
    -> Module
    -> Local Type
    -> Node Src.RecordField
    -> Result Error ( String, Can.Type )
canonicalizeRecordField currentModuleName currentModule localTypes recordField =
    let
        ( name, annotation ) =
            Node.value recordField
    in
    Result.map (Tuple.pair (Node.value name)) <|
        canonicalizeTypeAnnotation currentModuleName currentModule localTypes annotation


lookupValue : Module -> ModuleName -> String -> Maybe Annotation
lookupValue currentModule moduleName name =
    Dict.get moduleName currentModule.qualifiedValues
        |> Maybe.andThen (Dict.get name)


lookupType : Module -> ModuleName -> String -> Maybe Type
lookupType currentModule qualifier name =
    if List.isEmpty qualifier then
        case Dict.get name currentModule.types of
            Nothing ->
                Dict.get qualifier currentModule.qualifiedTypes
                    |> Maybe.andThen (Dict.get name)

            maybeType ->
                maybeType

    else
        Dict.get qualifier currentModule.qualifiedTypes
            |> Maybe.andThen (Dict.get name)



---- EXPOSE


expose : Exposing -> Module -> Module
expose exposingList currentModule =
    case exposingList of
        All _ ->
            { currentModule
                | exposedTypes = currentModule.types
                , exposedConstructors = currentModule.constructors
                , exposedValues = currentModule.values
                , exposedBinops = currentModule.binops
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
            exposeType name currentModule
                |> exposeRecordConstructorsFor name

        TypeExpose exposedType ->
            case exposedType.open of
                Nothing ->
                    currentModule
                        |> exposeType exposedType.name

                Just _ ->
                    currentModule
                        |> exposeType exposedType.name
                        |> exposeConstructorsFor exposedType.name


exposeBinop : String -> Module -> Module
exposeBinop name currentModule =
    case Dict.get name currentModule.binops of
        Nothing ->
            currentModule

        Just binop ->
            { currentModule
                | exposedBinops = Dict.insert name binop currentModule.exposedBinops
            }


exposeValue : String -> Module -> Module
exposeValue name currentModule =
    case Dict.get name currentModule.values of
        Nothing ->
            currentModule

        Just annotation ->
            { currentModule
                | exposedValues = Dict.insert name annotation currentModule.exposedValues
            }


exposeType : String -> Module -> Module
exposeType name currentModule =
    case Dict.get name currentModule.types of
        Nothing ->
            currentModule

        Just tipe ->
            { currentModule
                | exposedTypes = Dict.insert name tipe currentModule.exposedTypes
            }


exposeConstructorsFor : String -> Module -> Module
exposeConstructorsFor name currentModule =
    case Dict.get name currentModule.types of
        Nothing ->
            currentModule

        Just (Union _ _ _) ->
            { currentModule
                | exposedConstructors =
                    currentModule.constructors
                        |> Dict.filter (\_ -> isConstructorFor name)
                        |> Dict.foldl Dict.insert currentModule.exposedConstructors
            }

        Just (Alias _ _ _) ->
            currentModule


exposeRecordConstructorsFor : String -> Module -> Module
exposeRecordConstructorsFor name currentModule =
    case Dict.get name currentModule.types of
        Nothing ->
            currentModule

        Just (Union _ _ _) ->
            currentModule

        Just (Alias _ _ _) ->
            { currentModule
                | exposedConstructors =
                    currentModule.constructors
                        |> Dict.filter (\_ -> isRecordConstructorFor name)
                        |> Dict.foldl Dict.insert currentModule.exposedConstructors
            }


isConstructorFor : String -> Constructor -> Bool
isConstructorFor name constructor =
    case constructor of
        Constructor thisName _ _ ->
            name == thisName

        RecordConstructor _ _ ->
            False


isRecordConstructorFor : String -> Constructor -> Bool
isRecordConstructorFor name constructor =
    case constructor of
        Constructor _ _ _ ->
            False

        RecordConstructor thisName _ ->
            name == thisName



---- ADD IMPORTS


type alias Imports =
    { binops : Local Binop
    , qualifiedTypes : Qualified Type
    , qualifiedConstructors : Qualified Constructor
    , qualifiedValues : Qualified Annotation
    }


addImports : Dict ModuleName Module -> List (Node Import) -> Imports
addImports importedModules =
    let
        initialImports =
            { binops = Dict.empty
            , qualifiedTypes =
                Dict.singleton [] <|
                    Dict.singleton "List" <|
                        Union [ "List" ] [] (Can.Type [ "List" ] "List" [ Can.Var "a" ])
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
            addType name qualifier importedModule imports

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
    { imports | qualifiedTypes = addQualified qualifier exposedTypes qualifiedTypes }


addType : String -> ModuleName -> Module -> Imports -> Imports
addType name qualifer { exposedTypes } ({ qualifiedTypes } as imports) =
    case Dict.get name exposedTypes of
        Nothing ->
            imports

        Just tipe ->
            { imports
                | qualifiedTypes = addSingleQualified qualifer name tipe qualifiedTypes
            }


addConstructorsFor : String -> ModuleName -> Module -> Imports -> Imports
addConstructorsFor name qualifer { exposedTypes, exposedConstructors } ({ qualifiedConstructors } as imports) =
    case Dict.get name exposedTypes of
        Nothing ->
            imports

        Just _ ->
            { imports
                | qualifiedConstructors =
                    (exposedConstructors
                        |> Dict.filter (\_ -> isConstructorFor name)
                        |> Dict.foldl Dict.insert Dict.empty
                        |> addQualified qualifer
                    )
                        qualifiedConstructors
            }


addConstructors : ModuleName -> Module -> Imports -> Imports
addConstructors qualifier { exposedConstructors } ({ qualifiedConstructors } as imports) =
    { imports
        | qualifiedConstructors =
            addQualified qualifier exposedConstructors qualifiedConstructors
    }


addValues : ModuleName -> Module -> Imports -> Imports
addValues qualifier { exposedValues } ({ qualifiedValues } as imports) =
    { imports | qualifiedValues = addQualified qualifier exposedValues qualifiedValues }


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
    Dict.update qualifer <|
        \maybeDictA ->
            case maybeDictA of
                Nothing ->
                    Just (Dict.singleton name a)

                Just dictA ->
                    Just (Dict.insert name a dictA)


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


requiredModules : ModuleData -> Set ModuleName
requiredModules moduleData =
    let
        isNative moduleName =
            case moduleName of
                "Native" :: _ ->
                    True

                "Elm" :: "Kernel" :: _ ->
                    True

                _ ->
                    False
    in
    if Set.member moduleData.moduleName coreModules then
        List.map (.moduleName >> Node.value) moduleData.imports
            |> List.filter (not << isNative)
            |> Set.fromList

    else
        List.map (.moduleName >> Node.value)
            (moduleData.imports ++ List.map Node.value defaultImports)
            |> List.filter (not << isNative)
            |> Set.fromList



---- STORE


type alias Store comparable input output =
    { todo : List (TodoItem comparable input)
    , done : Dict comparable output
    }


emptyStore : Store comparable input output
emptyStore =
    { todo = []
    , done = Dict.empty
    }


type alias TodoItem comparable input =
    { name : comparable
    , blockedBy : Set comparable
    , input : input
    }


type alias StoreConfig comparable input output =
    { required : input -> Set comparable
    , process : Dict comparable output -> input -> Result Error output
    }


add :
    StoreConfig comparable input output
    -> comparable
    -> input
    -> Store comparable input output
    -> Result Error (Store comparable input output)
add { required, process } name input store =
    let
        available =
            Dict.keys store.done
                |> Set.fromList
    in
    { store
        | todo =
            { name = name
            , blockedBy = Set.diff (required input) available
            , input = input
            }
                :: store.todo
    }
        |> processTodo process Set.empty


replace :
    StoreConfig comparable input output
    -> comparable
    -> input
    -> Store comparable input output
    -> Result Error (Store comparable input output)
replace { required, process } name input store =
    let
        available =
            Dict.keys store.done
                |> Set.fromList
    in
    { store
        | todo =
            { name = name
            , blockedBy = Set.diff (required input) available
            , input = input
            }
                :: store.todo
        , done = Dict.remove name store.done
    }
        |> processTodo process Set.empty


processTodo :
    (Dict comparable output -> input -> Result Error output)
    -> Set comparable
    -> Store comparable input output
    -> Result Error (Store comparable input output)
processTodo process nowDone store =
    let
        processTodoItem todoItem stuff =
            case stuff of
                Err _ ->
                    stuff

                Ok ( done, todo ) ->
                    let
                        newBlockers =
                            Set.diff todoItem.blockedBy nowDone
                    in
                    if Set.isEmpty newBlockers then
                        case process (Dict.union store.done done) todoItem.input of
                            Err error ->
                                Err error

                            Ok output ->
                                Ok ( Dict.insert todoItem.name output done, todo )

                    else
                        Ok ( done, { todoItem | blockedBy = newBlockers } :: todo )
    in
    case List.foldl processTodoItem (Ok ( Dict.empty, [] )) store.todo of
        Err error ->
            Err error

        Ok ( newlyDone, newTodo ) ->
            let
                newStore =
                    { store | todo = newTodo, done = Dict.union store.done newlyDone }
            in
            if Dict.isEmpty newlyDone then
                Ok newStore

            else
                processTodo process (Set.fromList (Dict.keys newlyDone)) newStore



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
                    Explicit [ Node emptyRange (TypeOrAliasExpose "String") ]
      }
    , { moduleName = Node emptyRange [ "Char" ]
      , moduleAlias = Nothing
      , exposingList =
            Just <|
                Node emptyRange <|
                    Explicit [ Node emptyRange (TypeOrAliasExpose "Char") ]
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
                    Explicit [ Node emptyRange (TypeOrAliasExpose "Program") ]
      }
    , { moduleName = Node emptyRange [ "Platform", "Cmd" ]
      , moduleAlias = Just (Node emptyRange [ "Cmd" ])
      , exposingList =
            Just <|
                Node emptyRange <|
                    Explicit [ Node emptyRange (TypeOrAliasExpose "Cmd") ]
      }
    , { moduleName = Node emptyRange [ "Platform", "Sub" ]
      , moduleAlias = Just (Node emptyRange [ "Sub" ])
      , exposingList =
            Just <|
                Node emptyRange <|
                    Explicit [ Node emptyRange (TypeOrAliasExpose "Sub") ]
      }
    ]
        |> List.map (Node emptyRange)
