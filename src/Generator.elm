module Generator exposing
    ( Generator, Expr
    , addUnions, addAliases, addValues, takeValues, default, for
    , value, call
    , tuple, cases
    , record, recordUpdate, field, accessor
    , all
    , first, firstN
    , exprToString, exprToText
    , constructor, jsonDecoder, todo
    )

{-|

@docs Generator, Expr
@docs addUnions, addAliases, addValues, takeValues, default, for

@docs value, call
@docs tuple, cases
@docs record, recordUpdate, field, accessor
@docs all
@docs first, firstN

@docs exprToString, exprToText

-}

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

import BranchedState exposing (BranchedState)
import Canonical exposing (Alias, Union)
import Canonical.Annotation exposing (Annotation(..))
import Canonical.Type exposing (Type(..))
import Dict exposing (Dict)
import Set exposing (Set)
import Solver exposing (Comparability(..), Unifiability(..))
import State exposing (State)
import String.Extra as String


{-| A `Generator` helps generate Elm expressions which satisfy some
given type. For example, if you are looking for an Elm expression of the type
`List String -> Int`, you could do the following:

    exprs =
        value
            |> addValues values
            |> for (Lambda (Type "List" [ Var "a" ]) (Type "Int" []))
            |> exprToText

    values =
        Dict.fromList
            [ ( "List.count"
              , Lambda (Type "List" [ Var "a" ]) (Type "Int" [])
              )
            ]

Then `exprs == [ "List.count" ]`. There are generators for values, function
calls, case expressions and tuples. You can also combine these generators.

-}
type Generator
    = Generator
        { transform : List (Dict String Annotation) -> List (Dict String Annotation)
        , generate : GeneratorConfig -> Type -> BranchedState GeneratorState Expr
        }


type alias GeneratorState =
    { count : Int
    , substitutions : Dict String Type
    }


type alias GeneratorConfig =
    { targetTypeVars : Set String
    , isRoot : Bool
    , unions : Dict String Union
    , aliases : Dict String Alias
    , values : List (Dict String Annotation)
    }


{-| Use `for` to generate `Expr`s from a `Generator`. You can turn these into
Elm code using `exprToString` or `exprToText`.
-}
type Expr
    = Call String (List Expr)
    | CreateRecord (List ( String, Expr ))
    | UpdateRecord String (List ( String, Expr ))
    | CreateTuple Expr Expr
    | Case Expr (List ( String, Expr ))
    | Pipe Expr Expr


{-| An example `Generator` which is a combination of all the ones which are
available. Take a look at its source code, to get an idea of what is possible.
-}
default : Generator
default =
    all
        [ jsonDecoder
        , recordUpdate <|
            all
                [ value
                , field
                ]
        , record <|
            all
                [ value
                , todo
                ]
        , value
        , constructor
        , field
        , accessor
        , tuple
            { first =
                all
                    [ recordUpdate value
                    , value
                    , call [ value ]
                    ]
            , second =
                all
                    [ recordUpdate value
                    , value
                    , call [ value ]
                    ]
            }
        , cases
            { matched =
                takeValues 1 <|
                    all
                        [ value
                        , field
                        , tuple
                            { first = value
                            , second = value
                            }
                        ]
            , branch =
                \_ ->
                    first <|
                        all
                            [ tuple
                                { first =
                                    all
                                        [ todo
                                        , value
                                        ]
                                , second =
                                    all
                                        [ value
                                        , call [ value ]
                                        ]
                                }
                            , value
                            ]
            }
        , cases
            { matched =
                takeValues 1 <|
                    all
                        [ value
                        , field
                        ]
            , branch =
                \_ ->
                    tuple
                        { first = todo
                        , second = value
                        }
            }
        , cases
            { matched =
                takeValues 1 <|
                    all
                        [ value
                        , field
                        ]
            , branch =
                \_ ->
                    todo
            }
        , call
            [ all
                [ value
                , field
                , accessor
                ]
            ]
        , call
            [ all
                [ value
                , field
                , accessor
                ]
            , all
                [ value
                , field
                , accessor
                ]
            ]
        ]


{-| If your `Generator` should potentially output case expression, you have to
add known custom types to it. For example

    generator =
        cases
            { matched = value
            , branch =
                \newValues ->
                    value
                        |> addValues newValues
            }
            |> addUnions
                [ { name = "Msg"
                  , comment = ""
                  , args = []
                  , tags =
                        [ ( "Tick", [ Type "Posix" [] ] )
                        , ( "NameChanged", [ Type "String" [] ] )
                        , ( "ValueChanged", [ Type "Int" [] ] )
                        ]
                  }
                ]

-}
addUnions : Dict String Union -> Generator -> Generator
addUnions newUnions (Generator stuff) =
    Generator
        { stuff
            | generate =
                \config ->
                    stuff.generate { config | unions = Dict.union newUnions config.unions }
        }


addAliases : Dict String Alias -> Generator -> Generator
addAliases newAliases (Generator stuff) =
    Generator
        { stuff
            | generate =
                \config ->
                    stuff.generate
                        { config | aliases = Dict.union newAliases config.aliases }
        }


{-| Add known values. The ones which are added last, will
be searched first. Also, `Generator`s like `all` or `cases` propagate their
known values to their children.
-}
addValues : Dict String Annotation -> Generator -> Generator
addValues newValues (Generator stuff) =
    Generator { stuff | transform = (::) newValues << stuff.transform }


{-| Drop known values which are further away then the provided value.
-}
takeValues : Int -> Generator -> Generator
takeValues distance (Generator stuff) =
    Generator { stuff | transform = List.take distance << stuff.transform }


{-| Generate all possible expressions which are of a certain type.
-}
for : Type -> Generator -> List Expr
for targetType (Generator stuff) =
    BranchedState.finalValues Nothing
        { count = 0
        , substitutions = Dict.empty
        }
        (stuff.generate
            { targetTypeVars = Canonical.Type.freeTypeVars targetType
            , isRoot = True
            , unions = Dict.empty
            , aliases = Dict.empty
            , values = stuff.transform []
            }
            targetType
        )


{-| -}
value : Generator
value =
    call []


{-| Generate a function call, where the arguments are generated using the
provided `Generators`.
-}
call : List Generator -> Generator
call argumentGenerators =
    Generator
        { transform = identity
        , generate =
            \config targetType ->
                let
                    collectScope valuesInScope =
                        valuesInScope
                            |> Dict.filter (\name _ -> isValue name)
                            |> Dict.toList
                            |> BranchedState.traverse collectValue

                    collectValue ( name, tipe ) =
                        instantiate tipe
                            |> BranchedState.andThen (collectArguments name)
                            |> BranchedState.join

                    collectArguments name instantiatedType =
                        case collectArgumentsHelp instantiatedType argumentGenerators [] of
                            Nothing ->
                                BranchedState.state []

                            Just ( arguments, substitutions ) ->
                                addSubstitutions substitutions
                                    |> BranchedState.map
                                        (\_ -> generateCall name arguments)

                    generateCall name arguments =
                        arguments
                            |> BranchedState.combine generateArgument
                            |> BranchedState.map (Call name << List.reverse)

                    generateArgument ( tipe, generator ) =
                        run generator { config | isRoot = False } tipe

                    collectArgumentsHelp tipe generators arguments =
                        case ( tipe, generators ) of
                            ( Lambda from to, generator :: rest ) ->
                                collectArgumentsHelp to rest <|
                                    (( from, generator ) :: arguments)

                            ( _, [] ) ->
                                case
                                    Solver.unifiability
                                        { typeA = tipe
                                        , typeB = targetType
                                        }
                                of
                                    NotUnifiable _ ->
                                        Nothing

                                    Unifiable comparability substitutions ->
                                        case comparability of
                                            TypesAreEqual ->
                                                Just ( arguments, substitutions )

                                            NotComparable ->
                                                if config.isRoot then
                                                    Nothing

                                                else
                                                    Just ( arguments, substitutions )

                                            TypeAIsMoreGeneral ->
                                                Just ( arguments, substitutions )

                                            TypeBIsMoreGeneral ->
                                                if targetTypeVarsBound substitutions then
                                                    Nothing

                                                else
                                                    Just ( arguments, substitutions )

                            _ ->
                                Nothing

                    targetTypeVarsBound =
                        targetTypeVarsBoundBy config.targetTypeVars
                in
                BranchedState.traverse collectScope config.values
        }


isValue : String -> Bool
isValue name =
    name
        |> String.split "."
        |> List.reverse
        |> List.head
        |> Maybe.andThen
            (String.toList
                >> List.head
                >> Maybe.map Char.isLower
            )
        |> Maybe.withDefault True


{-| -}
constructor : Generator
constructor =
    Generator
        { transform = identity
        , generate =
            \config targetType ->
                let
                    constructorOfTargetType ( name, annotation ) =
                        if not (isValue name) then
                            instantiate annotation
                                |> BranchedState.andThen
                                    (\tipe ->
                                        whenUnifiable config
                                            (\_ -> Call name [])
                                            { typeA = tipe
                                            , typeB = targetType
                                            }
                                    )

                        else
                            BranchedState.state []
                in
                BranchedState.traverse
                    (Dict.toList >> BranchedState.traverse constructorOfTargetType)
                    config.values
        }


{-| Use all of the given `Generators`.
-}
all : List Generator -> Generator
all generators =
    Generator
        { transform = identity
        , generate =
            \config targetType ->
                let
                    generateExprs generator =
                        run generator config targetType
                in
                BranchedState.traverse generateExprs generators
        }


{-| -}
first : Generator -> Generator
first =
    firstN 1


{-| -}
firstN : Int -> Generator -> Generator
firstN newLimit (Generator stuff) =
    Generator
        { stuff
            | generate =
                \config targetType ->
                    BranchedState.withLimit (Just newLimit) <|
                        stuff.generate config targetType
        }


{-| Generate a tuple.
-}
tuple : { first : Generator, second : Generator } -> Generator
tuple generator =
    Generator
        { transform = identity
        , generate =
            \config targetType ->
                case targetType of
                    Tuple (typeA :: typeB :: []) ->
                        let
                            toTuple exprA =
                                run generator.second config typeB
                                    |> BranchedState.map (CreateTuple exprA)
                        in
                        run generator.first config typeA
                            |> BranchedState.andThen toTuple

                    _ ->
                        BranchedState.state []
        }


{-| -}
record : Generator -> Generator
record generator =
    Generator
        { transform = identity
        , generate =
            \config targetType ->
                case targetType of
                    Record fields var ->
                        let
                            generateField ( fieldName, fieldType ) =
                                run generator { config | isRoot = False } fieldType
                                    |> BranchedState.map (Tuple.pair fieldName)
                        in
                        fields
                            |> BranchedState.combine generateField
                            |> BranchedState.map CreateRecord

                    _ ->
                        BranchedState.state []
        }


{-| -}
recordUpdate : Generator -> Generator
recordUpdate generator =
    Generator
        { transform = identity
        , generate =
            \config targetType ->
                case targetType of
                    Record fields var ->
                        let
                            ofTargetType ( name, annotation ) =
                                instantiate annotation
                                    |> BranchedState.map
                                        (\tipe ->
                                            case
                                                Solver.unifiability
                                                    { typeA = tipe
                                                    , typeB = targetType
                                                    }
                                            of
                                                NotUnifiable _ ->
                                                    Nothing

                                                Unifiable _ _ ->
                                                    Just name
                                        )

                            toRecordUpdate name =
                                fields
                                    |> BranchedState.traverse updateField
                                    |> BranchedState.map
                                        (UpdateRecord name << List.singleton)

                            updateField ( fieldName, tipe ) =
                                run generator config tipe
                                    |> BranchedState.map (Tuple.pair fieldName)
                        in
                        BranchedState.traverse
                            (\values ->
                                Dict.toList values
                                    |> BranchedState.combine ofTargetType
                                    |> BranchedState.map (List.filterMap identity)
                                    |> BranchedState.andThen
                                        (BranchedState.traverse toRecordUpdate)
                            )
                            config.values

                    _ ->
                        BranchedState.state []
        }


{-| -}
field : Generator
field =
    Generator
        { transform = identity
        , generate =
            \config targetType ->
                let
                    fieldsOfTargetType ( name, annotation ) =
                        instantiate annotation
                            |> BranchedState.andThen
                                (\tipe ->
                                    case tipe of
                                        Record fields _ ->
                                            BranchedState.traverse (ofTargetType name) fields

                                        _ ->
                                            BranchedState.state []
                                )

                    ofTargetType recordName ( fieldName, tipe ) =
                        let
                            expr =
                                Call (recordName ++ "." ++ fieldName) []
                        in
                        whenUnifiable config
                            (\_ -> expr)
                            { typeA = tipe
                            , typeB = targetType
                            }
                in
                BranchedState.traverse
                    (Dict.toList >> BranchedState.traverse fieldsOfTargetType)
                    config.values
        }


{-| -}
accessor : Generator
accessor =
    Generator
        { transform = identity
        , generate =
            \config targetType ->
                case targetType of
                    Lambda (Record fields var) to ->
                        let
                            ofToType ( fieldName, tipe ) =
                                let
                                    expr =
                                        Call ("." ++ fieldName) []
                                in
                                whenUnifiable config
                                    (\_ -> expr)
                                    { typeA = tipe
                                    , typeB = to
                                    }
                        in
                        BranchedState.traverse ofToType fields

                    _ ->
                        BranchedState.state []
        }


{-| -}
cases : { matched : Generator, branch : Dict String Annotation -> Generator } -> Generator
cases generator =
    Generator
        { transform = identity
        , generate =
            \config targetType ->
                let
                    (Generator stuffMatched) =
                        generator.matched

                    exprs =
                        config.unions
                            |> Dict.toList
                            |> BranchedState.traverse generateMatched
                            |> BranchedState.andThen generateCase

                    generateMatched ( name, union ) =
                        BranchedState.map (Tuple.pair union.constructors) <|
                            stuffMatched.generate
                                { config | values = stuffMatched.transform config.values }
                                (Type union.moduleName name (List.map Var union.vars))

                    generateCase ( constructors, matched ) =
                        constructors
                            |> BranchedState.combine generateBranch
                            |> BranchedState.map (Case matched)

                    generateBranch ( name, subTypes ) =
                        BranchedState.map (Tuple.pair (branch name subTypes)) <|
                            run (generator.branch (toNewValues subTypes))
                                { config | isRoot = False }
                                targetType

                    branch name subTypes =
                        if List.isEmpty subTypes then
                            name

                        else
                            String.join " "
                                (name :: List.map newValueFromType subTypes)

                    toNewValues types =
                        types
                            |> List.map toNewValue
                            |> Dict.fromList

                    toNewValue tipe =
                        ( newValueFromType tipe
                        , Canonical.Annotation.fromType tipe
                        )

                    newValueFromType tipe =
                        case tipe of
                            Type _ name _ ->
                                "new" ++ name

                            _ ->
                                "a"
                in
                case targetType of
                    Type _ _ _ ->
                        exprs

                    Tuple _ ->
                        exprs

                    Record _ _ ->
                        exprs

                    _ ->
                        BranchedState.state []
        }


{-| -}
todo : Generator
todo =
    Generator
        { transform = identity
        , generate =
            \config _ ->
                BranchedState.state
                    [ Call "Debug.todo" [ Call "\"implementation missing\"" [] ] ]
        }


{-| -}
jsonDecoder : Generator
jsonDecoder =
    customDecoder
        { unwrap =
            \tipe ->
                case tipe of
                    Type [ "Json", "Decode" ] "Decoder" [ decoded ] ->
                        Just decoded

                    _ ->
                        Nothing
        , wrap =
            \tipe ->
                Type [ "Json", "Decode" ] "Decoder" [ tipe ]
        , toCall =
            \name expr ->
                Call "Decode.required"
                    [ Call ("\"" ++ name ++ "\"") []
                    , expr
                    ]
        , toInit =
            \name exprs ->
                pipeline
                    (Call "Decode.succeed" [ Call name [] ])
                    exprs
        }


pipeline : Expr -> List Expr -> Expr
pipeline firstExpr restExprs =
    case restExprs of
        [] ->
            firstExpr

        nextExpr :: nextRestExprs ->
            List.foldl
                (\exprRight exprLeft -> Pipe exprLeft exprRight)
                (Pipe firstExpr nextExpr)
                nextRestExprs


{-| -}
customDecoder :
    { unwrap : Type -> Maybe Type
    , wrap : Type -> Type
    , toCall : String -> Expr -> Expr
    , toInit : String -> List Expr -> Expr
    }
    -> Generator
customDecoder ({ unwrap, wrap, toCall, toInit } as customization) =
    Generator
        { transform = identity
        , generate =
            \config targetType ->
                case unwrap targetType of
                    Just decoded ->
                        case decoded of
                            Record fields Nothing ->
                                let
                                    generateField ( fieldName, fieldType ) =
                                        wrap fieldType
                                            |> run (customDecoder customization)
                                                config
                                            |> BranchedState.map (toCall fieldName)

                                    maybeRecordName =
                                        config.aliases
                                            |> Dict.toList
                                            |> List.filterMap
                                                (\( name, alias_ ) ->
                                                    if alias_.tipe == decoded then
                                                        Just name

                                                    else
                                                        Nothing
                                                )
                                            |> List.head
                                in
                                case maybeRecordName of
                                    Nothing ->
                                        BranchedState.state []

                                    Just recordName ->
                                        BranchedState.combine generateField fields
                                            |> BranchedState.map (toInit recordName)

                            tipe ->
                                run value config targetType

                    Nothing ->
                        BranchedState.state []
        }



------ HELPER


run : Generator -> GeneratorConfig -> Type -> BranchedState GeneratorState Expr
run (Generator { transform, generate }) config tipe =
    let
        runHelp { substitutions } =
            generate
                { config | values = transform config.values }
                (Canonical.Type.apply substitutions tipe)
    in
    BranchedState.get
        |> BranchedState.andThen runHelp


whenUnifiable :
    GeneratorConfig
    -> (() -> a)
    ->
        { typeA : Type
        , typeB : Type
        }
    -> BranchedState GeneratorState a
whenUnifiable config func { typeA, typeB } =
    let
        targetTypeVarsBound =
            targetTypeVarsBoundBy config.targetTypeVars
    in
    case
        Solver.unifiability
            { typeA = typeA
            , typeB = typeB
            }
    of
        NotUnifiable _ ->
            BranchedState.state []

        Unifiable comparability substitutions ->
            case comparability of
                TypesAreEqual ->
                    addSubstitutions substitutions
                        |> BranchedState.map
                            func

                NotComparable ->
                    if config.isRoot then
                        BranchedState.state []

                    else
                        addSubstitutions substitutions
                            |> BranchedState.map
                                func

                TypeAIsMoreGeneral ->
                    addSubstitutions substitutions
                        |> BranchedState.map
                            func

                TypeBIsMoreGeneral ->
                    if targetTypeVarsBound substitutions then
                        BranchedState.state []

                    else
                        addSubstitutions substitutions
                            |> BranchedState.map
                                func


addSubstitutions : Dict String Type -> BranchedState GeneratorState ()
addSubstitutions newSubstitutions =
    BranchedState.get
        |> BranchedState.andThen
            (\currentState ->
                BranchedState.put
                    { currentState
                        | substitutions =
                            Dict.union
                                newSubstitutions
                                currentState.substitutions
                    }
            )


targetTypeVarsBoundBy : Set String -> Dict String Type -> Bool
targetTypeVarsBoundBy targetTypeVars bindTypeVariables =
    targetTypeVars
        |> Set.toList
        |> List.any (varBoundBy bindTypeVariables)


varBoundBy : Dict String Type -> String -> Bool
varBoundBy uncheckedBoundTypeVars varName =
    case Dict.get varName uncheckedBoundTypeVars of
        Nothing ->
            False

        Just varTipe ->
            case varTipe of
                Var newVarName ->
                    varBoundBy
                        (Dict.remove varName uncheckedBoundTypeVars)
                        newVarName

                _ ->
                    True


instantiate : Annotation -> BranchedState GeneratorState Type
instantiate (ForAll vars tipe) =
    BranchedState.get
        |> BranchedState.andThen
            (\currentState ->
                let
                    ( newCount, subst ) =
                        freshVars vars currentState.count Dict.empty
                in
                BranchedState.put { currentState | count = newCount }
                    |> BranchedState.map
                        (\_ -> Canonical.Type.apply subst tipe)
            )


freshVars : List String -> Int -> Dict String Type -> ( Int, Dict String Type )
freshVars vars count subst =
    case vars of
        [] ->
            ( count, subst )

        var :: rest ->
            let
                prefix =
                    if String.startsWith "comparable" var then
                        "comparable"

                    else if String.startsWith "number" var then
                        "number"

                    else
                        "a"
            in
            freshVars rest
                (count + 1)
                (Dict.insert var
                    (Var (prefix ++ String.fromInt count))
                    subst
                )



---- PRINT


{-| Turn an `Expr` into Elm code. This will use a lot of line breaks.
-}
exprToText : Bool -> Expr -> String
exprToText isArgument expr =
    exprToStringHelp True isArgument expr


{-| Turn an `Expr` into Elm code. This will use very few line breaks.
-}
exprToString : Bool -> Expr -> String
exprToString isArgument expr =
    exprToStringHelp False isArgument expr


exprToStringHelp : Bool -> Bool -> Expr -> String
exprToStringHelp addLinebreaks isArgument expr =
    case expr of
        Call name args ->
            let
                callString =
                    String.concat
                        [ String.join " "
                            (name
                                :: List.map (exprToStringHelp addLinebreaks True) args
                            )
                        ]
            in
            if isArgument && List.length args >= 1 then
                String.concat
                    [ "("
                    , callString
                    , ")"
                    ]

            else
                callString

        CreateRecord values ->
            let
                valueToString ( fieldName, tipe ) =
                    fieldName ++ " = " ++ exprToStringHelp addLinebreaks False tipe
            in
            String.concat
                [ "{ "
                , String.join "\n, " <|
                    List.map valueToString values
                , "\n}"
                ]

        UpdateRecord name values ->
            if List.isEmpty values then
                name

            else
                let
                    valueToString ( fieldName, tipe ) =
                        fieldName ++ " = " ++ exprToStringHelp addLinebreaks False tipe
                in
                String.concat
                    [ "{ "
                    , name
                    , " | "
                    , String.join " , " <|
                        List.map valueToString values
                    , " }"
                    ]

        CreateTuple exprA exprB ->
            if addLinebreaks then
                String.concat
                    [ "( "
                    , exprToStringHelp addLinebreaks False exprA
                    , "\n, "
                    , exprToStringHelp addLinebreaks False exprB
                    , "\n)"
                    ]

            else
                String.concat
                    [ "( "
                    , exprToStringHelp addLinebreaks False exprA
                    , ", "
                    , exprToStringHelp addLinebreaks False exprB
                    , " )"
                    ]

        Case matchedExpr branches ->
            let
                branchToString ( branch, branchExpr ) =
                    String.concat
                        [ branch
                        , " ->\n"
                        , indent (exprToStringHelp addLinebreaks False branchExpr)
                        ]
            in
            String.concat
                [ "case "
                , exprToStringHelp addLinebreaks False matchedExpr
                , " of\n"
                , indent <|
                    String.join "\n\n" <|
                        List.map branchToString branches
                ]

        Pipe leftExpr rightExpr ->
            String.concat
                [ exprToStringHelp addLinebreaks False leftExpr
                , if addLinebreaks then
                    "\n    |> "

                  else
                    " |> "
                , exprToStringHelp addLinebreaks False rightExpr
                ]


indent : String -> String
indent text =
    text
        |> String.lines
        |> List.map
            (\line ->
                if line == "" then
                    line

                else
                    "    " ++ line
            )
        |> String.join "\n"
