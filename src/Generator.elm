module Generator exposing
    ( Generator, Expr
    , addUnions, addValues, takeValues, default, for
    , value, call
    , tuple, cases
    , record, recordUpdate, field, accessor
    , all
    , first, firstN
    , exprToString, exprToText
    )

{-|

@docs Generator, Expr
@docs addUnions, addValues, takeValues, default, for

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
import Combine exposing (combineWith)
import Dict exposing (Dict)
import Elm.Docs exposing (Alias, Module, Union)
import Elm.Type exposing (Type(..))
import Set exposing (Set)
import State exposing (State)
import String.Extra as String
import Type exposing (Comparability(..), Substitutions, Unifiability(..))


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
        { transform : List (Dict String Type) -> List (Dict String Type)
        , generate : GeneratorConfig -> Type -> BranchedState GeneratorState Expr
        }


type alias GeneratorState =
    { count : Int
    , substitutions : Substitutions
    }


type alias GeneratorConfig =
    { targetTypeVars : Set String
    , isRoot : Bool
    , limit : Maybe Int
    , unions : List Union
    , aliases : List Alias
    , values : List (Dict String Type)
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


{-| An example `Generator` which is a combination of all the ones which are
available. Take a look at its source code, to get an idea of what is possible.
-}
default : Generator
default =
    all
        [ recordUpdate value
        , value
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
            { matched = value
            , branch =
                \newValues ->
                    all
                        [ recordUpdate <|
                            all
                                [ value
                                    |> addValues newValues
                                    |> takeValues 1
                                , call
                                    [ value
                                        |> addValues newValues
                                        |> takeValues 1
                                    ]
                                ]
                        , value
                        ]
            }
        , call [ value ]
        , call [ value, value ]
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
addUnions : List Union -> Generator -> Generator
addUnions newUnions (Generator stuff) =
    Generator
        { stuff
            | generate =
                \config ->
                    stuff.generate { config | unions = newUnions ++ config.unions }
        }


{-| Add known values. The ones which are added last, will
be searched first. Also, `Generator`s like `all` or `cases` propagate their
known values to their children.
-}
addValues : Dict String Type -> Generator -> Generator
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
    BranchedState.finalValues
        { count = 0
        , substitutions = Type.noSubstitutions
        }
        (stuff.generate
            { targetTypeVars = Type.typeVariables targetType
            , isRoot = True
            , limit = Nothing
            , unions = []
            , aliases = []
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

                    generateArgument ( tipe, Generator { transform, generate } ) =
                        BranchedState.get
                            |> BranchedState.andThen
                                (\{ substitutions } ->
                                    generate
                                        { config
                                            | isRoot = False
                                            , values = transform config.values
                                        }
                                        (Type.substitute substitutions tipe)
                                )

                    collectArgumentsHelp tipe generators arguments =
                        case ( tipe, generators ) of
                            ( Lambda from to, generator :: rest ) ->
                                collectArgumentsHelp to rest <|
                                    (( from, generator ) :: arguments)

                            ( _, [] ) ->
                                case
                                    Type.unifiability
                                        { typeA = tipe
                                        , typeB = targetType
                                        }
                                of
                                    NotUnifiable ->
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

                    targetTypeVarsBound substitutions =
                        targetTypeVarsBoundBy
                            config.targetTypeVars
                            substitutions.bindTypeVariables
                in
                BranchedState.traverse collectScope config.values
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
                    generateExprs (Generator { transform, generate }) =
                        generate
                            { config | values = transform config.values }
                            targetType
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
                \config ->
                    stuff.generate { config | limit = Just newLimit }
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
                            (Generator stuffFirst) =
                                generator.first

                            (Generator stuffSecond) =
                                generator.second

                            toTuple exprA =
                                BranchedState.map (CreateTuple exprA) <|
                                    stuffSecond.generate
                                        { config
                                            | values =
                                                stuffSecond.transform config.values
                                        }
                                        typeB
                        in
                        BranchedState.andThen toTuple <|
                            stuffFirst.generate
                                { config | values = stuffFirst.transform config.values }
                                typeA

                    _ ->
                        BranchedState.state []
        }


{-| -}
record : Generator -> Generator
record (Generator stuff) =
    Generator
        { transform = identity
        , generate =
            \config targetType ->
                case targetType of
                    Record fields var ->
                        let
                            generateField ( fieldName, fieldType ) =
                                BranchedState.get
                                    |> BranchedState.andThen
                                        (\{ substitutions } ->
                                            stuff.generate
                                                { config
                                                    | isRoot = False
                                                    , values = stuff.transform config.values
                                                }
                                                (Type.substitute substitutions
                                                    fieldType
                                                )
                                        )
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
recordUpdate (Generator stuff) =
    Generator
        { transform = identity
        , generate =
            \config targetType ->
                case targetType of
                    Record fields var ->
                        let
                            ofTargetType name tipe collected =
                                collect name collected <|
                                    Type.unifiability
                                        { typeA = tipe
                                        , typeB = targetType
                                        }

                            collect name collected unifiability =
                                case unifiability of
                                    Type.NotUnifiable ->
                                        collected

                                    Type.Unifiable _ _ ->
                                        name :: collected

                            toRecordUpdate name =
                                fields
                                    |> BranchedState.traverse updateField
                                    |> BranchedState.map
                                        (UpdateRecord name << List.singleton)

                            updateField ( fieldName, tipe ) =
                                BranchedState.map (Tuple.pair fieldName) <|
                                    stuff.generate
                                        { config | values = stuff.transform config.values }
                                        tipe
                        in
                        BranchedState.traverse
                            (Dict.foldl ofTargetType []
                                >> BranchedState.traverse toRecordUpdate
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
                    fieldsOfTargetType ( name, tipe ) =
                        case tipe of
                            Record fields _ ->
                                BranchedState.traverse (ofTargetType name) fields

                            _ ->
                                BranchedState.state []

                    ofTargetType recordName ( fieldName, tipe ) =
                        case
                            Type.unifiability
                                { typeA = tipe
                                , typeB = targetType
                                }
                        of
                            NotUnifiable ->
                                BranchedState.state []

                            Unifiable comparability substitutions ->
                                let
                                    name =
                                        recordName ++ "." ++ fieldName
                                in
                                case comparability of
                                    TypesAreEqual ->
                                        addSubstitutions substitutions
                                            |> BranchedState.map
                                                (\_ -> Call name [])

                                    NotComparable ->
                                        if config.isRoot then
                                            BranchedState.state []

                                        else
                                            addSubstitutions substitutions
                                                |> BranchedState.map
                                                    (\_ -> Call name [])

                                    TypeAIsMoreGeneral ->
                                        addSubstitutions substitutions
                                            |> BranchedState.map
                                                (\_ -> Call name [])

                                    TypeBIsMoreGeneral ->
                                        if targetTypeVarsBound substitutions then
                                            BranchedState.state []

                                        else
                                            addSubstitutions substitutions
                                                |> BranchedState.map
                                                    (\_ -> Call name [])

                    targetTypeVarsBound substitutions =
                        targetTypeVarsBoundBy
                            config.targetTypeVars
                            substitutions.bindTypeVariables
                in
                BranchedState.traverse
                    (Dict.toList
                        >> BranchedState.traverse fieldsOfTargetType
                    )
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
                                case
                                    Type.unifiability
                                        { typeA = tipe
                                        , typeB = to
                                        }
                                of
                                    NotUnifiable ->
                                        BranchedState.state []

                                    Unifiable comparability substitutions ->
                                        let
                                            name =
                                                "." ++ fieldName
                                        in
                                        case comparability of
                                            TypesAreEqual ->
                                                addSubstitutions substitutions
                                                    |> BranchedState.map
                                                        (\_ -> Call name [])

                                            NotComparable ->
                                                if config.isRoot then
                                                    BranchedState.state []

                                                else
                                                    addSubstitutions substitutions
                                                        |> BranchedState.map
                                                            (\_ -> Call name [])

                                            TypeAIsMoreGeneral ->
                                                addSubstitutions substitutions
                                                    |> BranchedState.map
                                                        (\_ -> Call name [])

                                            TypeBIsMoreGeneral ->
                                                if targetTypeVarsBound substitutions then
                                                    BranchedState.state []

                                                else
                                                    addSubstitutions substitutions
                                                        |> BranchedState.map
                                                            (\_ -> Call name [])

                            targetTypeVarsBound substitutions =
                                targetTypeVarsBoundBy
                                    config.targetTypeVars
                                    substitutions.bindTypeVariables
                        in
                        BranchedState.traverse ofToType fields

                    _ ->
                        BranchedState.state []
        }


{-| -}
cases : { matched : Generator, branch : Dict String Type -> Generator } -> Generator
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
                            |> BranchedState.traverse generateMatched
                            |> BranchedState.andThen generateCase

                    generateMatched union =
                        BranchedState.map (Tuple.pair union.tags) <|
                            stuffMatched.generate
                                { config | values = stuffMatched.transform config.values }
                                (Type union.name (List.map Var union.args))

                    generateCase ( tags, matched ) =
                        tags
                            |> BranchedState.combine generateBranch
                            |> BranchedState.map (Case matched)

                    generateBranch ( name, subTypes ) =
                        let
                            (Generator stuffBranch) =
                                generator.branch (toNewValues subTypes)
                        in
                        BranchedState.map (Tuple.pair (branch name subTypes)) <|
                            stuffBranch.generate
                                { config
                                    | isRoot = False
                                    , values = stuffBranch.transform config.values
                                }
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
                        , tipe
                        )

                    newValueFromType tipe =
                        case tipe of
                            Type name _ ->
                                "new" ++ name

                            _ ->
                                "a"
                in
                case targetType of
                    Type _ _ ->
                        exprs

                    Tuple _ ->
                        exprs

                    Record _ _ ->
                        exprs

                    _ ->
                        BranchedState.state []
        }



------ HELPER


addSubstitutions : Substitutions -> BranchedState GeneratorState ()
addSubstitutions newSubstitutions =
    let
        add substitutions =
            { bindTypeVariables =
                Dict.union
                    newSubstitutions.bindTypeVariables
                    substitutions.bindTypeVariables
            , bindRecordVariables =
                Dict.union
                    newSubstitutions.bindRecordVariables
                    substitutions.bindRecordVariables
            }
    in
    BranchedState.get
        |> BranchedState.andThen
            (\currentState ->
                BranchedState.put
                    { currentState
                        | substitutions = add currentState.substitutions
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


instantiate : Type -> BranchedState GeneratorState Type
instantiate tipe =
    BranchedState.get
        |> BranchedState.andThen
            (\currentState ->
                let
                    ( newType, newCount ) =
                        instantiateHelp tipe
                            |> State.run currentState.count
                in
                BranchedState.put { currentState | count = newCount }
                    |> BranchedState.map
                        (\_ -> newType)
            )


instantiateHelp : Type -> State Int Type
instantiateHelp tipe =
    State.get
        |> State.andThen
            (\count ->
                let
                    oldTypeVars =
                        Type.typeVariables tipe

                    substitutions =
                        { bindTypeVariables =
                            Type.typeVariables tipe
                                |> Set.toList
                                |> List.indexedMap
                                    (\index name ->
                                        ( name
                                        , if String.startsWith "comparable" name then
                                            Var
                                                ("comparableA"
                                                    ++ String.fromInt
                                                        (index + count)
                                                )

                                          else if String.startsWith "number" name then
                                            Var
                                                ("numberA"
                                                    ++ String.fromInt
                                                        (index + count)
                                                )

                                          else
                                            Var
                                                ("a"
                                                    ++ String.fromInt
                                                        (index + count)
                                                )
                                        )
                                    )
                                |> Dict.fromList
                        , bindRecordVariables = Dict.empty
                        }
                in
                State.put (count + 1)
                    |> State.map
                        (\_ ->
                            Type.substitute substitutions tipe
                        )
            )



---- PRINT


{-| Turn an `Expr` into Elm code. This will use a lot of line breaks.
-}
exprToText : Expr -> String
exprToText expr =
    exprToStringHelp True False expr


{-| Turn an `Expr` into Elm code. This will use very few line breaks.
-}
exprToString : Expr -> String
exprToString expr =
    exprToStringHelp False False expr


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
