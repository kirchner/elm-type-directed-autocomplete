module Generator exposing
    ( Generator, Expr
    , addUnions, addValues, takeValues, default, for
    , value, call
    , tuple, cases
    , recordUpdate, field, accessor
    , all, first
    , exprToString, exprToText
    )

{-|

@docs Generator, Expr
@docs addUnions, addValues, takeValues, default, for

@docs value, call
@docs tuple, cases
@docs recordUpdate, field, accessor
@docs all, first

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
        (List (Dict String Type) -> List (Dict String Type))
        (GenerateConfig
         -> GenerateState
         -> Type
         -> List ( Expr, ( Substitutions, GenerateState ) )
        )


type alias GenerateState =
    { count : Int }


type alias GenerateConfig =
    { targetTypeVars : Set String
    , isRoot : Bool
    , unions : List Union
    , values : List (Dict String Type)
    }


{-| Use `for` to generate `Expr`s from a `Generator`. You can turn these into
Elm code using `exprToString` or `exprToText`.
-}
type Expr
    = Call String (List Expr)
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
addUnions newUnions (Generator transformValues generator) =
    Generator transformValues <|
        \config ->
            generator { config | unions = newUnions ++ config.unions }


{-| Add known values. The ones which are added last, will
be searched first. Also, `Generator`s like `all` or `cases` propagate their
known values to their children.
-}
addValues : Dict String Type -> Generator -> Generator
addValues newValues (Generator transformValues generator) =
    Generator ((::) newValues << transformValues) generator


{-| Drop known values which are further away then the provided value.
-}
takeValues : Int -> Generator -> Generator
takeValues distance (Generator transformValues generator) =
    Generator (List.take distance << transformValues) generator


{-| Generate all possible expressions which are of a certain type.
-}
for : Type -> Generator -> List Expr
for targetType (Generator transformValues generator) =
    List.map Tuple.first <|
        generator
            { targetTypeVars = Type.typeVariables targetType
            , isRoot = True
            , unions = []
            , values = transformValues []
            }
            { count = 0 }
            targetType


{-| -}
value : Generator
value =
    call []


{-| Generate a function call, where the arguments are generated using the
provided `Generators`.
-}
call : List Generator -> Generator
call argumentGenerators =
    Generator identity <|
        \config state targetType ->
            let
                collectScope valuesInScope calls =
                    Dict.foldl collectValue calls valuesInScope

                collectValue name tipe calls =
                    let
                        ( instantiatedType, newCount ) =
                            instantiate state.count tipe
                    in
                    case collectArguments instantiatedType argumentGenerators [] of
                        Nothing ->
                            calls

                        Just ( arguments, substitutions ) ->
                            List.filterMap
                                (\( argumentExprs, finalState ) ->
                                    if
                                        List.length argumentExprs
                                            == List.length argumentGenerators
                                    then
                                        Just
                                            ( Call name (List.reverse argumentExprs)
                                            , ( substitutions
                                              , finalState
                                              )
                                            )

                                    else
                                        Nothing
                                )
                                (collectArgumentExprs
                                    { state | count = newCount }
                                    substitutions
                                    arguments
                                )
                                ++ calls

                collectArguments tipe generators arguments =
                    case ( tipe, generators ) of
                        ( Lambda from to, generator :: rest ) ->
                            collectArguments to rest <|
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

                collectArgumentExprs currentState substitutions arguments =
                    let
                        substitutedArguments =
                            List.map
                                (Tuple.mapFirst (Type.substitute substitutions))
                                arguments
                    in
                    case substitutedArguments of
                        [] ->
                            [ ( [], currentState ) ]

                        ( tipe, Generator transform generator ) :: rest ->
                            List.concatMap
                                (\( argumentExpr, ( nextSubstitutions, nextState ) ) ->
                                    List.map
                                        (\( restExprs, finalState ) ->
                                            ( argumentExpr :: restExprs
                                            , finalState
                                            )
                                        )
                                        (collectArgumentExprs
                                            nextState
                                            nextSubstitutions
                                            rest
                                        )
                                )
                                (generator
                                    { config
                                        | isRoot = False
                                        , values = transform config.values
                                    }
                                    currentState
                                    tipe
                                )
            in
            List.foldr collectScope [] config.values


{-| Use all of the given `Generators`.
-}
all : List Generator -> Generator
all generators =
    Generator identity <|
        \config state targetType ->
            List.concatMap
                (\(Generator transform generator) ->
                    generator
                        { config | values = transform config.values }
                        state
                        targetType
                )
                generators


{-| -}
first : List Generator -> Generator
first generators =
    Generator identity <|
        \config state targetType ->
            firstHelp config state targetType generators


firstHelp :
    GenerateConfig
    -> GenerateState
    -> Type
    -> List Generator
    -> List ( Expr, ( Substitutions, GenerateState ) )
firstHelp config state targetType generators =
    case generators of
        [] ->
            []

        (Generator transform generator) :: rest ->
            case
                generator
                    { config | values = transform config.values }
                    state
                    targetType
            of
                [] ->
                    firstHelp config state targetType rest

                result :: _ ->
                    [ result ]


{-| Generate a tuple.
-}
tuple : { first : Generator, second : Generator } -> Generator
tuple generator =
    Generator identity <|
        \config state targetType ->
            case targetType of
                Tuple (typeA :: typeB :: []) ->
                    let
                        (Generator firstTransform firstGenerator) =
                            generator.first

                        (Generator secondTransform secondGenerator) =
                            generator.second

                        toTuple ( exprA, ( _, nextState ) ) =
                            List.map (Tuple.mapFirst (CreateTuple exprA)) <|
                                secondGenerator
                                    { config | values = secondTransform config.values }
                                    nextState
                                    typeB
                    in
                    List.concatMap toTuple <|
                        firstGenerator
                            { config | values = firstTransform config.values }
                            state
                            typeA

                _ ->
                    []


{-| -}
recordUpdate : Generator -> Generator
recordUpdate (Generator transform generator) =
    Generator identity <|
        \config state targetType ->
            case targetType of
                Record fields var ->
                    let
                        ofTargetType name tipe collected =
                            Type.unifiability
                                { typeA = tipe
                                , typeB = targetType
                                }
                                |> collect name collected

                        collect name collected unifiability =
                            case unifiability of
                                Type.NotUnifiable ->
                                    collected

                                Type.Unifiable _ _ ->
                                    ( name, state ) :: collected

                        toRecordUpdate ( name, nextState ) =
                            fields
                                |> List.foldl (updateField nextState) []
                                |> List.map
                                    (\( fieldName, ( tipe, finalState ) ) ->
                                        ( UpdateRecord name [ ( fieldName, tipe ) ]
                                        , finalState
                                        )
                                    )

                        updateField nextState ( fieldName, tipe ) collected =
                            generator
                                { config | values = transform config.values }
                                nextState
                                tipe
                                |> List.map (Tuple.pair fieldName)
                                |> List.append collected
                    in
                    List.concatMap
                        (Dict.foldl ofTargetType [] >> List.concatMap toRecordUpdate)
                        config.values

                _ ->
                    []


{-| -}
field : Generator
field =
    Generator identity <|
        \config state targetType ->
            let
                fieldsOfTargetType name tipe collected =
                    case tipe of
                        Record fields _ ->
                            List.filterMap (ofTargetType name) fields
                                ++ collected

                        _ ->
                            collected

                ofTargetType recordName ( fieldName, tipe ) =
                    case
                        Type.unifiability
                            { typeA = tipe
                            , typeB = targetType
                            }
                    of
                        NotUnifiable ->
                            Nothing

                        Unifiable comparability substitutions ->
                            let
                                name =
                                    recordName ++ "." ++ fieldName
                            in
                            case comparability of
                                TypesAreEqual ->
                                    Just ( Call name [], ( substitutions, state ) )

                                NotComparable ->
                                    if config.isRoot then
                                        Nothing

                                    else
                                        Just ( Call name [], ( substitutions, state ) )

                                TypeAIsMoreGeneral ->
                                    Just ( Call name [], ( substitutions, state ) )

                                TypeBIsMoreGeneral ->
                                    if targetTypeVarsBound substitutions then
                                        Nothing

                                    else
                                        Just ( Call name [], ( substitutions, state ) )

                targetTypeVarsBound substitutions =
                    targetTypeVarsBoundBy
                        config.targetTypeVars
                        substitutions.bindTypeVariables
            in
            List.concatMap (Dict.foldl fieldsOfTargetType []) config.values


{-| -}
accessor : Generator
accessor =
    Generator identity <|
        \config state targetType ->
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
                                    Nothing

                                Unifiable comparability substitutions ->
                                    let
                                        name =
                                            "." ++ fieldName
                                    in
                                    case comparability of
                                        TypesAreEqual ->
                                            Just ( Call name [], ( substitutions, state ) )

                                        NotComparable ->
                                            if config.isRoot then
                                                Nothing

                                            else
                                                Just ( Call name [], ( substitutions, state ) )

                                        TypeAIsMoreGeneral ->
                                            Just ( Call name [], ( substitutions, state ) )

                                        TypeBIsMoreGeneral ->
                                            if targetTypeVarsBound substitutions then
                                                Nothing

                                            else
                                                Just ( Call name [], ( substitutions, state ) )

                        targetTypeVarsBound substitutions =
                            targetTypeVarsBoundBy
                                config.targetTypeVars
                                substitutions.bindTypeVariables
                    in
                    List.filterMap ofToType fields

                _ ->
                    []


{-| -}
cases : { matched : Generator, branch : Dict String Type -> Generator } -> Generator
cases generator =
    Generator identity <|
        \config state targetType ->
            let
                (Generator matchedTransform matchedGenerator) =
                    generator.matched

                matchedValues =
                    config.unions
                        |> List.concatMap suggestMatched
                        |> List.concatMap suggestCase
                        |> List.map
                            (\( expr, state_ ) ->
                                ( expr, ( Type.noSubstitutions, state_ ) )
                            )

                suggestMatched union =
                    List.map (Tuple.pair union.tags) <|
                        matchedGenerator
                            { config | values = matchedTransform config.values }
                            state
                            (Type union.name (List.map Var union.args))

                suggestCase ( tags, ( matched, ( _, nextState ) ) ) =
                    List.map (Tuple.mapFirst (Case matched))
                        (suggestBranches nextState tags)

                suggestBranches currentState tags =
                    case tags of
                        [] ->
                            [ ( [], currentState ) ]

                        ( name, subTypes ) :: restTags ->
                            let
                                branch =
                                    if List.isEmpty subTypes then
                                        name

                                    else
                                        String.join " "
                                            (name :: List.map newValueFromType subTypes)

                                (Generator transformValues branchGenerator) =
                                    generator.branch (toNewValues subTypes)
                            in
                            List.concatMap
                                (\( branchExpr, ( _, nextState ) ) ->
                                    List.map
                                        (\( branchExprs, finalState ) ->
                                            ( ( branch, branchExpr ) :: branchExprs
                                            , finalState
                                            )
                                        )
                                        (suggestBranches nextState restTags)
                                )
                                (branchGenerator
                                    { config
                                        | isRoot = False
                                        , values = transformValues config.values
                                    }
                                    currentState
                                    targetType
                                )

                suggestBranch nextState ( name, subTypes ) =
                    let
                        branch =
                            if List.isEmpty subTypes then
                                name

                            else
                                String.join " "
                                    (name :: List.map newValueFromType subTypes)

                        (Generator transformValues branchGenerator) =
                            generator.branch (toNewValues subTypes)
                    in
                    ( branch
                    , branchGenerator
                        { config | values = transformValues config.values }
                        nextState
                        targetType
                    )

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
                    matchedValues

                Tuple _ ->
                    matchedValues

                Record _ _ ->
                    matchedValues

                _ ->
                    []



------ HELPER


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


instantiate : Int -> Type -> ( Type, Int )
instantiate count tipe =
    instantiateHelp tipe
        |> State.run count


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
