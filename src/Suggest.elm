module Suggest exposing
    ( Generator, Expr
    , addUnions, addValues, takeValues, default, for
    , value, call
    , tuple, recordUpdate, cases
    , all
    , exprToString, exprToText
    )

{-|

@docs Generator, Expr
@docs addUnions, addValues, takeValues, default, for

@docs value, call
@docs tuple, recordUpdate, cases
@docs all

@docs exprToString, exprToText

-}

import Dict exposing (Dict)
import Elm.Docs exposing (Alias, Module, Union)
import Elm.Type exposing (Type(..))
import Set exposing (Set)
import State exposing (State)
import String.Extra as String
import Type exposing (Substitutions)


{-| -}
type Expr
    = Call String (List Expr)
    | UpdateRecord String (List ( String, Expr ))
    | CreateTuple Expr Expr
    | Case Expr (List ( String, Expr ))



---- GENERATOR


{-| -}
type Generator
    = Generator
        (List (Dict String Type) -> List (Dict String Type))
        (GenerateConfig
         -> GenerateState
         -> Type
         -> List ( Expr, GenerateState )
        )


type alias GenerateState =
    { count : Int
    , substitutions : Substitutions
    }


type alias GenerateConfig =
    { targetTypeVars : Set String
    , isRoot : Bool
    , unions : List Union
    , values : List (Dict String Type)
    }


{-| -}
default : Generator
default =
    all
        [ recordUpdate (call [])
        , call []
        , tuple
            { first =
                all
                    [ recordUpdate (call [])
                    , call []
                    , call [ value ]
                    ]
            , second =
                all
                    [ recordUpdate (call [])
                    , call []
                    , call [ value ]
                    ]
            }
        , cases
            { matched = call []
            , branch =
                \newValues ->
                    all
                        [ recordUpdate <|
                            all
                                [ call []
                                    |> addValues newValues
                                    |> takeValues 1
                                , call
                                    [ value
                                        |> addValues newValues
                                        |> takeValues 1
                                    ]
                                ]
                        , call []
                        ]
            }
        , call [ value ]
        , call [ value, value ]
        ]


{-| -}
addUnions : List Union -> Generator -> Generator
addUnions newUnions (Generator transformValues generator) =
    Generator transformValues <|
        \config ->
            generator { config | unions = newUnions ++ config.unions }


{-| -}
addValues : Dict String Type -> Generator -> Generator
addValues newValues (Generator transformValues generator) =
    Generator ((::) newValues << transformValues) generator


{-| -}
takeValues : Int -> Generator -> Generator
takeValues distance (Generator transformValues generator) =
    Generator (List.take distance << transformValues) generator


{-| -}
for : Type -> Generator -> List Expr
for targetType (Generator transformValues generator) =
    List.map Tuple.first <|
        generator
            { targetTypeVars = Type.typeVariables targetType
            , isRoot = True
            , unions = []
            , values = transformValues []
            }
            { count = 0
            , substitutions = Type.noSubstitutions
            }
            targetType


{-| -}
value : Generator
value =
    call []


{-| -}
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
                    case
                        collectArguments { state | count = newCount }
                            tipe
                            argumentGenerators
                            []
                    of
                        Nothing ->
                            calls

                        Just ( arguments, nextState ) ->
                            List.filterMap
                                (\( argumentExprs, finalState ) ->
                                    if
                                        List.length argumentExprs
                                            == List.length argumentGenerators
                                    then
                                        Just
                                            ( Call name (List.reverse argumentExprs)
                                            , finalState
                                            )

                                    else
                                        Nothing
                                )
                                (collectArgumentExprs nextState arguments)
                                ++ calls

                collectArgumentExprs currentState arguments =
                    case arguments of
                        [] ->
                            [ ( [], currentState ) ]

                        ( tipe, Generator transform generator ) :: rest ->
                            List.concatMap
                                (\( argumentExpr, nextState ) ->
                                    List.map
                                        (\( restExprs, finalState ) ->
                                            ( argumentExpr :: restExprs
                                            , finalState
                                            )
                                        )
                                        (collectArgumentExprs nextState rest)
                                )
                                (generator
                                    { config
                                        | isRoot = False
                                        , values = transform config.values
                                    }
                                    currentState
                                    (Type.substitute currentState.substitutions tipe)
                                )

                collectArguments nextState tipe generators arguments =
                    case ( tipe, generators ) of
                        ( Lambda from to, generator :: rest ) ->
                            collectArguments nextState to rest <|
                                (( from, generator ) :: arguments)

                        ( _, [] ) ->
                            let
                                ( isGood, newSubstitutions ) =
                                    tipe
                                        |> Type.unifiable targetType
                                        |> State.run state.substitutions

                                targetTypeVarsBound =
                                    config.targetTypeVars
                                        |> Set.toList
                                        |> List.any varBound

                                varBound varName =
                                    case
                                        Dict.get varName newSubstitutions.bindTypeVariables
                                    of
                                        Nothing ->
                                            False

                                        Just varTipe ->
                                            case varTipe of
                                                Var newVarName ->
                                                    varBound newVarName

                                                _ ->
                                                    True
                            in
                            if isGood && not targetTypeVarsBound then
                                Just
                                    ( List.map
                                        (Tuple.mapFirst
                                            (Type.substitute newSubstitutions)
                                        )
                                        arguments
                                    , { nextState | substitutions = newSubstitutions }
                                    )

                            else
                                Nothing

                        _ ->
                            Nothing
            in
            List.foldr collectScope [] config.values


{-| -}
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

                        toTuple ( exprA, nextState ) =
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
                            Type.unifiable tipe targetType
                                |> State.run state.substitutions
                                |> collect name collected

                        collect name collected ( isUnifiable, nextSubstitutions ) =
                            if isUnifiable then
                                ( name, { state | substitutions = nextSubstitutions } )
                                    :: collected

                            else
                                collected

                        toRecordUpdate ( name, nextState ) =
                            fields
                                |> List.foldl (updateField nextState) []
                                |> List.map
                                    (\( field, ( tipe, finalState ) ) ->
                                        ( UpdateRecord name [ ( field, tipe ) ]
                                        , finalState
                                        )
                                    )

                        updateField nextState ( field, tipe ) collected =
                            generator
                                { config | values = transform config.values }
                                nextState
                                tipe
                                |> List.map (Tuple.pair field)
                                |> List.append collected
                    in
                    List.concatMap
                        (Dict.foldl ofTargetType [] >> List.concatMap toRecordUpdate)
                        config.values

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

                suggestMatched union =
                    List.map (Tuple.pair union.tags) <|
                        matchedGenerator
                            { config | values = matchedTransform config.values }
                            state
                            (Type union.name (List.map Var union.args))

                suggestCase ( tags, ( matched, nextState ) ) =
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
                                (\( branchExpr, nextState ) ->
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
                                        , Var
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


exprToText : Expr -> String
exprToText expr =
    exprToStringHelp True False expr


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
