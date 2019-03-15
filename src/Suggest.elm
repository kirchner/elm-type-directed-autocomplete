module Suggest exposing
    ( Expr
    , Generator
    , addUnions
    , addValues
    , all
    , call
    , cases
    , default
    , exprToString
    , exprToText
    , for
    , recordUpdate
    , takeValues
    , tuple
    , value
    )

import Dict exposing (Dict)
import Elm.Docs exposing (Alias, Module, Union)
import Elm.Type exposing (Type(..))
import Set
import State exposing (State)
import String.Extra as String
import Type exposing (Substitutions)


type Expr
    = Call String (List Expr)
    | UpdateRecord String (List ( String, Expr ))
    | CreateTuple Expr Expr
    | Case Expr (List ( String, Expr ))



---- GENERATOR


type Generator
    = Generator
        (List (Dict String Type) -> List (Dict String Type))
        (Type
         -> List Union
         -> List (Dict String Type)
         -> Substitutions
         -> List ( Expr, Substitutions )
        )


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


addUnions : List Union -> Generator -> Generator
addUnions newUnions (Generator transformValues generator) =
    Generator transformValues <|
        \targetType unions values ->
            generator targetType (newUnions ++ unions) values


addValues : Dict String Type -> Generator -> Generator
addValues newValues (Generator transformValues generator) =
    Generator ((::) newValues << transformValues) generator


addValuesList : List (Dict String Type) -> Generator -> Generator
addValuesList newValues (Generator transformValues generator) =
    Generator transformValues <|
        \targetType unions values ->
            generator targetType unions (newValues ++ values)


takeValues : Int -> Generator -> Generator
takeValues distance (Generator transformValues generator) =
    Generator (transformValues >> List.take distance) generator


for : Type -> Generator -> List Expr
for targetType generator =
    generator
        |> generate targetType Type.noSubstitutions
        |> List.map Tuple.first


generate : Type -> Substitutions -> Generator -> List ( Expr, Substitutions )
generate targetType substitutions (Generator transformValues generator) =
    generator targetType [] (transformValues []) substitutions


all : List Generator -> Generator
all generators =
    Generator identity <|
        \targetType unions values substitutions ->
            List.concatMap
                (\(Generator transformValues generator) ->
                    generator targetType unions (transformValues values) substitutions
                )
                generators


value : Generator
value =
    Generator identity <|
        \targetType unions values substitutions ->
            let
                ofTargetType name tipe collected =
                    Type.unifiable tipe targetType
                        |> State.run substitutions
                        |> collect name collected

                collect name collected ( isUnifiable, nextSubstitutions ) =
                    let
                        prevBoundVars =
                            Dict.keys substitutions.bindTypeVariables
                                |> Set.fromList

                        nextBoundVars =
                            Dict.keys nextSubstitutions.bindTypeVariables
                                |> Set.fromList

                        newBoundVars =
                            Set.diff nextBoundVars prevBoundVars

                        targetTypeVars =
                            Type.typeVariables targetType

                        newBoundVarsInTargetType =
                            targetTypeVars
                                |> Set.intersect newBoundVars
                                |> Set.isEmpty
                                |> not
                    in
                    if
                        isUnifiable
                            && (not newBoundVarsInTargetType
                                    || Set.isEmpty targetTypeVars
                                    || not (Set.isEmpty prevBoundVars)
                               )
                    then
                        ( Call name [], nextSubstitutions ) :: collected

                    else
                        collected
            in
            values
                |> List.concatMap (Dict.foldl ofTargetType [])


call : List Generator -> Generator
call generators =
    Generator identity <|
        \targetType unions values substitutions ->
            let
                ofTargetType name tipe collected =
                    case ofTargetTypeHelp [] generators tipe of
                        Nothing ->
                            collected

                        Just ( froms, to ) ->
                            ( name, froms, to ) :: collected

                ofTargetTypeHelp froms args tipe =
                    case ( args, tipe ) of
                        ( [], _ ) ->
                            let
                                ( isUnifiable, nextSubstitutions ) =
                                    Type.unifiable tipe targetType
                                        |> State.run substitutions

                                prevBoundVars =
                                    Dict.keys substitutions.bindTypeVariables
                                        |> Set.fromList

                                nextBoundVars =
                                    Dict.keys nextSubstitutions.bindTypeVariables
                                        |> Set.fromList

                                newBoundVars =
                                    Set.diff nextBoundVars prevBoundVars

                                targetTypeVars =
                                    Type.typeVariables targetType

                                newBoundVarsInTargetType =
                                    targetTypeVars
                                        |> Set.intersect newBoundVars
                                        |> Set.isEmpty
                                        |> not
                            in
                            if
                                isUnifiable
                                    && (not newBoundVarsInTargetType
                                            || Set.isEmpty targetTypeVars
                                            || not (Set.isEmpty prevBoundVars)
                                       )
                            then
                                Just ( froms, nextSubstitutions )

                            else
                                Nothing

                        ( firstArg :: restArgs, Lambda from to ) ->
                            ofTargetTypeHelp
                                (( from, firstArg ) :: froms)
                                restArgs
                                to

                        _ ->
                            Nothing

                toCall ( name, froms, nextSubstitutions ) =
                    suggestArguments [] froms nextSubstitutions
                        |> List.filterMap
                            (\( arguments, finalSubstitutions ) ->
                                if List.length arguments == List.length generators then
                                    Just
                                        ( Call name (List.reverse arguments)
                                        , finalSubstitutions
                                        )

                                else
                                    Nothing
                            )

                suggestArguments revArguments froms nextSubstitutions =
                    case froms of
                        [] ->
                            [ ( [], Type.noSubstitutions ) ]

                        ( firstFrom, Generator transform fromGenerator ) :: restFroms ->
                            List.concatMap
                                (\( firstArgumentExpr, nextNextSubstitutions ) ->
                                    List.map
                                        (\( restArgumentExprs, finalSubstitutions ) ->
                                            ( firstArgumentExpr :: restArgumentExprs
                                            , finalSubstitutions
                                            )
                                        )
                                        (suggestArguments
                                            (firstArgumentExpr :: revArguments)
                                            restFroms
                                            nextNextSubstitutions
                                        )
                                )
                                (fromGenerator firstFrom
                                    unions
                                    (transform values)
                                    nextSubstitutions
                                )
            in
            values
                |> List.concatMap
                    (Dict.foldl ofTargetType []
                        >> List.concatMap toCall
                    )


tuple : { first : Generator, second : Generator } -> Generator
tuple generator =
    Generator identity <|
        \targetType unions values substitutions ->
            case targetType of
                Tuple (typeA :: typeB :: []) ->
                    let
                        (Generator firstTransform firstGenerator) =
                            generator.first

                        (Generator secondTransform secondGenerator) =
                            generator.second

                        toTuple ( exprA, nextSubstitutions ) =
                            secondGenerator typeB
                                unions
                                (secondTransform values)
                                nextSubstitutions
                                |> List.map (Tuple.mapFirst (CreateTuple exprA))
                    in
                    firstGenerator typeA
                        unions
                        (firstTransform values)
                        substitutions
                        |> List.concatMap toTuple

                _ ->
                    []


recordUpdate : Generator -> Generator
recordUpdate (Generator transformValues fieldGenerator) =
    Generator identity <|
        \targetType unions values substitutions ->
            case targetType of
                Record fields var ->
                    let
                        ofTargetType name tipe collected =
                            Type.unifiable tipe targetType
                                |> State.run substitutions
                                |> collect name collected

                        collect name collected ( isUnifiable, nextSubstitutions ) =
                            if isUnifiable then
                                ( name, nextSubstitutions ) :: collected

                            else
                                collected

                        toRecordUpdate ( name, nextSubstitutions ) =
                            fields
                                |> List.foldl (updateField nextSubstitutions) []
                                |> List.map
                                    (\( field, ( tipe, finalSubstitutions ) ) ->
                                        ( UpdateRecord name [ ( field, tipe ) ]
                                        , finalSubstitutions
                                        )
                                    )

                        updateField nextSubstitutions ( field, tipe ) collected =
                            fieldGenerator tipe
                                unions
                                (transformValues values)
                                nextSubstitutions
                                |> List.map (Tuple.pair field)
                                |> List.append collected
                    in
                    values
                        |> List.concatMap
                            (Dict.foldl ofTargetType []
                                >> List.concatMap toRecordUpdate
                            )

                _ ->
                    []


cases :
    { matched : Generator
    , branch : Dict String Type -> Generator
    }
    -> Generator
cases generator =
    Generator identity <|
        \targetType unions values substitutions ->
            let
                matchedValues =
                    unions
                        |> List.concatMap suggestMatched
                        |> List.concatMap suggestCase
                        |> List.map (\expr -> ( expr, Type.noSubstitutions ))

                suggestMatched union =
                    generator.matched
                        |> addUnions unions
                        |> addValuesList values
                        |> generate (Type union.name (List.map Var union.args))
                            Type.noSubstitutions
                        |> List.map (Tuple.first >> Tuple.pair union.tags)

                suggestCase ( tags, matched ) =
                    tags
                        |> List.map suggestBranch
                        |> namedCombinations
                        |> List.map (Case matched)

                suggestBranch ( name, subTypes ) =
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
                    , branchGenerator targetType
                        unions
                        (transformValues values)
                        Type.noSubstitutions
                        |> List.map Tuple.first
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



---- HELPER


namedCombinations : List ( String, List a ) -> List (List ( String, a ))
namedCombinations lists =
    case lists of
        [] ->
            [ [] ]

        ( name, first ) :: rest ->
            first
                |> List.concatMap
                    (\a ->
                        namedCombinations rest
                            |> List.map
                                (\combination ->
                                    ( name, a ) :: combination
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
