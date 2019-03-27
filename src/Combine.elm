module Combine exposing
    ( combine
    , combineWith
    )


combine : Maybe Int -> List (List a) -> List (List a)
combine limit pools =
    combineWith limit (\list _ -> List.map (\a -> ( a, () )) list) pools ()
        |> List.map Tuple.first


combineWith : Maybe Int -> (a -> s -> List ( b, s )) -> List a -> s -> List ( List b, s )
combineWith limit toPool inputs initialState =
    let
        limitOrInfinity =
            Maybe.withDefault (Basics.floor (1 / 0)) limit
    in
    if limitOrInfinity > 0 then
        combineHelp 0 toPool inputs initialState limitOrInfinity
            |> Tuple.first

    else
        []


combineHelp :
    Int
    -> (b -> s -> List ( a, s ))
    -> List b
    -> s
    -> Int
    -> ( List ( List a, s ), Int )
combineHelp totalCount toPool inputs currentState limit =
    case inputs of
        [] ->
            ( [ ( [], currentState ) ]
            , totalCount + 1
            )

        input :: otherInputs ->
            let
                collect exprs ( acc, count ) =
                    case exprs of
                        [] ->
                            ( acc, count )

                        ( expr, nextState ) :: otherExprs ->
                            let
                                ( nextCombinations, nextCount ) =
                                    combineHelp count toPool otherInputs nextState limit
                            in
                            if nextCount >= limit then
                                ( acc
                                    ++ List.map
                                        (\( nextExprs, finalState ) ->
                                            ( expr :: nextExprs, finalState )
                                        )
                                        nextCombinations
                                , nextCount
                                )

                            else
                                collect otherExprs
                                    ( acc
                                        ++ List.map
                                            (\( nextExprs, finalState ) ->
                                                ( expr :: nextExprs, finalState )
                                            )
                                            nextCombinations
                                    , nextCount
                                    )
            in
            collect (toPool input currentState) ( [], totalCount )
