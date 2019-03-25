module Combine exposing
    ( combine
    , combineWith
    )


combine : Maybe Int -> List (List a) -> List (List a)
combine limit pools =
    combineWith limit (\_ list -> List.map (\a -> ( a, () )) list) () pools
        |> List.map Tuple.first


combineWith : Maybe Int -> (s -> b -> List ( a, s )) -> s -> List b -> List ( List a, s )
combineWith limit toPool initialState inputs =
    let
        limitOrInfinity =
            Maybe.withDefault (Basics.floor (1 / 0)) limit
    in
    if limitOrInfinity > 0 then
        combineHelp 0 toPool initialState limitOrInfinity inputs
            |> Tuple.first

    else
        []


combineHelp :
    Int
    -> (s -> b -> List ( a, s ))
    -> s
    -> Int
    -> List b
    -> ( List ( List a, s ), Int )
combineHelp totalCount toPool currentState limit inputs =
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
                                    combineHelp count toPool nextState limit otherInputs
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
            collect (toPool currentState input) ( [], totalCount )
