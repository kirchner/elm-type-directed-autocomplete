module BranchedState exposing
    ( BranchedState
    , advance
    , andThen
    , combine
    , embed
    , finalValues
    , get
    , join
    , map
    , put
    , state
    , traverse
    , withLimit
    )

---- TYPE AND CONSTRUCTS


type BranchedState state value
    = BranchedState (Maybe Int -> state -> List ( value, state ))


state : List value -> BranchedState state value
state values =
    BranchedState <|
        \limit currentState ->
            limitMap limit (\value -> ( value, currentState )) values


embed : (s -> List a) -> BranchedState s a
embed computeA =
    BranchedState <|
        \limit currentState ->
            computeA currentState
                |> limitMap limit (\value -> ( value, currentState ))


advance : (s -> List ( a, s )) -> BranchedState s a
advance computeA =
    BranchedState <|
        \limit ->
            computeA >> take limit



---- MAPPING


map : (a -> b) -> BranchedState s a -> BranchedState s b
map func (BranchedState computeA) =
    BranchedState <|
        \limit currentState ->
            computeA limit currentState
                |> List.map (Tuple.mapFirst func)


map2 : (a -> b -> c) -> BranchedState s a -> BranchedState s b -> BranchedState s c
map2 func (BranchedState computeA) (BranchedState computeB) =
    BranchedState <|
        \limit currentState ->
            take limit <|
                limitConcatMap limit
                    (\( a, nextState ) ->
                        List.map (Tuple.mapFirst (func a))
                            (computeB limit nextState)
                    )
                    (computeA limit currentState)


map3 :
    (a -> b -> c -> d)
    -> BranchedState s a
    -> BranchedState s b
    -> BranchedState s c
    -> BranchedState s d
map3 func stepA stepB stepC =
    map func stepA
        |> andMap stepB
        |> andMap stepC



---- CHAINING


andMap : BranchedState s a -> BranchedState s (a -> b) -> BranchedState s b
andMap =
    \b a -> map2 (<|) a b


andThen : (a -> BranchedState s b) -> BranchedState s a -> BranchedState s b
andThen func (BranchedState computeA) =
    BranchedState <|
        \limit currentState ->
            take limit <|
                limitConcatMap limit
                    (\( a, nextState ) ->
                        let
                            (BranchedState computeB) =
                                func a
                        in
                        computeB limit nextState
                    )
                    (computeA limit currentState)


join : BranchedState s (BranchedState s a) -> BranchedState s a
join (BranchedState computeBranchedState) =
    BranchedState <|
        \limit currentState ->
            limitConcatMap limit
                (\( BranchedState computeA, nextState ) ->
                    computeA limit nextState
                )
                (computeBranchedState limit currentState)



---- CHANGING STATE


get : BranchedState s s
get =
    BranchedState <|
        \limit currentState ->
            case limit of
                Just 0 ->
                    []

                _ ->
                    [ ( currentState, currentState ) ]


put : s -> BranchedState s ()
put newState =
    BranchedState <|
        \limit _ ->
            case limit of
                Just 0 ->
                    []

                _ ->
                    [ ( (), newState ) ]


modify : (s -> s) -> BranchedState s ()
modify func =
    BranchedState <|
        \limit currentState ->
            case limit of
                Just 0 ->
                    []

                _ ->
                    [ ( (), func currentState ) ]



---- RUNNING STATE


run : Maybe Int -> s -> BranchedState s a -> List ( a, s )
run limit initialState (BranchedState computeA) =
    computeA limit initialState


finalValues : Maybe Int -> s -> BranchedState s a -> List a
finalValues limit initialState (BranchedState computeA) =
    List.map Tuple.first (computeA limit initialState)


finalStates : Maybe Int -> s -> BranchedState s a -> List s
finalStates limit initialState (BranchedState computeA) =
    List.map Tuple.second (computeA limit initialState)



---- GENERALIZED LIST FUNCTIONS


traverse : (a -> BranchedState s b) -> List a -> BranchedState s b
traverse func listA =
    BranchedState <|
        \limit currentState ->
            limitConcatMap limit
                (\a ->
                    let
                        (BranchedState computeB) =
                            func a
                    in
                    computeB limit currentState
                )
                listA


combine : (a -> BranchedState s b) -> List a -> BranchedState s (List b)
combine func listA =
    BranchedState <|
        \limit currentState ->
            case listA of
                [] ->
                    [ ( [], currentState ) ]

                a :: restA ->
                    let
                        (BranchedState computeB) =
                            func a
                    in
                    computeB limit currentState
                        |> limitConcatMap limit
                            (\( b, nextState ) ->
                                let
                                    (BranchedState computeRestA) =
                                        combine func restA
                                in
                                List.map (Tuple.mapFirst ((::) b))
                                    (computeRestA limit nextState)
                            )



---- LIMIT


withLimit : Maybe Int -> BranchedState s a -> BranchedState s a
withLimit newLimit (BranchedState computeA) =
    BranchedState <|
        \_ currentState ->
            computeA newLimit currentState



---- HELPER


{-| -}
take : Maybe Int -> List a -> List a
take =
    Maybe.map List.take
        >> Maybe.withDefault identity


{-| -}
limitMap : Maybe Int -> (a -> b) -> List a -> List b
limitMap limit =
    case limit of
        Nothing ->
            mapHelp []

        Just actualLimit ->
            limitMapHelp 0 [] actualLimit


mapHelp : List b -> (a -> b) -> List a -> List b
mapHelp collected func values =
    case values of
        [] ->
            List.reverse collected

        value :: rest ->
            mapHelp (func value :: collected) func rest


limitMapHelp : Int -> List b -> Int -> (a -> b) -> List a -> List b
limitMapHelp count collected limit func values =
    case values of
        [] ->
            List.reverse collected

        value :: rest ->
            if limit < count then
                limitMapHelp
                    (count + 1)
                    (func value :: collected)
                    limit
                    func
                    rest

            else
                List.reverse collected


{-| -}
limitConcatMap : Maybe Int -> (a -> List b) -> List a -> List b
limitConcatMap limit =
    case limit of
        Nothing ->
            concatMapHelp [] []

        Just actualLimit ->
            limitConcatMapHelp 0 [] [] actualLimit


concatMapHelp : List b -> List b -> (a -> List b) -> List a -> List b
concatMapHelp collected listB func listA =
    case listB of
        [] ->
            case listA of
                [] ->
                    List.reverse collected

                a :: restA ->
                    concatMapHelp
                        collected
                        (func a)
                        func
                        restA

        b :: restB ->
            concatMapHelp
                (b :: collected)
                restB
                func
                listA


limitConcatMapHelp :
    Int
    -> List b
    -> List b
    -> Int
    -> (a -> List b)
    -> List a
    -> List b
limitConcatMapHelp count collected listB limit func listA =
    if count < limit then
        case listB of
            [] ->
                case listA of
                    [] ->
                        List.reverse collected

                    a :: restA ->
                        limitConcatMapHelp
                            count
                            collected
                            (func a)
                            limit
                            func
                            restA

            b :: restB ->
                limitConcatMapHelp
                    (count + 1)
                    (b :: collected)
                    restB
                    limit
                    func
                    listA

    else
        List.reverse collected
