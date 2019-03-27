module BranchedState exposing
    ( BranchedState
    , andThen
    , combine
    , finalValues
    , get
    , join
    , map
    , put
    , state
    , traverse
    )

---- TYPE AND CONSTRUCTS


type BranchedState state value
    = BranchedState (state -> List ( value, state ))


state : List value -> BranchedState state value
state values =
    BranchedState <|
        \currentState ->
            List.map (\value -> ( value, currentState ))
                values


embed : (s -> List a) -> BranchedState s a
embed computeA =
    BranchedState <|
        \currentState ->
            List.map (\value -> ( value, currentState ))
                (computeA currentState)


advance : (s -> List ( a, s )) -> BranchedState s a
advance computeA =
    BranchedState computeA



---- MAPPING


map : (a -> b) -> BranchedState s a -> BranchedState s b
map func (BranchedState computeA) =
    BranchedState <|
        \currentState ->
            List.map (Tuple.mapFirst func) (computeA currentState)


map2 : (a -> b -> c) -> BranchedState s a -> BranchedState s b -> BranchedState s c
map2 func (BranchedState computeA) (BranchedState computeB) =
    BranchedState <|
        \currentState ->
            List.concatMap
                (\( a, nextState ) ->
                    List.map (Tuple.mapFirst (func a))
                        (computeB nextState)
                )
                (computeA currentState)


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
        \currentState ->
            List.concatMap
                (\( a, nextState ) ->
                    let
                        (BranchedState runB) =
                            func a
                    in
                    runB nextState
                )
                (computeA currentState)


join : BranchedState s (BranchedState s a) -> BranchedState s a
join (BranchedState computeBranchedState) =
    BranchedState <|
        \currentState ->
            List.concatMap
                (\( BranchedState computeA, nextState ) ->
                    computeA nextState
                )
                (computeBranchedState currentState)



---- CHANGING STATE


get : BranchedState s s
get =
    BranchedState <|
        \currentState ->
            [ ( currentState, currentState ) ]


put : s -> BranchedState s ()
put newState =
    BranchedState <|
        \_ ->
            [ ( (), newState ) ]


modify : (s -> s) -> BranchedState s ()
modify func =
    BranchedState <|
        \currentState ->
            [ ( (), func currentState ) ]



---- RUNNING STATE


run : s -> BranchedState s a -> List ( a, s )
run initialState (BranchedState computeA) =
    computeA initialState


finalValues : s -> BranchedState s a -> List a
finalValues initialState (BranchedState computeA) =
    List.map Tuple.first (computeA initialState)


finalStates : s -> BranchedState s a -> List s
finalStates initialState (BranchedState computeA) =
    List.map Tuple.second (computeA initialState)



---- GENERALIZED LIST FUNCTIONS


traverse : (a -> BranchedState s b) -> List a -> BranchedState s b
traverse func listA =
    BranchedState <|
        \currentState ->
            List.concatMap
                (\a ->
                    let
                        (BranchedState computeB) =
                            func a
                    in
                    computeB currentState
                )
                listA


combine : (a -> BranchedState s b) -> List a -> BranchedState s (List b)
combine func listA =
    BranchedState <|
        \currentState ->
            case listA of
                [] ->
                    [ ( [], currentState ) ]

                a :: restA ->
                    let
                        (BranchedState computeB) =
                            func a
                    in
                    computeB currentState
                        |> List.concatMap
                            (\( b, nextState ) ->
                                let
                                    (BranchedState computeRestA) =
                                        combine func restA
                                in
                                List.map (Tuple.mapFirst ((::) b))
                                    (computeRestA nextState)
                            )
