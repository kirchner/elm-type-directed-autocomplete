module Triple exposing (mapFirst, mapSecond, mapThird)


mapFirst : (a -> d) -> ( a, b, c ) -> ( d, b, c )
mapFirst f ( a, b, c ) =
    ( f a, b, c )


mapSecond : (b -> d) -> ( a, b, c ) -> ( a, d, c )
mapSecond f ( a, b, c ) =
    ( a, f b, c )


mapThird : (c -> d) -> ( a, b, c ) -> ( a, b, d )
mapThird f ( a, b, c ) =
    ( a, b, f c )
