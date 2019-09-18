module Result.Extra exposing
    ( andThen2
    , combine
    )


andThen2 : (a -> b -> Result err c) -> Result err a -> Result err b -> Result err c
andThen2 f resultA resultB =
    Result.andThen
        (\a -> Result.andThen (f a) resultB)
        resultA


combine : List (Result x a) -> Result x (List a)
combine =
    List.foldr (Result.map2 (::)) (Ok [])
