module Main exposing (main)

import Benchmark exposing (Benchmark, benchmark, compare, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Dict exposing (Dict)
import Elm.Docs
import Elm.Type exposing (Type(..))
import Generator
    exposing
        ( Generator
        , addValues
        , all
        , call
        , firstN
        , for
        , value
        )
import Json.Decode as Decode


main : BenchmarkProgram
main =
    program suiteFirstN


comparison : Benchmark
comparison =
    describe "for"
        [ describe "core modules and some constants"
            [ compare "Int"
                "call [ all [ value, call [ value ] ], value ]"
                (\_ ->
                    innerAllGenerator
                        |> for int
                )
                "all [ call [ value, value ], call [ call [ value ], value ] ]"
                (\_ ->
                    outerAllGenerator
                        |> for int
                )
            ]
        ]


innerAllGenerator : Generator
innerAllGenerator =
    call
        [ all
            [ value
            , call [ value ]
            ]
        , value
        ]


outerAllGenerator : Generator
outerAllGenerator =
    all
        [ call
            [ value
            , value
            ]
        , call
            [ call [ value ]
            , value
            ]
        ]


suiteFirstN : Benchmark
suiteFirstN =
    describe "for"
        [ describe "core modules"
            [ describe "call [ value ]"
                [ compare "Int"
                    "no limit"
                    (\_ ->
                        callValueGenerator
                            |> for int
                    )
                    "limit == 1"
                    (\_ ->
                        callValueFirstNGenerator
                            |> for int
                    )
                , compare "Float"
                    "no limit"
                    (\_ ->
                        callValueGenerator
                            |> for float
                    )
                    "limit == 1"
                    (\_ ->
                        callValueFirstNGenerator
                            |> for float
                    )
                , compare "String"
                    "no limit"
                    (\_ ->
                        callValueGenerator
                            |> for string
                    )
                    "limit == 1"
                    (\_ ->
                        callValueFirstNGenerator
                            |> for string
                    )
                , compare "List Int"
                    "no limit"
                    (\_ ->
                        callValueGenerator
                            |> for listInt
                    )
                    "limit == 1"
                    (\_ ->
                        callValueFirstNGenerator
                            |> for listInt
                    )
                , compare "List Int -> List Int"
                    "no limit"
                    (\_ ->
                        callValueGenerator
                            |> for listIntToListInt
                    )
                    "limit == 1"
                    (\_ ->
                        callValueFirstNGenerator
                            |> for listIntToListInt
                    )
                ]
            , describe "call [ value, value ]"
                [ compare "List Int -> Int"
                    "no limit"
                    (\_ ->
                        callValueValueGenerator
                            |> for listIntToInt
                    )
                    "limit == 1"
                    (\_ ->
                        callValueValueFirstNGenerator
                            |> for listIntToInt
                    )
                , compare "List Int -> String"
                    "no limit"
                    (\_ ->
                        callValueValueGenerator
                            |> for listIntToString
                    )
                    "limit == 1"
                    (\_ ->
                        callValueValueFirstNGenerator
                            |> for listIntToString
                    )
                ]
            , describe "call [ value, value, value ]"
                [ compare "Int"
                    "no limit"
                    (\_ ->
                        callValueValueValueGenerator
                            |> for int
                    )
                    "limit == 1"
                    (\_ ->
                        callValueValueValueFirstNGenerator
                            |> for int
                    )
                , compare "Float"
                    "no limit"
                    (\_ ->
                        callValueValueValueGenerator
                            |> for float
                    )
                    "limit == 1"
                    (\_ ->
                        callValueValueValueFirstNGenerator
                            |> for float
                    )
                , compare "String"
                    "no limit"
                    (\_ ->
                        callValueValueValueGenerator
                            |> for string
                    )
                    "limit == 1"
                    (\_ ->
                        callValueValueValueFirstNGenerator
                            |> for string
                    )
                ]
            ]
        ]


suite : Benchmark
suite =
    describe "for"
        [ describe "core modules"
            [ describe "value"
                [ benchmark "Int" <|
                    \_ ->
                        valueGenerator
                            |> for int
                , benchmark "Float" <|
                    \_ ->
                        valueGenerator
                            |> for float
                , benchmark "String" <|
                    \_ ->
                        valueGenerator
                            |> for string
                , benchmark "List Int" <|
                    \_ ->
                        valueGenerator
                            |> for listInt
                ]
            , describe "call [ value ]"
                [ benchmark "Int" <|
                    \_ ->
                        callValueGenerator
                            |> for int
                , benchmark "Float" <|
                    \_ ->
                        callValueGenerator
                            |> for float
                , benchmark "String" <|
                    \_ ->
                        callValueGenerator
                            |> for string
                , benchmark "List Int" <|
                    \_ ->
                        callValueGenerator
                            |> for listInt
                , benchmark "List Int -> List Int" <|
                    \_ ->
                        callValueGenerator
                            |> for listIntToListInt
                ]
            ]
        , describe "core modules and some constants"
            [ describe "call [ value, value ]"
                [ benchmark "List Int -> Int" <|
                    \_ ->
                        callValueValueGenerator
                            |> for listIntToInt
                , benchmark "List Int -> String" <|
                    \_ ->
                        callValueValueGenerator
                            |> for listIntToString
                ]
            , describe "call [ value, value, value ]"
                [ benchmark "Int" <|
                    \_ ->
                        callValueValueValueGenerator
                            |> for int
                , benchmark "Float" <|
                    \_ ->
                        callValueValueValueGenerator
                            |> for float
                , benchmark "String" <|
                    \_ ->
                        callValueValueValueGenerator
                            |> for string
                ]
            , describe "call [ call [ value ], value ]"
                [ benchmark "Int" <|
                    \_ ->
                        call_callValue_valueGenerator
                            |> for int
                , benchmark "Float" <|
                    \_ ->
                        call_callValue_valueGenerator
                            |> for float
                , benchmark "String" <|
                    \_ ->
                        call_callValue_valueGenerator
                            |> for string
                ]
            , describe "call [ value, call [ value ] ]"
                [ benchmark "Int" <|
                    \_ ->
                        call_value_callValueGenerator
                            |> for int
                , benchmark "Float" <|
                    \_ ->
                        call_value_callValueGenerator
                            |> for float
                , benchmark "String" <|
                    \_ ->
                        call_value_callValueGenerator
                            |> for string
                ]
            , describe "all generators together"
                [ benchmark "Int" <|
                    \_ ->
                        allGenerator
                            |> for int
                , benchmark "Float" <|
                    \_ ->
                        allGenerator
                            |> for float
                , benchmark "String" <|
                    \_ ->
                        allGenerator
                            |> for string
                ]
            ]
        ]



---- GENERATORS


valueGenerator : Generator
valueGenerator =
    value
        |> addValues values


callValueGenerator : Generator
callValueGenerator =
    call [ value ]
        |> addValues values


callValueFirstNGenerator : Generator
callValueFirstNGenerator =
    call [ firstN 1 value ]
        |> addValues values


callValueValueGenerator : Generator
callValueValueGenerator =
    call [ value, value ]
        |> addValues valuesWithConstants


callValueValueFirstNGenerator : Generator
callValueValueFirstNGenerator =
    call [ firstN 1 value, firstN 1 value ]
        |> addValues valuesWithConstants


callValueValueValueGenerator : Generator
callValueValueValueGenerator =
    call [ value, value, value ]
        |> addValues valuesWithConstants


callValueValueValueFirstNGenerator : Generator
callValueValueValueFirstNGenerator =
    call [ firstN 1 value, firstN 1 value, firstN 1 value ]
        |> addValues valuesWithConstants


call_callValue_valueGenerator : Generator
call_callValue_valueGenerator =
    call [ call [ value ], value ]
        |> addValues valuesWithConstants


call_value_callValueGenerator : Generator
call_value_callValueGenerator =
    call [ value, call [ value ] ]
        |> addValues valuesWithConstants


allGenerator : Generator
allGenerator =
    all
        [ value
        , call [ value ]
        , call [ value, value ]
        , call [ value, value, value ]
        , call [ call [ value ], value ]
        , call [ value, call [ value ] ]
        ]
        |> addValues valuesWithConstants



---- TYPES


int =
    Type "Int" []


float =
    Type "Float" []


string =
    Type "String" []


list tipe =
    Type "List" [ tipe ]


listInt =
    list int


listIntToListInt =
    Lambda listInt listInt


listIntToInt =
    Lambda listInt int


listIntToString =
    Lambda listInt string



---- VALUES


values : Dict String Type
values =
    case Decode.decodeString (Decode.list Elm.Docs.decoder) coreJson of
        Err _ ->
            Dict.empty

        Ok modules ->
            modules
                |> List.concatMap .values
                |> List.map (\value -> ( value.name, value.tipe ))
                |> Dict.fromList


valuesWithConstants : Dict String Type
valuesWithConstants =
    values
        |> Dict.insert "int" int
        |> Dict.insert "float" float
        |> Dict.insert "string" string
        |> Dict.insert "listInt" listInt


coreJson : String
coreJson =
    """[
  {
    "name": "Array",
    "comment": " Fast immutable arrays. The elements in an array must have the same type.\\n\\n# Arrays\\n@docs Array\\n\\n# Creation\\n@docs empty, initialize, repeat, fromList\\n\\n# Query\\n@docs isEmpty, length, get\\n\\n# Manipulate\\n@docs set, push, append, slice\\n\\n# Lists\\n@docs toList, toIndexedList\\n\\n# Transform\\n@docs map, indexedMap, foldl, foldr, filter\\n",
    "unions": [
      {
        "name": "Array",
        "comment": " Representation of fast immutable arrays. You can create arrays of integers\\n(`Array Int`) or strings (`Array String`) or any other type of value you can\\ndream up.\\n",
        "args": [
          "a"
        ],
        "cases": []
      }
    ],
    "aliases": [],
    "values": [
      {
        "name": "append",
        "comment": " Append two arrays to a new one.\\n\\n    append (repeat 2 42) (repeat 3 81) == fromList [42,42,81,81,81]\\n",
        "type": "Array.Array a -> Array.Array a -> Array.Array a"
      },
      {
        "name": "empty",
        "comment": " Return an empty array.\\n\\n    length empty == 0\\n",
        "type": "Array.Array a"
      },
      {
        "name": "filter",
        "comment": " Keep elements that pass the test.\\n\\n    filter isEven (fromList [1,2,3,4,5,6]) == (fromList [2,4,6])\\n",
        "type": "(a -> Basics.Bool) -> Array.Array a -> Array.Array a"
      },
      {
        "name": "foldl",
        "comment": " Reduce an array from the left. Read `foldl` as fold from the left.\\n\\n    foldl (::) [] (fromList [1,2,3]) == [3,2,1]\\n",
        "type": "(a -> b -> b) -> b -> Array.Array a -> b"
      },
      {
        "name": "foldr",
        "comment": " Reduce an array from the right. Read `foldr` as fold from the right.\\n\\n    foldr (+) 0 (repeat 3 5) == 15\\n",
        "type": "(a -> b -> b) -> b -> Array.Array a -> b"
      },
      {
        "name": "fromList",
        "comment": " Create an array from a `List`.\\n",
        "type": "List.List a -> Array.Array a"
      },
      {
        "name": "get",
        "comment": " Return `Just` the element at the index or `Nothing` if the index is out of\\nrange.\\n\\n    get  0 (fromList [0,1,2]) == Just 0\\n    get  2 (fromList [0,1,2]) == Just 2\\n    get  5 (fromList [0,1,2]) == Nothing\\n    get -1 (fromList [0,1,2]) == Nothing\\n",
        "type": "Basics.Int -> Array.Array a -> Maybe.Maybe a"
      },
      {
        "name": "indexedMap",
        "comment": " Apply a function on every element with its index as first argument.\\n\\n    indexedMap (*) (fromList [5,5,5]) == fromList [0,5,10]\\n",
        "type": "(Basics.Int -> a -> b) -> Array.Array a -> Array.Array b"
      },
      {
        "name": "initialize",
        "comment": " Initialize an array. `initialize n f` creates an array of length `n` with\\nthe element at index `i` initialized to the result of `(f i)`.\\n\\n    initialize 4 identity    == fromList [0,1,2,3]\\n    initialize 4 (\\
 -> n*n) == fromList [0,1,4,9]\\n    initialize 4 (always 0)  == fromList [0,0,0,0]\\n",
        "type": "Basics.Int -> (Basics.Int -> a) -> Array.Array a"
      },
      {
        "name": "isEmpty",
        "comment": " Determine if an array is empty.\\n\\n    isEmpty empty == True\\n",
        "type": "Array.Array a -> Basics.Bool"
      },
      {
        "name": "length",
        "comment": " Return the length of an array.\\n\\n    length (fromList [1,2,3]) == 3\\n",
        "type": "Array.Array a -> Basics.Int"
      },
      {
        "name": "map",
        "comment": " Apply a function on every element in an array.\\n\\n    map sqrt (fromList [1,4,9]) == fromList [1,2,3]\\n",
        "type": "(a -> b) -> Array.Array a -> Array.Array b"
      },
      {
        "name": "push",
        "comment": " Push an element onto the end of an array.\\n\\n    push 3 (fromList [1,2]) == fromList [1,2,3]\\n",
        "type": "a -> Array.Array a -> Array.Array a"
      },
      {
        "name": "repeat",
        "comment": " Creates an array with a given length, filled with a default element.\\n\\n    repeat 5 0     == fromList [0,0,0,0,0]\\n    repeat 3 "cat" == fromList ["cat","cat","cat"]\\n\\nNotice that `repeat 3 x` is the same as `initialize 3 (always x)`.\\n",
        "type": "Basics.Int -> a -> Array.Array a"
      },
      {
        "name": "set",
        "comment": " Set the element at a particular index. Returns an updated array.\\nIf the index is out of range, the array is unaltered.\\n\\n    set 1 7 (fromList [1,2,3]) == fromList [1,7,3]\\n",
        "type": "Basics.Int -> a -> Array.Array a -> Array.Array a"
      },
      {
        "name": "slice",
        "comment": " Get a sub-section of an array: `(slice start end array)`. The `start` is a\\nzero-based index where we will start our slice. The `end` is a zero-based index\\nthat indicates the end of the slice. The slice extracts up to but not including\\n`end`.\\n\\n    slice  0  3 (fromList [0,1,2,3,4]) == fromList [0,1,2]\\n    slice  1  4 (fromList [0,1,2,3,4]) == fromList [1,2,3]\\n\\nBoth the `start` and `end` indexes can be negative, indicating an offset from\\nthe end of the array.\\n\\n    slice  1 -1 (fromList [0,1,2,3,4]) == fromList [1,2,3]\\n    slice -2  5 (fromList [0,1,2,3,4]) == fromList [3,4]\\n\\nThis makes it pretty easy to `pop` the last element off of an array:\\n`slice 0 -1 array`\\n",
        "type": "Basics.Int -> Basics.Int -> Array.Array a -> Array.Array a"
      },
      {
        "name": "toIndexedList",
        "comment": " Create an indexed list from an array. Each element of the array will be\\npaired with its index.\\n\\n    toIndexedList (fromList ["cat","dog"]) == [(0,"cat"), (1,"dog")]\\n",
        "type": "Array.Array a -> List.List ( Basics.Int, a )"
      },
      {
        "name": "toList",
        "comment": " Create a list of elements from an array.\\n\\n    toList (fromList [3,5,8]) == [3,5,8]\\n",
        "type": "Array.Array a -> List.List a"
      }
    ],
    "binops": []
  },
  {
    "name": "Basics",
    "comment": " Tons of useful functions that get imported by default.\\n\\n# Math\\n@docs Int, Float, (+), (-), (*), (/), (//), (^)\\n\\n# Int to Float / Float to Int\\n@docs toFloat, round, floor, ceiling, truncate\\n\\n# Equality\\n@docs (==), (/=)\\n\\n# Comparison\\n\\nThese functions only work on `comparable` types. This includes numbers,\\ncharacters, strings, lists of comparable things, and tuples of comparable\\nthings.\\n\\n@docs (<), (>), (<=), (>=), max, min, compare, Order\\n\\n# Booleans\\n@docs Bool, not, (&&), (||), xor\\n\\n# Append Strings and Lists\\n@docs (++)\\n\\n# Fancier Math\\n@docs modBy, remainderBy, negate, abs, clamp, sqrt, logBase, e\\n\\n# Angles\\n@docs degrees, radians, turns\\n\\n# Trigonometry\\n@docs pi, cos, sin, tan, acos, asin, atan, atan2\\n\\n# Polar Coordinates\\n@docs toPolar, fromPolar\\n\\n# Floating Point Checks\\n@docs isNaN, isInfinite\\n\\n# Function Helpers\\n@docs identity, always, (<|), (|>), (<<), (>>), Never, never\\n\\n",
    "unions": [
      {
        "name": "Bool",
        "comment": " A “Boolean” value. It can either be `True` or `False`.\\n\\n**Note:** Programmers coming from JavaScript, Java, etc. tend to reach for\\nboolean values way too often in Elm. Using a [union type][ut] is often clearer\\nand more reliable. You can learn more about this from Jeremy [here][jf] or\\nfrom Richard [here][rt].\\n\\n[ut]: https://guide.elm-lang.org/types/union_types.html\\n[jf]: https://youtu.be/6TDKHGtAxeg?t=1m25s\\n[rt]: https://youtu.be/IcgmSRJHu_8?t=1m14s\\n",
        "args": [],
        "cases": [
          [
            "True",
            []
          ],
          [
            "False",
            []
          ]
        ]
      },
      {
        "name": "Float",
        "comment": " A `Float` is a [floating-point number][fp]. Valid syntax for floats includes:\\n\\n    0\\n    42\\n    3.14\\n    0.1234\\n    6.022e23   -- == (6.022 * 10^23)\\n    6.022e+23  -- == (6.022 * 10^23)\\n    1.602e−19  -- == (1.602 * 10^-19)\\n    1e3        -- == (1 * 10^3) == 1000\\n\\n**Historical Note:** The particular details of floats (e.g. `NaN`) are\\nspecified by [IEEE 754][ieee] which is literally hard-coded into almost all\\nCPUs in the world. That means if you think `NaN` is weird, you must\\nsuccessfully overtake Intel and AMD with a chip that is not backwards\\ncompatible with any widely-used assembly language.\\n\\n[fp]: https://en.wikipedia.org/wiki/Floating-point_arithmetic\\n[ieee]: https://en.wikipedia.org/wiki/IEEE_754\\n",
        "args": [],
        "cases": []
      },
      {
        "name": "Int",
        "comment": " An `Int` is a whole number. Valid syntax for integers includes:\\n\\n    0\\n    42\\n    9000\\n    0xFF   -- 255 in hexadecimal\\n    0x000A --  10 in hexadecimal\\n\\n**Note:** `Int` math is well-defined in the range `-2^31` to `2^31 - 1`. Outside\\nof that range, the behavior is determined by the compilation target. When\\ngenerating JavaScript, the safe range expands to `-2^53` to `2^53 - 1` for some\\noperations, but if we generate WebAssembly some day, we would do the traditional\\n[integer overflow][io]. This quirk is necessary to get good performance on\\nquirky compilation targets.\\n\\n**Historical Note:** The name `Int` comes from the term [integer][]. It appears\\nthat the `int` abbreviation was introduced in [ALGOL 68][68], shortening it\\nfrom `integer` in [ALGOL 60][60]. Today, almost all programming languages use\\nthis abbreviation.\\n\\n[io]: https://en.wikipedia.org/wiki/Integer_overflow\\n[integer]: https://en.wikipedia.org/wiki/Integer\\n[60]: https://en.wikipedia.org/wiki/ALGOL_60\\n[68]: https://en.wikipedia.org/wiki/ALGOL_68\\n",
        "args": [],
        "cases": []
      },
      {
        "name": "Never",
        "comment": " A value that can never happen! For context:\\n\\n  - The boolean type `Bool` has two values: `True` and `False`\\n  - The unit type `()` has one value: `()`\\n  - The never type `Never` has no values!\\n\\nYou may see it in the wild in `Html Never` which means this HTML will never\\nproduce any messages. You would need to write an event handler like\\n`onClick ??? : Attribute Never` but how can we fill in the question marks?!\\nSo there cannot be any event handlers on that HTML.\\n\\nYou may also see this used with tasks that never fail, like `Task Never ()`.\\n\\nThe `Never` type is useful for restricting *arguments* to a function. Maybe my\\nAPI can only accept HTML without event handlers, so I require `Html Never` and\\nusers can give `Html msg` and everything will go fine. Generally speaking, you\\ndo not want `Never` in your return types though.\\n",
        "args": [],
        "cases": []
      },
      {
        "name": "Order",
        "comment": " Represents the relative ordering of two things.\\nThe relations are less than, equal to, and greater than.\\n",
        "args": [],
        "cases": [
          [
            "LT",
            []
          ],
          [
            "EQ",
            []
          ],
          [
            "GT",
            []
          ]
        ]
      }
    ],
    "aliases": [],
    "values": [
      {
        "name": "abs",
        "comment": " Get the [absolute value][abs] of a number.\\n\\n    abs 16   == 16\\n    abs -4   == 4\\n    abs -8.5 == 8.5\\n    abs 3.14 == 3.14\\n\\n[abs]: https://en.wikipedia.org/wiki/Absolute_value\\n",
        "type": "number -> number"
      },
      {
        "name": "acos",
        "comment": " Figure out the arccosine for `adjacent / hypotenuse` in radians:\\n\\n    acos (1/2) == 1.0471975511965979 -- 60° or pi/3 radians\\n\\n",
        "type": "Basics.Float -> Basics.Float"
      },
      {
        "name": "always",
        "comment": " Create a function that *always* returns the same value. Useful with\\nfunctions like `map`:\\n\\n    List.map (always 0) [1,2,3,4,5] == [0,0,0,0,0]\\n\\n    -- List.map (\\_ -> 0) [1,2,3,4,5] == [0,0,0,0,0]\\n    -- always = (\\x _ -> x)\\n",
        "type": "a -> b -> a"
      },
      {
        "name": "asin",
        "comment": " Figure out the arcsine for `opposite / hypotenuse` in radians:\\n\\n    asin (1/2) == 0.5235987755982989 -- 30° or pi/6 radians\\n\\n",
        "type": "Basics.Float -> Basics.Float"
      },
      {
        "name": "atan",
        "comment": " This helps you find the angle (in radians) to an `(x,y)` coordinate, but\\nin a way that is rarely useful in programming. **You probably want\\n[`atan2`](#atan2) instead!**\\n\\nThis version takes `y/x` as its argument, so there is no way to know whether\\nthe negative signs comes from the `y` or `x` value. So as we go counter-clockwise\\naround the origin from point `(1,1)` to `(1,-1)` to `(-1,-1)` to `(-1,1)` we do\\nnot get angles that go in the full circle:\\n\\n    atan (  1 /  1 ) ==  0.7853981633974483 --  45° or   pi/4 radians\\n    atan (  1 / -1 ) == -0.7853981633974483 -- 315° or 7*pi/4 radians\\n    atan ( -1 / -1 ) ==  0.7853981633974483 --  45° or   pi/4 radians\\n    atan ( -1 /  1 ) == -0.7853981633974483 -- 315° or 7*pi/4 radians\\n\\nNotice that everything is between `pi/2` and `-pi/2`. That is pretty useless\\nfor figuring out angles in any sort of visualization, so again, check out\\n[`atan2`](#atan2) instead!\\n",
        "type": "Basics.Float -> Basics.Float"
      },
      {
        "name": "atan2",
        "comment": " This helps you find the angle (in radians) to an `(x,y)` coordinate.\\nSo rather than saying `atan (y/x)` you say `atan2 y x` and you can get a full\\nrange of angles:\\n\\n    atan2  1  1 ==  0.7853981633974483 --  45° or   pi/4 radians\\n    atan2  1 -1 ==  2.356194490192345  -- 135° or 3*pi/4 radians\\n    atan2 -1 -1 == -2.356194490192345  -- 225° or 5*pi/4 radians\\n    atan2 -1  1 == -0.7853981633974483 -- 315° or 7*pi/4 radians\\n\\n",
        "type": "Basics.Float -> Basics.Float -> Basics.Float"
      },
      {
        "name": "ceiling",
        "comment": " Ceiling function, rounding up.\\n\\n    ceiling 1.0 == 1\\n    ceiling 1.2 == 2\\n    ceiling 1.5 == 2\\n    ceiling 1.8 == 2\\n\\n    ceiling -1.2 == -1\\n    ceiling -1.5 == -1\\n    ceiling -1.8 == -1\\n",
        "type": "Basics.Float -> Basics.Int"
      },
      {
        "name": "clamp",
        "comment": " Clamps a number within a given range. With the expression\\n`clamp 100 200 x` the results are as follows:\\n\\n    100     if x < 100\\n     x      if 100 <= x < 200\\n    200     if 200 <= x\\n",
        "type": "number -> number -> number -> number"
      },
      {
        "name": "compare",
        "comment": " Compare any two comparable values. Comparable values include `String`,\\n`Char`, `Int`, `Float`, or a list or tuple containing comparable values. These\\nare also the only values that work as `Dict` keys or `Set` members.\\n\\n    compare 3 4 == LT\\n    compare 4 4 == EQ\\n    compare 5 4 == GT\\n",
        "type": "comparable -> comparable -> Basics.Order"
      },
      {
        "name": "cos",
        "comment": " Figure out the cosine given an angle in radians.\\n\\n    cos (degrees 60)     == 0.5000000000000001\\n    cos (turns (1/6))    == 0.5000000000000001\\n    cos (radians (pi/3)) == 0.5000000000000001\\n    cos (pi/3)           == 0.5000000000000001\\n\\n",
        "type": "Basics.Float -> Basics.Float"
      },
      {
        "name": "degrees",
        "comment": " Convert degrees to standard Elm angles (radians).\\n\\n    degrees 180 == 3.141592653589793\\n",
        "type": "Basics.Float -> Basics.Float"
      },
      {
        "name": "e",
        "comment": " An approximation of e.\\n",
        "type": "Basics.Float"
      },
      {
        "name": "floor",
        "comment": " Floor function, rounding down.\\n\\n    floor 1.0 == 1\\n    floor 1.2 == 1\\n    floor 1.5 == 1\\n    floor 1.8 == 1\\n\\n    floor -1.2 == -2\\n    floor -1.5 == -2\\n    floor -1.8 == -2\\n",
        "type": "Basics.Float -> Basics.Int"
      },
      {
        "name": "fromPolar",
        "comment": " Convert polar coordinates (r,&theta;) to Cartesian coordinates (x,y).\\n\\n    fromPolar (sqrt 2, degrees 45) == (1, 1)\\n",
        "type": "( Basics.Float, Basics.Float ) -> ( Basics.Float, Basics.Float )"
      },
      {
        "name": "identity",
        "comment": " Given a value, returns exactly the same value. This is called\\n[the identity function](https://en.wikipedia.org/wiki/Identity_function).\\n",
        "type": "a -> a"
      },
      {
        "name": "isInfinite",
        "comment": " Determine whether a float is positive or negative infinity.\\n\\n    isInfinite (0/0)     == False\\n    isInfinite (sqrt -1) == False\\n    isInfinite (1/0)     == True\\n    isInfinite 1         == False\\n\\nNotice that NaN is not infinite! For float `n` to be finite implies that\\n`not (isInfinite n || isNaN n)` evaluates to `True`.\\n",
        "type": "Basics.Float -> Basics.Bool"
      },
      {
        "name": "isNaN",
        "comment": " Determine whether a float is an undefined or unrepresentable number.\\nNaN stands for *not a number* and it is [a standardized part of floating point\\nnumbers](https://en.wikipedia.org/wiki/NaN).\\n\\n    isNaN (0/0)     == True\\n    isNaN (sqrt -1) == True\\n    isNaN (1/0)     == False  -- infinity is a number\\n    isNaN 1         == False\\n",
        "type": "Basics.Float -> Basics.Bool"
      },
      {
        "name": "logBase",
        "comment": " Calculate the logarithm of a number with a given base.\\n\\n    logBase 10 100 == 2\\n    logBase 2 256 == 8\\n",
        "type": "Basics.Float -> Basics.Float -> Basics.Float"
      },
      {
        "name": "max",
        "comment": " Find the larger of two comparables.\\n\\n    max 42 12345678 == 12345678\\n    max "abc" "xyz" == "xyz"\\n",
        "type": "comparable -> comparable -> comparable"
      },
      {
        "name": "min",
        "comment": " Find the smaller of two comparables.\\n\\n    min 42 12345678 == 42\\n    min "abc" "xyz" == "abc"\\n",
        "type": "comparable -> comparable -> comparable"
      },
      {
        "name": "modBy",
        "comment": " Perform [modular arithmetic](https://en.wikipedia.org/wiki/Modular_arithmetic).\\nA common trick is to use (n mod 2) to detect even and odd numbers:\\n\\n    modBy 2 0 == 0\\n    modBy 2 1 == 1\\n    modBy 2 2 == 0\\n    modBy 2 3 == 1\\n\\nOur `modBy` function works in the typical mathematical way when you run into\\nnegative numbers:\\n\\n    List.map (modBy 4) [ -5, -4, -3, -2, -1,  0,  1,  2,  3,  4,  5 ]\\n    --                 [  3,  0,  1,  2,  3,  0,  1,  2,  3,  0,  1 ]\\n\\nUse [`remainderBy`](#remainderBy) for a different treatment of negative numbers,\\nor read Daan Leijen’s [Division and Modulus for Computer Scientists][dm] for more\\ninformation.\\n\\n[dm]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf\\n",
        "type": "Basics.Int -> Basics.Int -> Basics.Int"
      },
      {
        "name": "negate",
        "comment": " Negate a number.\\n\\n    negate 42 == -42\\n    negate -42 == 42\\n    negate 0 == 0\\n",
        "type": "number -> number"
      },
      {
        "name": "never",
        "comment": " A function that can never be called. Seems extremely pointless, but it\\n*can* come in handy. Imagine you have some HTML that should never produce any\\nmessages. And say you want to use it in some other HTML that *does* produce\\nmessages. You could say:\\n\\n    import Html exposing (..)\\n\\n    embedHtml : Html Never -> Html msg\\n    embedHtml staticStuff =\\n      div []\\n        [ text "hello"\\n        , Html.map never staticStuff\\n        ]\\n\\nSo the `never` function is basically telling the type system, make sure no one\\never calls me!\\n",
        "type": "Basics.Never -> a"
      },
      {
        "name": "not",
        "comment": " Negate a boolean value.\\n\\n    not True == False\\n    not False == True\\n",
        "type": "Basics.Bool -> Basics.Bool"
      },
      {
        "name": "pi",
        "comment": " An approximation of pi.\\n",
        "type": "Basics.Float"
      },
      {
        "name": "radians",
        "comment": " Convert radians to standard Elm angles (radians).\\n\\n    radians pi == 3.141592653589793\\n",
        "type": "Basics.Float -> Basics.Float"
      },
      {
        "name": "remainderBy",
        "comment": " Get the remainder after division. Here are bunch of examples of dividing by four:\\n\\n    List.map (remainderBy 4) [ -5, -4, -3, -2, -1,  0,  1,  2,  3,  4,  5 ]\\n    --                       [ -1,  0, -3, -2, -1,  0,  1,  2,  3,  0,  1 ]\\n\\nUse [`modBy`](#modBy) for a different treatment of negative numbers,\\nor read Daan Leijen’s [Division and Modulus for Computer Scientists][dm] for more\\ninformation.\\n\\n[dm]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf\\n",
        "type": "Basics.Int -> Basics.Int -> Basics.Int"
      },
      {
        "name": "round",
        "comment": " Round a number to the nearest integer.\\n\\n    round 1.0 == 1\\n    round 1.2 == 1\\n    round 1.5 == 2\\n    round 1.8 == 2\\n\\n    round -1.2 == -1\\n    round -1.5 == -1\\n    round -1.8 == -2\\n",
        "type": "Basics.Float -> Basics.Int"
      },
      {
        "name": "sin",
        "comment": " Figure out the sine given an angle in radians.\\n\\n    sin (degrees 30)     == 0.49999999999999994\\n    sin (turns (1/12))   == 0.49999999999999994\\n    sin (radians (pi/6)) == 0.49999999999999994\\n    sin (pi/6)           == 0.49999999999999994\\n\\n",
        "type": "Basics.Float -> Basics.Float"
      },
      {
        "name": "sqrt",
        "comment": " Take the square root of a number.\\n\\n    sqrt  4 == 2\\n    sqrt  9 == 3\\n    sqrt 16 == 4\\n    sqrt 25 == 5\\n",
        "type": "Basics.Float -> Basics.Float"
      },
      {
        "name": "tan",
        "comment": " Figure out the tangent given an angle in radians.\\n\\n    tan (degrees 45)     == 0.9999999999999999\\n    tan (turns (1/8))    == 0.9999999999999999\\n    tan (radians (pi/4)) == 0.9999999999999999\\n    tan (pi/4)           == 0.9999999999999999\\n",
        "type": "Basics.Float -> Basics.Float"
      },
      {
        "name": "toFloat",
        "comment": " Convert an integer into a float. Useful when mixing `Int` and `Float`\\nvalues like this:\\n\\n    halfOf : Int -> Float\\n    halfOf number =\\n      toFloat number / 2\\n\\n",
        "type": "Basics.Int -> Basics.Float"
      },
      {
        "name": "toPolar",
        "comment": " Convert Cartesian coordinates (x,y) to polar coordinates (r,&theta;).\\n\\n    toPolar (3, 4) == ( 5, 0.9272952180016122)\\n    toPolar (5,12) == (13, 1.1760052070951352)\\n",
        "type": "( Basics.Float, Basics.Float ) -> ( Basics.Float, Basics.Float )"
      },
      {
        "name": "truncate",
        "comment": " Truncate a number, rounding towards zero.\\n\\n    truncate 1.0 == 1\\n    truncate 1.2 == 1\\n    truncate 1.5 == 1\\n    truncate 1.8 == 1\\n\\n    truncate -1.2 == -1\\n    truncate -1.5 == -1\\n    truncate -1.8 == -1\\n",
        "type": "Basics.Float -> Basics.Int"
      },
      {
        "name": "turns",
        "comment": " Convert turns to standard Elm angles (radians). One turn is equal to 360°.\\n\\n    turns (1/2) == 3.141592653589793\\n",
        "type": "Basics.Float -> Basics.Float"
      },
      {
        "name": "xor",
        "comment": " The exclusive-or operator. `True` if exactly one input is `True`.\\n\\n    xor True  True  == False\\n    xor True  False == True\\n    xor False True  == True\\n    xor False False == False\\n",
        "type": "Basics.Bool -> Basics.Bool -> Basics.Bool"
      }
    ],
    "binops": [
      {
        "name": "&&",
        "comment": " The logical AND operator. `True` if both inputs are `True`.\\n\\n    True  && True  == True\\n    True  && False == False\\n    False && True  == False\\n    False && False == False\\n\\n**Note:** When used in the infix position, like `(left && right)`, the operator\\nshort-circuits. This means if `left` is `False` we do not bother evaluating `right`\\nand just return `False` overall.\\n",
        "type": "Basics.Bool -> Basics.Bool -> Basics.Bool",
        "associativity": "right",
        "precedence": 3
      },
      {
        "name": "*",
        "comment": " Multiply numbers like `2 * 3 == 6`.\\n\\nSee [`(+)`](#+) for docs on the `number` type variable.\\n",
        "type": "number -> number -> number",
        "associativity": "left",
        "precedence": 7
      },
      {
        "name": "+",
        "comment": " Add two numbers. The `number` type variable means this operation can be\\nspecialized to `Int -> Int -> Int` or to `Float -> Float -> Float`. So you\\ncan do things like this:\\n\\n    3002 + 4004 == 7006  -- all ints\\n    3.14 + 3.14 == 6.28  -- all floats\\n\\nYou _cannot_ add an `Int` and a `Float` directly though. Use functions like\\n[toFloat](#toFloat) or [round](#round) to convert both values to the same type.\\nSo if you needed to add a list length to a `Float` for some reason, you\\ncould say one of these:\\n\\n    3.14 + toFloat (List.length [1,2,3]) == 6.14\\n    round 3.14 + List.length [1,2,3]     == 6\\n\\n**Note:** Languages like Java and JavaScript automatically convert `Int` values\\nto `Float` values when you mix and match. This can make it difficult to be sure\\nexactly what type of number you are dealing with. When you try to _infer_ these\\nconversions (as Scala does) it can be even more confusing. Elm has opted for a\\ndesign that makes all conversions explicit.\\n",
        "type": "number -> number -> number",
        "associativity": "left",
        "precedence": 6
      },
      {
        "name": "++",
        "comment": " Put two appendable things together. This includes strings, lists, and text.\\n\\n    "hello" ++ "world" == "helloworld"\\n    [1,1,2] ++ [3,5,8] == [1,1,2,3,5,8]\\n",
        "type": "appendable -> appendable -> appendable",
        "associativity": "right",
        "precedence": 5
      },
      {
        "name": "-",
        "comment": " Subtract numbers like `4 - 3 == 1`.\\n\\nSee [`(+)`](#+) for docs on the `number` type variable.\\n",
        "type": "number -> number -> number",
        "associativity": "left",
        "precedence": 6
      },
      {
        "name": "/",
        "comment": " Floating-point division:\\n\\n    3.14 / 2 == 1.57\\n\\n",
        "type": "Basics.Float -> Basics.Float -> Basics.Float",
        "associativity": "left",
        "precedence": 7
      },
      {
        "name": "//",
        "comment": " Integer division:\\n\\n    3 // 2 == 1\\n\\nNotice that the remainder is discarded.\\n",
        "type": "Basics.Int -> Basics.Int -> Basics.Int",
        "associativity": "left",
        "precedence": 7
      },
      {
        "name": "/=",
        "comment": " Check if values are not &ldquo;the same&rdquo;.\\n\\nSo `(a /= b)` is the same as `(not (a == b))`.\\n",
        "type": "a -> a -> Basics.Bool",
        "associativity": "non",
        "precedence": 4
      },
      {
        "name": "<",
        "comment": "",
        "type": "comparable -> comparable -> Basics.Bool",
        "associativity": "non",
        "precedence": 4
      },
      {
        "name": "<<",
        "comment": " Function composition, passing results along in the suggested direction. For\\nexample, the following code checks if the square root of a number is odd:\\n\\n    not << isEven << sqrt\\n\\nYou can think of this operator as equivalent to the following:\\n\\n    (g << f)  ==  (\\x -> g (f x))\\n\\nSo our example expands out to something like this:\\n\\n    \\
 -> not (isEven (sqrt n))\\n",
        "type": "(b -> c) -> (a -> b) -> a -> c",
        "associativity": "left",
        "precedence": 9
      },
      {
        "name": "<=",
        "comment": "",
        "type": "comparable -> comparable -> Basics.Bool",
        "associativity": "non",
        "precedence": 4
      },
      {
        "name": "<|",
        "comment": " Saying `f <| x` is exactly the same as `f x`.\\n\\nIt can help you avoid parentheses, which can be nice sometimes. Maybe you want\\nto apply a function to a `case` expression? That sort of thing.\\n",
        "type": "(a -> b) -> a -> b",
        "associativity": "right",
        "precedence": 0
      },
      {
        "name": "==",
        "comment": " Check if values are &ldquo;the same&rdquo;.\\n\\n**Note:** Elm uses structural equality on tuples, records, and user-defined\\nunion types. This means the values `(3, 4)` and `(3, 4)` are definitely equal.\\nThis is not true in languages like JavaScript that use reference equality on\\nobjects.\\n\\n**Note:** Equality (in the Elm sense) is not possible for certain types. For\\nexample, the functions `(\\
 -> n + 1)` and `(\\
 -> 1 + n)` are &ldquo;the\\nsame&rdquo; but detecting this in general is [undecidable][]. In a future\\nrelease, the compiler will detect when `(==)` is used with problematic\\ntypes and provide a helpful error message. This will require quite serious\\ninfrastructure work that makes sense to batch with another big project, so the\\nstopgap is to crash as quickly as possible. Problematic types include functions\\nand JavaScript values like `Json.Encode.Value` which could contain functions\\nif passed through a port.\\n\\n[undecidable]: https://en.wikipedia.org/wiki/Undecidable_problem\\n",
        "type": "a -> a -> Basics.Bool",
        "associativity": "non",
        "precedence": 4
      },
      {
        "name": ">",
        "comment": "",
        "type": "comparable -> comparable -> Basics.Bool",
        "associativity": "non",
        "precedence": 4
      },
      {
        "name": ">=",
        "comment": "",
        "type": "comparable -> comparable -> Basics.Bool",
        "associativity": "non",
        "precedence": 4
      },
      {
        "name": ">>",
        "comment": " Function composition, passing results along in the suggested direction. For\\nexample, the following code checks if the square root of a number is odd:\\n\\n    sqrt >> isEven >> not\\n\\n",
        "type": "(a -> b) -> (b -> c) -> a -> c",
        "associativity": "right",
        "precedence": 9
      },
      {
        "name": "^",
        "comment": " Exponentiation\\n\\n    3^2 == 9\\n    3^3 == 27\\n",
        "type": "number -> number -> number",
        "associativity": "right",
        "precedence": 8
      },
      {
        "name": "|>",
        "comment": " Saying `x |> f` is exactly the same as `f x`.\\n\\nIt is called the “pipe” operator because it lets you write “pipelined” code.\\nFor example, say we have a `sanitize` function for turning user input into\\nintegers:\\n\\n    -- BEFORE\\n    sanitize : String -> Maybe Int\\n    sanitize input =\\n      String.toInt (String.trim input)\\n\\nWe can rewrite it like this:\\n\\n    -- AFTER\\n    sanitize : String -> Maybe Int\\n    sanitize input =\\n      input\\n        |> String.trim\\n        |> String.toInt\\n\\nTotally equivalent! I recommend trying to rewrite code that uses `x |> f`\\ninto code like `f x` until there are no pipes left. That can help you build\\nyour intuition.\\n\\n**Note:** This can be overused! I think folks find it quite neat, but when you\\nhave three or four steps, the code often gets clearer if you break out a\\ntop-level helper function. Now the transformation has a name. The arguments are\\nnamed. It has a type annotation. It is much more self-documenting that way!\\nTesting the logic gets easier too. Nice side benefit!\\n",
        "type": "a -> (a -> b) -> b",
        "associativity": "left",
        "precedence": 0
      },
      {
        "name": "||",
        "comment": " The logical OR operator. `True` if one or both inputs are `True`.\\n\\n    True  || True  == True\\n    True  || False == True\\n    False || True  == True\\n    False || False == False\\n\\n**Note:** When used in the infix position, like `(left || right)`, the operator\\nshort-circuits. This means if `left` is `True` we do not bother evaluating `right`\\nand just return `True` overall.\\n",
        "type": "Basics.Bool -> Basics.Bool -> Basics.Bool",
        "associativity": "right",
        "precedence": 2
      }
    ]
  },
  {
    "name": "Bitwise",
    "comment": " Library for [bitwise operations](https://en.wikipedia.org/wiki/Bitwise_operation).\\n\\n# Basic Operations\\n@docs and, or, xor, complement\\n\\n# Bit Shifts\\n@docs shiftLeftBy, shiftRightBy, shiftRightZfBy\\n",
    "unions": [],
    "aliases": [],
    "values": [
      {
        "name": "and",
        "comment": " Bitwise AND\\n",
        "type": "Basics.Int -> Basics.Int -> Basics.Int"
      },
      {
        "name": "complement",
        "comment": " Flip each bit individually, often called bitwise NOT\\n",
        "type": "Basics.Int -> Basics.Int"
      },
      {
        "name": "or",
        "comment": " Bitwise OR\\n",
        "type": "Basics.Int -> Basics.Int -> Basics.Int"
      },
      {
        "name": "shiftLeftBy",
        "comment": " Shift bits to the left by a given offset, filling new bits with zeros.\\nThis can be used to multiply numbers by powers of two.\\n\\n    shiftLeftBy 1 5 == 10\\n    shiftLeftBy 5 1 == 32\\n",
        "type": "Basics.Int -> Basics.Int -> Basics.Int"
      },
      {
        "name": "shiftRightBy",
        "comment": " Shift bits to the right by a given offset, filling new bits with\\nwhatever is the topmost bit. This can be used to divide numbers by powers of two.\\n\\n    shiftRightBy 1  32 == 16\\n    shiftRightBy 2  32 == 8\\n    shiftRightBy 1 -32 == -16\\n\\nThis is called an [arithmetic right shift][ars], often written `>>`, and\\nsometimes called a sign-propagating right shift because it fills empty spots\\nwith copies of the highest bit.\\n\\n[ars]: https://en.wikipedia.org/wiki/Bitwise_operation#Arithmetic_shift\\n",
        "type": "Basics.Int -> Basics.Int -> Basics.Int"
      },
      {
        "name": "shiftRightZfBy",
        "comment": " Shift bits to the right by a given offset, filling new bits with zeros.\\n\\n    shiftRightZfBy 1  32 == 16\\n    shiftRightZfBy 2  32 == 8\\n    shiftRightZfBy 1 -32 == 2147483632\\n\\nThis is called an [logical right shift][lrs], often written `>>>`, and\\nsometimes called a zero-fill right shift because it fills empty spots with\\nzeros.\\n\\n[lrs]: https://en.wikipedia.org/wiki/Bitwise_operation#Logical_shift\\n",
        "type": "Basics.Int -> Basics.Int -> Basics.Int"
      },
      {
        "name": "xor",
        "comment": " Bitwise XOR\\n",
        "type": "Basics.Int -> Basics.Int -> Basics.Int"
      }
    ],
    "binops": []
  },
  {
    "name": "Char",
    "comment": " Functions for working with characters. Character literals are enclosed in\\n`'a'` pair of single quotes.\\n\\n# Characters\\n@docs Char\\n\\n# ASCII Letters\\n@docs isUpper, isLower, isAlpha, isAlphaNum\\n\\n# Digits\\n@docs isDigit, isOctDigit, isHexDigit\\n\\n# Conversion\\n@docs toUpper, toLower, toLocaleUpper, toLocaleLower\\n\\n# Unicode Code Points\\n@docs toCode, fromCode\\n",
    "unions": [
      {
        "name": "Char",
        "comment": " A `Char` is a single [unicode][u] character:\\n\\n    'a'\\n    '0'\\n    'Z'\\n    '?'\\n    '"'\\n    'Σ'\\n    '🙈'\\n\\n    '\\t'\\n    '\\"'\\n    '\\''\\n    '\\u{1F648}' -- '🙈'\\n\\n**Note 1:** You _cannot_ use single quotes around multiple characters like in\\nJavaScript. This is how we distinguish [`String`](String#String) and `Char`\\nvalues in syntax.\\n\\n**Note 2:** You can use the unicode escapes from `\\u{0000}` to `\\u{10FFFF}` to\\nrepresent characters by their code point. You can also include the unicode\\ncharacters directly. Using the escapes can be better if you need one of the\\nmany whitespace characters with different widths.\\n\\n[u]: https://en.wikipedia.org/wiki/Unicode\\n",
        "args": [],
        "cases": []
      }
    ],
    "aliases": [],
    "values": [
      {
        "name": "fromCode",
        "comment": " Convert a Unicode [code point][cp] to a character.\\n\\n    fromCode 65      == 'A'\\n    fromCode 66      == 'B'\\n    fromCode 0x6728  == '木'\\n    fromCode 0x1D306 == '𝌆'\\n    fromCode 0x1F603 == '😃'\\n    fromCode -1      == '�'\\n\\nThe full range of unicode is from `0` to `0x10FFFF`. With numbers outside that\\nrange, you get [the replacement character][fffd].\\n\\n[cp]: https://en.wikipedia.org/wiki/Code_point\\n[fffd]: https://en.wikipedia.org/wiki/Specials_(Unicode_block)#Replacement_character\\n",
        "type": "Basics.Int -> Char.Char"
      },
      {
        "name": "isAlpha",
        "comment": " Detect upper case and lower case ASCII characters.\\n\\n    isAlpha 'a' == True\\n    isAlpha 'b' == True\\n    isAlpha 'E' == True\\n    isAlpha 'Y' == True\\n\\n    isAlpha '0' == False\\n    isAlpha '-' == False\\n    isAlpha 'π' == False\\n",
        "type": "Char.Char -> Basics.Bool"
      },
      {
        "name": "isAlphaNum",
        "comment": " Detect upper case and lower case ASCII characters.\\n\\n    isAlphaNum 'a' == True\\n    isAlphaNum 'b' == True\\n    isAlphaNum 'E' == True\\n    isAlphaNum 'Y' == True\\n    isAlphaNum '0' == True\\n    isAlphaNum '7' == True\\n\\n    isAlphaNum '-' == False\\n    isAlphaNum 'π' == False\\n",
        "type": "Char.Char -> Basics.Bool"
      },
      {
        "name": "isDigit",
        "comment": " Detect digits `0123456789`\\n\\n    isDigit '0' == True\\n    isDigit '1' == True\\n    ...\\n    isDigit '9' == True\\n\\n    isDigit 'a' == False\\n    isDigit 'b' == False\\n    isDigit 'A' == False\\n",
        "type": "Char.Char -> Basics.Bool"
      },
      {
        "name": "isHexDigit",
        "comment": " Detect hexadecimal digits `0123456789abcdefABCDEF`\\n",
        "type": "Char.Char -> Basics.Bool"
      },
      {
        "name": "isLower",
        "comment": " Detect lower case ASCII characters.\\n\\n    isLower 'a' == True\\n    isLower 'b' == True\\n    ...\\n    isLower 'z' == True\\n\\n    isLower '0' == False\\n    isLower 'A' == False\\n    isLower '-' == False\\n    isLower 'π' == False\\n",
        "type": "Char.Char -> Basics.Bool"
      },
      {
        "name": "isOctDigit",
        "comment": " Detect octal digits `01234567`\\n\\n    isOctDigit '0' == True\\n    isOctDigit '1' == True\\n    ...\\n    isOctDigit '7' == True\\n\\n    isOctDigit '8' == False\\n    isOctDigit 'a' == False\\n    isOctDigit 'A' == False\\n",
        "type": "Char.Char -> Basics.Bool"
      },
      {
        "name": "isUpper",
        "comment": " Detect upper case ASCII characters.\\n\\n    isUpper 'A' == True\\n    isUpper 'B' == True\\n    ...\\n    isUpper 'Z' == True\\n\\n    isUpper '0' == False\\n    isUpper 'a' == False\\n    isUpper '-' == False\\n    isUpper 'Σ' == False\\n",
        "type": "Char.Char -> Basics.Bool"
      },
      {
        "name": "toCode",
        "comment": " Convert to the corresponding Unicode [code point][cp].\\n\\n[cp]: https://en.wikipedia.org/wiki/Code_point\\n\\n    toCode 'A' == 65\\n    toCode 'B' == 66\\n    toCode '木' == 0x6728\\n    toCode '𝌆' == 0x1D306\\n    toCode '😃' == 0x1F603\\n",
        "type": "Char.Char -> Basics.Int"
      },
      {
        "name": "toLocaleLower",
        "comment": " Convert to lower case, according to any locale-specific case mappings. ",
        "type": "Char.Char -> Char.Char"
      },
      {
        "name": "toLocaleUpper",
        "comment": " Convert to upper case, according to any locale-specific case mappings. ",
        "type": "Char.Char -> Char.Char"
      },
      {
        "name": "toLower",
        "comment": " Convert to lower case. ",
        "type": "Char.Char -> Char.Char"
      },
      {
        "name": "toUpper",
        "comment": " Convert to upper case. ",
        "type": "Char.Char -> Char.Char"
      }
    ],
    "binops": []
  },
  {
    "name": "Debug",
    "comment": " This module can be useful while _developing_ an application. It is not\\navailable for use in packages or production.\\n\\n# Debugging\\n@docs toString, log, todo\\n",
    "unions": [],
    "aliases": [],
    "values": [
      {
        "name": "log",
        "comment": " Log a tagged value on the developer console, and then return the value.\\n\\n    1 + log "number" 1        -- equals 2, logs "number: 1"\\n    length (log "start" [])   -- equals 0, logs "start: []"\\n\\nIt is often possible to sprinkle this around to see if values are what you\\nexpect. It is kind of old-school to do it this way, but it works!\\n\\n**Note:** This is not available with `elm make --optimize` because (1) it\\nrelies on `toString` which has the same restriction and (2) it is not a pure\\nfunction and would therefore have unpredictable behavior when paired with\\ncompiler optimizations that move code around.\\n\\n**Note:** If you want to create a terminal application that prints stuff out,\\nuse ports for now. That will give you full access to reading and writing in the\\nterminal. We may have a package in Elm for this someday, but browser\\napplications are the primary focus of platform development for now.\\n",
        "type": "String.String -> a -> a"
      },
      {
        "name": "toString",
        "comment": " Turn any kind of value into a string.\\n\\n    toString 42                == "42"\\n    toString [1,2]             == "[1,2]"\\n    toString ('a', "cat", 13)  == "('a', \\"cat\\", 13)"\\n    toString "he said, \\"hi\\"" == "\\"he said, \\\\\\"hi\\\\\\"\\""\\n\\nNotice that with strings, this is not the `identity` function. It escapes\\ncharacters so if you say `Html.text (toString "he said, \\"hi\\"")` it will\\nshow `"he said, \\"hi\\""` rather than `he said, "hi"`. This makes it nice\\nfor viewing Elm data structures.\\n\\n**Note:** This is not available with `elm make --optimize` which gets rid of\\na bunch of runtime metadata. For example, it shortens record field names, and\\nwe need that info to `toString` the value! As a consequence, packages cannot\\nuse `toString` because they may be used in `--optimize` mode.\\n",
        "type": "a -> String.String"
      },
      {
        "name": "todo",
        "comment": " This is a placeholder for code that you will write later.\\n\\nFor example, if you are working with a large union type and have partially\\ncompleted a case expression, it may make sense to do this:\\n\\n    type Entity = Ship | Fish | Captain | Seagull\\n\\n    drawEntity entity =\\n      case entity of\\n        Ship ->\\n          ...\\n\\n        Fish ->\\n          ...\\n\\n        _ ->\\n          Debug.todo "handle Captain and Seagull"\\n\\nThe Elm compiler recognizes each `Debug.todo` so if you run into it, you get\\nan **uncatchable runtime exception** that includes the module name and line\\nnumber.\\n\\n**Note:** This is not available with `elm make --optimize` or packages. The\\nidea is that a `todo` can be useful during development, but uncatchable runtime\\nexceptions should not appear in the resulting applications.\\n\\n**Note:** For the equivalent of try/catch error handling in Elm, use modules\\nlike [`Maybe`](#Maybe) and [`Result`](#Result) which guarantee that no error\\ngoes unhandled!\\n",
        "type": "String.String -> a"
      }
    ],
    "binops": []
  },
  {
    "name": "Dict",
    "comment": " A dictionary mapping unique keys to values. The keys can be any comparable\\ntype. This includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or\\nlists of comparable types.\\n\\nInsert, remove, and query operations all take *O(log n)* time.\\n\\n# Dictionaries\\n@docs Dict\\n\\n# Build\\n@docs empty, singleton, insert, update, remove\\n\\n# Query\\n@docs isEmpty, member, get, size\\n\\n# Lists\\n@docs keys, values, toList, fromList\\n\\n# Transform\\n@docs map, foldl, foldr, filter, partition\\n\\n# Combine\\n@docs union, intersect, diff, merge\\n\\n",
    "unions": [
      {
        "name": "Dict",
        "comment": " A dictionary of keys and values. So a `Dict String User` is a dictionary\\nthat lets you look up a `String` (such as user names) and find the associated\\n`User`.\\n\\n    import Dict exposing (Dict)\\n\\n    users : Dict String User\\n    users =\\n      Dict.fromList\\n        [ ("Alice", User "Alice" 28 1.65)\\n        , ("Bob"  , User "Bob"   19 1.82)\\n        , ("Chuck", User "Chuck" 33 1.75)\\n        ]\\n\\n    type alias User =\\n      { name : String\\n      , age : Int\\n      , height : Float\\n      }\\n",
        "args": [
          "k",
          "v"
        ],
        "cases": []
      }
    ],
    "aliases": [],
    "values": [
      {
        "name": "diff",
        "comment": " Keep a key-value pair when its key does not appear in the second dictionary.\\n",
        "type": "Dict.Dict comparable a -> Dict.Dict comparable b -> Dict.Dict comparable a"
      },
      {
        "name": "empty",
        "comment": " Create an empty dictionary. ",
        "type": "Dict.Dict k v"
      },
      {
        "name": "filter",
        "comment": " Keep only the key-value pairs that pass the given test. ",
        "type": "(comparable -> v -> Basics.Bool) -> Dict.Dict comparable v -> Dict.Dict comparable v"
      },
      {
        "name": "foldl",
        "comment": " Fold over the key-value pairs in a dictionary from lowest key to highest key.\\n\\n    import Dict exposing (Dict)\\n\\n    getAges : Dict String User -> List String\\n    getAges users =\\n      Dict.foldl addAge [] users\\n\\n    addAge : String -> User -> List String -> List String\\n    addAge _ user ages =\\n      user.age :: ages\\n\\n    -- getAges users == [33,19,28]\\n",
        "type": "(k -> v -> b -> b) -> b -> Dict.Dict k v -> b"
      },
      {
        "name": "foldr",
        "comment": " Fold over the key-value pairs in a dictionary from highest key to lowest key.\\n\\n    import Dict exposing (Dict)\\n\\n    getAges : Dict String User -> List String\\n    getAges users =\\n      Dict.foldr addAge [] users\\n\\n    addAge : String -> User -> List String -> List String\\n    addAge _ user ages =\\n      user.age :: ages\\n\\n    -- getAges users == [28,19,33]\\n",
        "type": "(k -> v -> b -> b) -> b -> Dict.Dict k v -> b"
      },
      {
        "name": "fromList",
        "comment": " Convert an association list into a dictionary. ",
        "type": "List.List ( comparable, v ) -> Dict.Dict comparable v"
      },
      {
        "name": "get",
        "comment": " Get the value associated with a key. If the key is not found, return\\n`Nothing`. This is useful when you are not sure if a key will be in the\\ndictionary.\\n\\n    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]\\n\\n    get "Tom"   animals == Just Cat\\n    get "Jerry" animals == Just Mouse\\n    get "Spike" animals == Nothing\\n\\n",
        "type": "comparable -> Dict.Dict comparable v -> Maybe.Maybe v"
      },
      {
        "name": "insert",
        "comment": " Insert a key-value pair into a dictionary. Replaces value when there is\\na collision. ",
        "type": "comparable -> v -> Dict.Dict comparable v -> Dict.Dict comparable v"
      },
      {
        "name": "intersect",
        "comment": " Keep a key-value pair when its key appears in the second dictionary.\\nPreference is given to values in the first dictionary.\\n",
        "type": "Dict.Dict comparable v -> Dict.Dict comparable v -> Dict.Dict comparable v"
      },
      {
        "name": "isEmpty",
        "comment": " Determine if a dictionary is empty.\\n\\n    isEmpty empty == True\\n",
        "type": "Dict.Dict k v -> Basics.Bool"
      },
      {
        "name": "keys",
        "comment": " Get all of the keys in a dictionary, sorted from lowest to highest.\\n\\n    keys (fromList [(0,"Alice"),(1,"Bob")]) == [0,1]\\n",
        "type": "Dict.Dict k v -> List.List k"
      },
      {
        "name": "map",
        "comment": " Apply a function to all values in a dictionary.\\n",
        "type": "(k -> a -> b) -> Dict.Dict k a -> Dict.Dict k b"
      },
      {
        "name": "member",
        "comment": " Determine if a key is in a dictionary. ",
        "type": "comparable -> Dict.Dict comparable v -> Basics.Bool"
      },
      {
        "name": "merge",
        "comment": " The most general way of combining two dictionaries. You provide three\\naccumulators for when a given key appears:\\n\\n  1. Only in the left dictionary.\\n  2. In both dictionaries.\\n  3. Only in the right dictionary.\\n\\nYou then traverse all the keys from lowest to highest, building up whatever\\nyou want.\\n",
        "type": "(comparable -> a -> result -> result) -> (comparable -> a -> b -> result -> result) -> (comparable -> b -> result -> result) -> Dict.Dict comparable a -> Dict.Dict comparable b -> result -> result"
      },
      {
        "name": "partition",
        "comment": " Partition a dictionary according to some test. The first dictionary\\ncontains all key-value pairs which passed the test, and the second contains\\nthe pairs that did not.\\n",
        "type": "(comparable -> v -> Basics.Bool) -> Dict.Dict comparable v -> ( Dict.Dict comparable v, Dict.Dict comparable v )"
      },
      {
        "name": "remove",
        "comment": " Remove a key-value pair from a dictionary. If the key is not found,\\nno changes are made. ",
        "type": "comparable -> Dict.Dict comparable v -> Dict.Dict comparable v"
      },
      {
        "name": "singleton",
        "comment": " Create a dictionary with one key-value pair. ",
        "type": "comparable -> v -> Dict.Dict comparable v"
      },
      {
        "name": "size",
        "comment": " Determine the number of key-value pairs in the dictionary. ",
        "type": "Dict.Dict k v -> Basics.Int"
      },
      {
        "name": "toList",
        "comment": " Convert a dictionary into an association list of key-value pairs, sorted by keys. ",
        "type": "Dict.Dict k v -> List.List ( k, v )"
      },
      {
        "name": "union",
        "comment": " Combine two dictionaries. If there is a collision, preference is given\\nto the first dictionary.\\n",
        "type": "Dict.Dict comparable v -> Dict.Dict comparable v -> Dict.Dict comparable v"
      },
      {
        "name": "update",
        "comment": " Update the value of a dictionary for a specific key with a given function. ",
        "type": "comparable -> (Maybe.Maybe v -> Maybe.Maybe v) -> Dict.Dict comparable v -> Dict.Dict comparable v"
      },
      {
        "name": "values",
        "comment": " Get all of the values in a dictionary, in the order of their keys.\\n\\n    values (fromList [(0,"Alice"),(1,"Bob")]) == ["Alice", "Bob"]\\n",
        "type": "Dict.Dict k v -> List.List v"
      }
    ],
    "binops": []
  },
  {
    "name": "List",
    "comment": " You can create a `List` in Elm with the `[1,2,3]` syntax, so lists are\\nused all over the place. This module has a bunch of functions to help you work\\nwith them!\\n\\n# Create\\n@docs singleton, repeat, range, (::)\\n\\n# Transform\\n@docs map, indexedMap, foldl, foldr, filter, filterMap\\n\\n# Utilities\\n@docs length, reverse, member, all, any, maximum, minimum, sum, product\\n\\n# Combine\\n@docs append, concat, concatMap, intersperse, map2, map3, map4, map5\\n\\n# Sort\\n@docs sort, sortBy, sortWith\\n\\n# Deconstruct\\n@docs isEmpty, head, tail, take, drop, partition, unzip\\n\\n",
    "unions": [],
    "aliases": [],
    "values": [
      {
        "name": "all",
        "comment": " Determine if all elements satisfy some test.\\n\\n    all isEven [2,4] == True\\n    all isEven [2,3] == False\\n    all isEven [] == True\\n",
        "type": "(a -> Basics.Bool) -> List.List a -> Basics.Bool"
      },
      {
        "name": "any",
        "comment": " Determine if any elements satisfy some test.\\n\\n    any isEven [2,3] == True\\n    any isEven [1,3] == False\\n    any isEven [] == False\\n",
        "type": "(a -> Basics.Bool) -> List.List a -> Basics.Bool"
      },
      {
        "name": "append",
        "comment": " Put two lists together.\\n\\n    append [1,1,2] [3,5,8] == [1,1,2,3,5,8]\\n    append ['a','b'] ['c'] == ['a','b','c']\\n\\nYou can also use [the `(++)` operator](Basics#++) to append lists.\\n",
        "type": "List.List a -> List.List a -> List.List a"
      },
      {
        "name": "concat",
        "comment": " Concatenate a bunch of lists into a single list:\\n\\n    concat [[1,2],[3],[4,5]] == [1,2,3,4,5]\\n",
        "type": "List.List (List.List a) -> List.List a"
      },
      {
        "name": "concatMap",
        "comment": " Map a given function onto a list and flatten the resulting lists.\\n\\n    concatMap f xs == concat (map f xs)\\n",
        "type": "(a -> List.List b) -> List.List a -> List.List b"
      },
      {
        "name": "drop",
        "comment": " Drop the first *n* members of a list.\\n\\n    drop 2 [1,2,3,4] == [3,4]\\n",
        "type": "Basics.Int -> List.List a -> List.List a"
      },
      {
        "name": "filter",
        "comment": " Keep elements that satisfy the test.\\n\\n    filter isEven [1,2,3,4,5,6] == [2,4,6]\\n",
        "type": "(a -> Basics.Bool) -> List.List a -> List.List a"
      },
      {
        "name": "filterMap",
        "comment": " Filter out certain values. For example, maybe you have a bunch of strings\\nfrom an untrusted source and you want to turn them into numbers:\\n\\n    numbers : List Int\\n    numbers =\\n      filterMap String.toInt ["3", "hi", "12", "4th", "May"]\\n\\n    -- numbers == [3, 12]\\n\\n",
        "type": "(a -> Maybe.Maybe b) -> List.List a -> List.List b"
      },
      {
        "name": "foldl",
        "comment": " Reduce a list from the left.\\n\\n    foldl (+)  0  [1,2,3] == 6\\n    foldl (::) [] [1,2,3] == [3,2,1]\\n\\nSo `foldl step state [1,2,3]` is like saying:\\n\\n    state\\n      |> step 1\\n      |> step 2\\n      |> step 3\\n",
        "type": "(a -> b -> b) -> b -> List.List a -> b"
      },
      {
        "name": "foldr",
        "comment": " Reduce a list from the right.\\n\\n    foldr (+)  0  [1,2,3] == 6\\n    foldr (::) [] [1,2,3] == [1,2,3]\\n\\nSo `foldr step state [1,2,3]` is like saying:\\n\\n    state\\n      |> step 3\\n      |> step 2\\n      |> step 1\\n",
        "type": "(a -> b -> b) -> b -> List.List a -> b"
      },
      {
        "name": "head",
        "comment": " Extract the first element of a list.\\n\\n    head [1,2,3] == Just 1\\n    head [] == Nothing\\n\\n**Note:** It is usually preferable to use a `case` to deconstruct a `List`\\nbecause it gives you `(x :: xs)` and you can work with both subparts.\\n",
        "type": "List.List a -> Maybe.Maybe a"
      },
      {
        "name": "indexedMap",
        "comment": " Same as `map` but the function is also applied to the index of each\\nelement (starting at zero).\\n\\n    indexedMap Tuple.pair ["Tom","Sue","Bob"] == [ (0,"Tom"), (1,"Sue"), (2,"Bob") ]\\n",
        "type": "(Basics.Int -> a -> b) -> List.List a -> List.List b"
      },
      {
        "name": "intersperse",
        "comment": " Places the given value between all members of the given list.\\n\\n    intersperse "on" ["turtles","turtles","turtles"] == ["turtles","on","turtles","on","turtles"]\\n",
        "type": "a -> List.List a -> List.List a"
      },
      {
        "name": "isEmpty",
        "comment": " Determine if a list is empty.\\n\\n    isEmpty [] == True\\n\\n**Note:** It is usually preferable to use a `case` to test this so you do not\\nforget to handle the `(x :: xs)` case as well!\\n",
        "type": "List.List a -> Basics.Bool"
      },
      {
        "name": "length",
        "comment": " Determine the length of a list.\\n\\n    length [1,2,3] == 3\\n",
        "type": "List.List a -> Basics.Int"
      },
      {
        "name": "map",
        "comment": " Apply a function to every element of a list.\\n\\n    map sqrt [1,4,9] == [1,2,3]\\n\\n    map not [True,False,True] == [False,True,False]\\n\\nSo `map func [ a, b, c ]` is the same as `[ func a, func b, func c ]`\\n",
        "type": "(a -> b) -> List.List a -> List.List b"
      },
      {
        "name": "map2",
        "comment": " Combine two lists, combining them with the given function.\\nIf one list is longer, the extra elements are dropped.\\n\\n    totals : List Int -> List Int -> List Int\\n    totals xs ys =\\n      List.map2 (+) xs ys\\n\\n    -- totals [1,2,3] [4,5,6] == [5,7,9]\\n\\n    pairs : List a -> List b -> List (a,b)\\n    pairs xs ys =\\n      List.map2 Tuple.pair xs ys\\n\\n    -- pairs ["alice","bob","chuck"] [2,5,7,8]\\n    --   == [("alice",2),("bob",5),("chuck",7)]\\n\\n",
        "type": "(a -> b -> result) -> List.List a -> List.List b -> List.List result"
      },
      {
        "name": "map3",
        "comment": "",
        "type": "(a -> b -> c -> result) -> List.List a -> List.List b -> List.List c -> List.List result"
      },
      {
        "name": "map4",
        "comment": "",
        "type": "(a -> b -> c -> d -> result) -> List.List a -> List.List b -> List.List c -> List.List d -> List.List result"
      },
      {
        "name": "map5",
        "comment": "",
        "type": "(a -> b -> c -> d -> e -> result) -> List.List a -> List.List b -> List.List c -> List.List d -> List.List e -> List.List result"
      },
      {
        "name": "maximum",
        "comment": " Find the maximum element in a non-empty list.\\n\\n    maximum [1,4,2] == Just 4\\n    maximum []      == Nothing\\n",
        "type": "List.List comparable -> Maybe.Maybe comparable"
      },
      {
        "name": "member",
        "comment": " Figure out whether a list contains a value.\\n\\n    member 9 [1,2,3,4] == False\\n    member 4 [1,2,3,4] == True\\n",
        "type": "a -> List.List a -> Basics.Bool"
      },
      {
        "name": "minimum",
        "comment": " Find the minimum element in a non-empty list.\\n\\n    minimum [3,2,1] == Just 1\\n    minimum []      == Nothing\\n",
        "type": "List.List comparable -> Maybe.Maybe comparable"
      },
      {
        "name": "partition",
        "comment": " Partition a list based on some test. The first list contains all values\\nthat satisfy the test, and the second list contains all the value that do not.\\n\\n    partition (\\x -> x < 3) [0,1,2,3,4,5] == ([0,1,2], [3,4,5])\\n    partition isEven        [0,1,2,3,4,5] == ([0,2,4], [1,3,5])\\n",
        "type": "(a -> Basics.Bool) -> List.List a -> ( List.List a, List.List a )"
      },
      {
        "name": "product",
        "comment": " Get the product of the list elements.\\n\\n    product [1,2,3,4] == 24\\n",
        "type": "List.List number -> number"
      },
      {
        "name": "range",
        "comment": " Create a list of numbers, every element increasing by one.\\nYou give the lowest and highest number that should be in the list.\\n\\n    range 3 6 == [3, 4, 5, 6]\\n    range 3 3 == [3]\\n    range 6 3 == []\\n",
        "type": "Basics.Int -> Basics.Int -> List.List Basics.Int"
      },
      {
        "name": "repeat",
        "comment": " Create a list with *n* copies of a value:\\n\\n    repeat 3 (0,0) == [(0,0),(0,0),(0,0)]\\n",
        "type": "Basics.Int -> a -> List.List a"
      },
      {
        "name": "reverse",
        "comment": " Reverse a list.\\n\\n    reverse [1,2,3,4] == [4,3,2,1]\\n",
        "type": "List.List a -> List.List a"
      },
      {
        "name": "singleton",
        "comment": " Create a list with only one element:\\n\\n    singleton 1234 == [1234]\\n    singleton "hi" == ["hi"]\\n",
        "type": "a -> List.List a"
      },
      {
        "name": "sort",
        "comment": " Sort values from lowest to highest\\n\\n    sort [3,1,5] == [1,3,5]\\n",
        "type": "List.List comparable -> List.List comparable"
      },
      {
        "name": "sortBy",
        "comment": " Sort values by a derived property.\\n\\n    alice = { name="Alice", height=1.62 }\\n    bob   = { name="Bob"  , height=1.85 }\\n    chuck = { name="Chuck", height=1.76 }\\n\\n    sortBy .name   [chuck,alice,bob] == [alice,bob,chuck]\\n    sortBy .height [chuck,alice,bob] == [alice,chuck,bob]\\n\\n    sortBy String.length ["mouse","cat"] == ["cat","mouse"]\\n",
        "type": "(a -> comparable) -> List.List a -> List.List a"
      },
      {
        "name": "sortWith",
        "comment": " Sort values with a custom comparison function.\\n\\n    sortWith flippedComparison [1,2,3,4,5] == [5,4,3,2,1]\\n\\n    flippedComparison a b =\\n        case compare a b of\\n          LT -> GT\\n          EQ -> EQ\\n          GT -> LT\\n\\nThis is also the most general sort function, allowing you\\nto define any other: `sort == sortWith compare`\\n",
        "type": "(a -> a -> Basics.Order) -> List.List a -> List.List a"
      },
      {
        "name": "sum",
        "comment": " Get the sum of the list elements.\\n\\n    sum [1,2,3,4] == 10\\n",
        "type": "List.List number -> number"
      },
      {
        "name": "tail",
        "comment": " Extract the rest of the list.\\n\\n    tail [1,2,3] == Just [2,3]\\n    tail [] == Nothing\\n\\n**Note:** It is usually preferable to use a `case` to deconstruct a `List`\\nbecause it gives you `(x :: xs)` and you can work with both subparts.\\n",
        "type": "List.List a -> Maybe.Maybe (List.List a)"
      },
      {
        "name": "take",
        "comment": " Take the first *n* members of a list.\\n\\n    take 2 [1,2,3,4] == [1,2]\\n",
        "type": "Basics.Int -> List.List a -> List.List a"
      },
      {
        "name": "unzip",
        "comment": " Decompose a list of tuples into a tuple of lists.\\n\\n    unzip [(0, True), (17, False), (1337, True)] == ([0,17,1337], [True,False,True])\\n",
        "type": "List.List ( a, b ) -> ( List.List a, List.List b )"
      }
    ],
    "binops": [
      {
        "name": "::",
        "comment": " Add an element to the front of a list.\\n\\n    1 :: [2,3] == [1,2,3]\\n    1 :: [] == [1]\\n\\nThis operator is pronounced *cons* for historical reasons, but you can think\\nof it like pushing an entry onto a stack.\\n",
        "type": "a -> List.List a -> List.List a",
        "associativity": "right",
        "precedence": 5
      }
    ]
  },
  {
    "name": "Maybe",
    "comment": " This library fills a bunch of important niches in Elm. A `Maybe` can help\\nyou with optional arguments, error handling, and records with optional fields.\\n\\n# Definition\\n@docs Maybe\\n\\n# Common Helpers\\n@docs withDefault, map, map2, map3, map4, map5\\n\\n# Chaining Maybes\\n@docs andThen\\n",
    "unions": [
      {
        "name": "Maybe",
        "comment": " Represent values that may or may not exist. It can be useful if you have a\\nrecord field that is only filled in sometimes. Or if a function takes a value\\nsometimes, but does not absolutely need it.\\n\\n    -- A person, but maybe we do not know their age.\\n    type alias Person =\\n        { name : String\\n        , age : Maybe Int\\n        }\\n\\n    tom = { name = "Tom", age = Just 42 }\\n    sue = { name = "Sue", age = Nothing }\\n",
        "args": [
          "a"
        ],
        "cases": [
          [
            "Just",
            [
              "a"
            ]
          ],
          [
            "Nothing",
            []
          ]
        ]
      }
    ],
    "aliases": [],
    "values": [
      {
        "name": "andThen",
        "comment": " Chain together many computations that may fail. It is helpful to see its\\ndefinition:\\n\\n    andThen : (a -> Maybe b) -> Maybe a -> Maybe b\\n    andThen callback maybe =\\n        case maybe of\\n            Just value ->\\n                callback value\\n\\n            Nothing ->\\n                Nothing\\n\\nThis means we only continue with the callback if things are going well. For\\nexample, say you need to parse some user input as a month:\\n\\n    parseMonth : String -> Maybe Int\\n    parseMonth userInput =\\n        String.toInt userInput\\n          |> andThen toValidMonth\\n\\n    toValidMonth : Int -> Maybe Int\\n    toValidMonth month =\\n        if 1 <= month && month <= 12 then\\n            Just month\\n        else\\n            Nothing\\n\\nIn the `parseMonth` function, if `String.toInt` produces `Nothing` (because\\nthe `userInput` was not an integer) this entire chain of operations will\\nshort-circuit and result in `Nothing`. If `toValidMonth` results in `Nothing`,\\nagain the chain of computations will result in `Nothing`.\\n",
        "type": "(a -> Maybe.Maybe b) -> Maybe.Maybe a -> Maybe.Maybe b"
      },
      {
        "name": "map",
        "comment": " Transform a `Maybe` value with a given function:\\n\\n    map sqrt (Just 9) == Just 3\\n    map sqrt Nothing  == Nothing\\n\\n    map sqrt (String.toFloat "9") == Just 3\\n    map sqrt (String.toFloat "x") == Nothing\\n\\n",
        "type": "(a -> b) -> Maybe.Maybe a -> Maybe.Maybe b"
      },
      {
        "name": "map2",
        "comment": " Apply a function if all the arguments are `Just` a value.\\n\\n    map2 (+) (Just 3) (Just 4) == Just 7\\n    map2 (+) (Just 3) Nothing == Nothing\\n    map2 (+) Nothing (Just 4) == Nothing\\n\\n    map2 (+) (String.toInt "1") (String.toInt "123") == Just 124\\n    map2 (+) (String.toInt "x") (String.toInt "123") == Nothing\\n    map2 (+) (String.toInt "1") (String.toInt "1.3") == Nothing\\n",
        "type": "(a -> b -> value) -> Maybe.Maybe a -> Maybe.Maybe b -> Maybe.Maybe value"
      },
      {
        "name": "map3",
        "comment": "",
        "type": "(a -> b -> c -> value) -> Maybe.Maybe a -> Maybe.Maybe b -> Maybe.Maybe c -> Maybe.Maybe value"
      },
      {
        "name": "map4",
        "comment": "",
        "type": "(a -> b -> c -> d -> value) -> Maybe.Maybe a -> Maybe.Maybe b -> Maybe.Maybe c -> Maybe.Maybe d -> Maybe.Maybe value"
      },
      {
        "name": "map5",
        "comment": "",
        "type": "(a -> b -> c -> d -> e -> value) -> Maybe.Maybe a -> Maybe.Maybe b -> Maybe.Maybe c -> Maybe.Maybe d -> Maybe.Maybe e -> Maybe.Maybe value"
      },
      {
        "name": "withDefault",
        "comment": " Provide a default value, turning an optional value into a normal\\nvalue.  This comes in handy when paired with functions like\\n[`Dict.get`](Dict#get) which gives back a `Maybe`.\\n\\n    withDefault 100 (Just 42)   -- 42\\n    withDefault 100 Nothing     -- 100\\n\\n    withDefault "unknown" (Dict.get "Tom" Dict.empty)   -- "unknown"\\n\\n**Note:** This can be overused! Many cases are better handled by a `case`\\nexpression. And if you end up using `withDefault` a lot, it can be a good sign\\nthat a [custom type][ct] will clean your code up quite a bit!\\n\\n[ct]: https://guide.elm-lang.org/types/custom_types.html\\n",
        "type": "a -> Maybe.Maybe a -> a"
      }
    ],
    "binops": []
  },
  {
    "name": "Platform",
    "comment": "\\n\\n# Programs\\n@docs Program, worker\\n\\n# Platform Internals\\n\\n## Tasks and Processes\\n@docs Task, ProcessId\\n\\n## Effect Manager Helpers\\n\\nAn extremely tiny portion of library authors should ever write effect managers.\\nFundamentally, Elm needs maybe 10 of them total. I get that people are smart,\\ncurious, etc. but that is not a substitute for a legitimate reason to make an\\neffect manager. Do you have an *organic need* this fills? Or are you just\\ncurious? Public discussions of your explorations should be framed accordingly.\\n\\n@docs Router, sendToApp, sendToSelf\\n",
    "unions": [
      {
        "name": "ProcessId",
        "comment": " Head over to the documentation for the [`Process`](Process) module for\\ninformation on this. It is only defined here because it is a platform\\nprimitive.\\n",
        "args": [],
        "cases": []
      },
      {
        "name": "Program",
        "comment": " A `Program` describes an Elm program! How does it react to input? Does it\\nshow anything on screen? Etc.\\n",
        "args": [
          "flags",
          "model",
          "msg"
        ],
        "cases": []
      },
      {
        "name": "Router",
        "comment": " An effect manager has access to a “router” that routes messages between\\nthe main app and your individual effect manager.\\n",
        "args": [
          "appMsg",
          "selfMsg"
        ],
        "cases": []
      },
      {
        "name": "Task",
        "comment": " Head over to the documentation for the [`Task`](Task) module for more\\ninformation on this. It is only defined here because it is a platform\\nprimitive.\\n",
        "args": [
          "err",
          "ok"
        ],
        "cases": []
      }
    ],
    "aliases": [],
    "values": [
      {
        "name": "sendToApp",
        "comment": " Send the router a message for the main loop of your app. This message will\\nbe handled by the overall `update` function, just like events from `Html`.\\n",
        "type": "Platform.Router msg a -> msg -> Platform.Task x ()"
      },
      {
        "name": "sendToSelf",
        "comment": " Send the router a message for your effect manager. This message will\\nbe routed to the `onSelfMsg` function, where you can update the state of your\\neffect manager as necessary.\\n\\nAs an example, the effect manager for web sockets\\n",
        "type": "Platform.Router a msg -> msg -> Platform.Task x ()"
      },
      {
        "name": "worker",
        "comment": " Create a [headless][] program with no user interface.\\n\\nThis is great if you want to use Elm as the &ldquo;brain&rdquo; for something\\nelse. For example, you could send messages out ports to modify the DOM, but do\\nall the complex logic in Elm.\\n\\n[headless]: https://en.wikipedia.org/wiki/Headless_software\\n\\nInitializing a headless program from JavaScript looks like this:\\n\\n```javascript\\nvar app = Elm.MyThing.init();\\n```\\n\\nIf _do_ want to control the user interface in Elm, the [`Browser`][browser]\\nmodule has a few ways to create that kind of `Program` instead!\\n\\n[headless]: https://en.wikipedia.org/wiki/Headless_software\\n[browser]: /packages/elm/browser/latest/Browser\\n",
        "type": "{ init : flags -> ( model, Platform.Cmd.Cmd msg ), update : msg -> model -> ( model, Platform.Cmd.Cmd msg ), subscriptions : model -> Platform.Sub.Sub msg } -> Platform.Program flags model msg"
      }
    ],
    "binops": []
  },
  {
    "name": "Platform.Cmd",
    "comment": "\\n\\n> **Note:** Elm has **managed effects**, meaning that things like HTTP\\n> requests or writing to disk are all treated as *data* in Elm. When this\\n> data is given to the Elm runtime system, it can do some “query optimization”\\n> before actually performing the effect. Perhaps unexpectedly, this managed\\n> effects idea is the heart of why Elm is so nice for testing, reuse,\\n> reproducibility, etc.\\n>\\n> Elm has two kinds of managed effects: commands and subscriptions.\\n\\n# Commands\\n@docs Cmd, none, batch\\n\\n# Fancy Stuff\\n@docs map\\n\\n",
    "unions": [
      {
        "name": "Cmd",
        "comment": " A command is a way of telling Elm, “Hey, I want you to do this thing!”\\nSo if you want to send an HTTP request, you would need to command Elm to do it.\\nOr if you wanted to ask for geolocation, you would need to command Elm to go\\nget it.\\n\\nEvery `Cmd` specifies (1) which effects you need access to and (2) the type of\\nmessages that will come back into your application.\\n\\n**Note:** Do not worry if this seems confusing at first! As with every Elm user\\never, commands will make more sense as you work through [the Elm Architecture\\nTutorial](https://guide.elm-lang.org/architecture/) and see how they\\nfit into a real application!\\n",
        "args": [
          "msg"
        ],
        "cases": []
      }
    ],
    "aliases": [],
    "values": [
      {
        "name": "batch",
        "comment": " When you need the runtime system to perform a couple commands, you\\ncan batch them together. Each is handed to the runtime at the same time,\\nand since each can perform arbitrary operations in the world, there are\\nno ordering guarantees about the results.\\n\\n**Note:** `Cmd.none` and `Cmd.batch [ Cmd.none, Cmd.none ]` and `Cmd.batch []`\\nall do the same thing.\\n",
        "type": "List.List (Platform.Cmd.Cmd msg) -> Platform.Cmd.Cmd msg"
      },
      {
        "name": "map",
        "comment": " Transform the messages produced by a command.\\nVery similar to [`Html.map`](/packages/elm/html/latest/Html#map).\\n\\nThis is very rarely useful in well-structured Elm code, so definitely read the\\nsection on [structure][] in the guide before reaching for this!\\n\\n[structure]: https://guide.elm-lang.org/webapps/structure.html\\n",
        "type": "(a -> msg) -> Platform.Cmd.Cmd a -> Platform.Cmd.Cmd msg"
      },
      {
        "name": "none",
        "comment": " Tell the runtime that there are no commands.\\n\\n",
        "type": "Platform.Cmd.Cmd msg"
      }
    ],
    "binops": []
  },
  {
    "name": "Platform.Sub",
    "comment": "\\n\\n> **Note:** Elm has **managed effects**, meaning that things like HTTP\\n> requests or writing to disk are all treated as *data* in Elm. When this\\n> data is given to the Elm runtime system, it can do some “query optimization”\\n> before actually performing the effect. Perhaps unexpectedly, this managed\\n> effects idea is the heart of why Elm is so nice for testing, reuse,\\n> reproducibility, etc.\\n>\\n> Elm has two kinds of managed effects: commands and subscriptions.\\n\\n# Subscriptions\\n@docs Sub, none, batch\\n\\n# Fancy Stuff\\n@docs map\\n",
    "unions": [
      {
        "name": "Sub",
        "comment": " A subscription is a way of telling Elm, “Hey, let me know if anything\\ninteresting happens over there!” So if you want to listen for messages on a web\\nsocket, you would tell Elm to create a subscription. If you want to get clock\\nticks, you would tell Elm to subscribe to that. The cool thing here is that\\nthis means *Elm* manages all the details of subscriptions instead of *you*.\\nSo if a web socket goes down, *you* do not need to manually reconnect with an\\nexponential backoff strategy, *Elm* does this all for you behind the scenes!\\n\\nEvery `Sub` specifies (1) which effects you need access to and (2) the type of\\nmessages that will come back into your application.\\n\\n**Note:** Do not worry if this seems confusing at first! As with every Elm user\\never, subscriptions will make more sense as you work through [the Elm Architecture\\nTutorial](https://guide.elm-lang.org/architecture/) and see how they fit\\ninto a real application!\\n",
        "args": [
          "msg"
        ],
        "cases": []
      }
    ],
    "aliases": [],
    "values": [
      {
        "name": "batch",
        "comment": " When you need to subscribe to multiple things, you can create a `batch` of\\nsubscriptions.\\n\\n**Note:** `Sub.none` and `Sub.batch [ Sub.none, Sub.none ]` and\\n`Sub.batch []` all do the same thing.\\n",
        "type": "List.List (Platform.Sub.Sub msg) -> Platform.Sub.Sub msg"
      },
      {
        "name": "map",
        "comment": " Transform the messages produced by a subscription.\\nVery similar to [`Html.map`](/packages/elm/html/latest/Html#map).\\n\\nThis is very rarely useful in well-structured Elm code, so definitely read the\\nsection on [structure][] in the guide before reaching for this!\\n\\n[structure]: https://guide.elm-lang.org/webapps/structure.html\\n",
        "type": "(a -> msg) -> Platform.Sub.Sub a -> Platform.Sub.Sub msg"
      },
      {
        "name": "none",
        "comment": " Tell the runtime that there are no subscriptions.\\n",
        "type": "Platform.Sub.Sub msg"
      }
    ],
    "binops": []
  },
  {
    "name": "Process",
    "comment": "\\n\\n# Processes\\n@docs Id, spawn, sleep, kill\\n\\n## Future Plans\\n\\nRight now, this library is pretty sparse. For example, there is no public API\\nfor processes to communicate with each other. This is a really important\\nability, but it is also something that is extraordinarily easy to get wrong!\\n\\nI think the trend will be towards an Erlang style of concurrency, where every\\nprocess has an “event queue” that anyone can send messages to. I currently\\nthink the API will be extended to be more like this:\\n\\n    type Id exit msg\\n\\n    spawn : Task exit a -> Task x (Id exit Never)\\n\\n    kill : Id exit msg -> Task x ()\\n\\n    send : Id exit msg -> msg -> Task x ()\\n\\nA process `Id` will have two type variables to make sure all communication is\\nvalid. The `exit` type describes the messages that are produced if the process\\nfails because of user code. So if processes are linked and trapping errors,\\nthey will need to handle this. The `msg` type just describes what kind of\\nmessages this process can be sent by strangers.\\n\\nWe shall see though! This is just a draft that does not cover nearly everything\\nit needs to, so the long-term vision for concurrency in Elm will be rolling out\\nslowly as I get more data and experience.\\n\\nI ask that people bullish on compiling to node.js keep this in mind. I think we\\ncan do better than the hopelessly bad concurrency model of node.js, and I hope\\nthe Elm community will be supportive of being more ambitious, even if it takes\\nlonger. That’s kind of what Elm is all about.\\n",
    "unions": [],
    "aliases": [
      {
        "name": "Id",
        "comment": " A light-weight process that runs concurrently. You can use `spawn` to\\nget a bunch of different tasks running in different processes. The Elm runtime\\nwill interleave their progress. So if a task is taking too long, we will pause\\nit at an `andThen` and switch over to other stuff.\\n\\n**Note:** We make a distinction between *concurrency* which means interleaving\\ndifferent sequences and *parallelism* which means running different\\nsequences at the exact same time. For example, a\\n[time-sharing system](https://en.wikipedia.org/wiki/Time-sharing) is definitely\\nconcurrent, but not necessarily parallel. So even though JS runs within a\\nsingle OS-level thread, Elm can still run things concurrently.\\n",
        "args": [],
        "type": "Platform.ProcessId"
      }
    ],
    "values": [
      {
        "name": "kill",
        "comment": " Sometimes you `spawn` a process, but later decide it would be a waste to\\nhave it keep running and doing stuff. The `kill` function will force a process\\nto bail on whatever task it is running. So if there is an HTTP request in\\nflight, it will also abort the request.\\n",
        "type": "Process.Id -> Task.Task x ()"
      },
      {
        "name": "sleep",
        "comment": " Block progress on the current process for the given number of milliseconds.\\nThe JavaScript equivalent of this is [`setTimeout`][setTimeout] which lets you\\ndelay work until later.\\n\\n[setTimeout]: https://developer.mozilla.org/en-US/docs/Web/API/WindowTimers/setTimeout\\n",
        "type": "Basics.Float -> Task.Task x ()"
      },
      {
        "name": "spawn",
        "comment": " Run a task in its own light-weight process. In the following example,\\n`task1` and `task2` will be interleaved. If `task1` makes a long HTTP request\\nor is just taking a long time, we can hop over to `task2` and do some work\\nthere.\\n\\n    spawn task1\\n      |> Task.andThen (\\_ -> spawn task2)\\n\\n**Note:** This creates a relatively restricted kind of `Process` because it\\ncannot receive any messages. More flexibility for user-defined processes will\\ncome in a later release!\\n",
        "type": "Task.Task x a -> Task.Task y Process.Id"
      }
    ],
    "binops": []
  },
  {
    "name": "Result",
    "comment": " A `Result` is the result of a computation that may fail. This is a great\\nway to manage errors in Elm.\\n\\n# Type and Constructors\\n@docs Result\\n\\n# Mapping\\n@docs map, map2, map3, map4, map5\\n\\n# Chaining\\n@docs andThen\\n\\n# Handling Errors\\n@docs withDefault, toMaybe, fromMaybe, mapError\\n",
    "unions": [
      {
        "name": "Result",
        "comment": " A `Result` is either `Ok` meaning the computation succeeded, or it is an\\n`Err` meaning that there was some failure.\\n",
        "args": [
          "error",
          "value"
        ],
        "cases": [
          [
            "Ok",
            [
              "value"
            ]
          ],
          [
            "Err",
            [
              "error"
            ]
          ]
        ]
      }
    ],
    "aliases": [],
    "values": [
      {
        "name": "andThen",
        "comment": " Chain together a sequence of computations that may fail. It is helpful\\nto see its definition:\\n\\n    andThen : (a -> Result e b) -> Result e a -> Result e b\\n    andThen callback result =\\n        case result of\\n          Ok value -> callback value\\n          Err msg -> Err msg\\n\\nThis means we only continue with the callback if things are going well. For\\nexample, say you need to use (`toInt : String -> Result String Int`) to parse\\na month and make sure it is between 1 and 12:\\n\\n    toValidMonth : Int -> Result String Int\\n    toValidMonth month =\\n        if month >= 1 && month <= 12\\n            then Ok month\\n            else Err "months must be between 1 and 12"\\n\\n    toMonth : String -> Result String Int\\n    toMonth rawString =\\n        toInt rawString\\n          |> andThen toValidMonth\\n\\n    -- toMonth "4" == Ok 4\\n    -- toMonth "9" == Ok 9\\n    -- toMonth "a" == Err "cannot parse to an Int"\\n    -- toMonth "0" == Err "months must be between 1 and 12"\\n\\nThis allows us to come out of a chain of operations with quite a specific error\\nmessage. It is often best to create a custom type that explicitly represents\\nthe exact ways your computation may fail. This way it is easy to handle in your\\ncode.\\n",
        "type": "(a -> Result.Result x b) -> Result.Result x a -> Result.Result x b"
      },
      {
        "name": "fromMaybe",
        "comment": " Convert from a simple `Maybe` to interact with some code that primarily\\nuses `Results`.\\n\\n    parseInt : String -> Maybe Int\\n\\n    resultParseInt : String -> Result String Int\\n    resultParseInt string =\\n        fromMaybe ("error parsing string: " ++ toString string) (parseInt string)\\n",
        "type": "x -> Maybe.Maybe a -> Result.Result x a"
      },
      {
        "name": "map",
        "comment": " Apply a function to a result. If the result is `Ok`, it will be converted.\\nIf the result is an `Err`, the same error value will propagate through.\\n\\n    map sqrt (Ok 4.0)          == Ok 2.0\\n    map sqrt (Err "bad input") == Err "bad input"\\n",
        "type": "(a -> value) -> Result.Result x a -> Result.Result x value"
      },
      {
        "name": "map2",
        "comment": " Apply a function if both results are `Ok`. If not, the first `Err` will\\npropagate through.\\n\\n    map2 max (Ok 42)   (Ok 13)   == Ok 42\\n    map2 max (Err "x") (Ok 13)   == Err "x"\\n    map2 max (Ok 42)   (Err "y") == Err "y"\\n    map2 max (Err "x") (Err "y") == Err "x"\\n\\nThis can be useful if you have two computations that may fail, and you want\\nto put them together quickly.\\n",
        "type": "(a -> b -> value) -> Result.Result x a -> Result.Result x b -> Result.Result x value"
      },
      {
        "name": "map3",
        "comment": "",
        "type": "(a -> b -> c -> value) -> Result.Result x a -> Result.Result x b -> Result.Result x c -> Result.Result x value"
      },
      {
        "name": "map4",
        "comment": "",
        "type": "(a -> b -> c -> d -> value) -> Result.Result x a -> Result.Result x b -> Result.Result x c -> Result.Result x d -> Result.Result x value"
      },
      {
        "name": "map5",
        "comment": "",
        "type": "(a -> b -> c -> d -> e -> value) -> Result.Result x a -> Result.Result x b -> Result.Result x c -> Result.Result x d -> Result.Result x e -> Result.Result x value"
      },
      {
        "name": "mapError",
        "comment": " Transform an `Err` value. For example, say the errors we get have too much\\ninformation:\\n\\n    parseInt : String -> Result ParseError Int\\n\\n    type alias ParseError =\\n        { message : String\\n        , code : Int\\n        , position : (Int,Int)\\n        }\\n\\n    mapError .message (parseInt "123") == Ok 123\\n    mapError .message (parseInt "abc") == Err "char 'a' is not a number"\\n",
        "type": "(x -> y) -> Result.Result x a -> Result.Result y a"
      },
      {
        "name": "toMaybe",
        "comment": " Convert to a simpler `Maybe` if the actual error message is not needed or\\nyou need to interact with some code that primarily uses maybes.\\n\\n    parseInt : String -> Result ParseError Int\\n\\n    maybeParseInt : String -> Maybe Int\\n    maybeParseInt string =\\n        toMaybe (parseInt string)\\n",
        "type": "Result.Result x a -> Maybe.Maybe a"
      },
      {
        "name": "withDefault",
        "comment": " If the result is `Ok` return the value, but if the result is an `Err` then\\nreturn a given default value. The following examples try to parse integers.\\n\\n    Result.withDefault 0 (Ok 123)   == 123\\n    Result.withDefault 0 (Err "no") == 0\\n",
        "type": "a -> Result.Result x a -> a"
      }
    ],
    "binops": []
  },
  {
    "name": "Set",
    "comment": " A set of unique values. The values can be any comparable type. This\\nincludes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or lists\\nof comparable types.\\n\\nInsert, remove, and query operations all take *O(log n)* time.\\n\\n# Sets\\n@docs Set\\n\\n# Build\\n@docs empty, singleton, insert, remove\\n\\n# Query\\n@docs isEmpty, member, size\\n\\n# Combine\\n@docs union, intersect, diff\\n\\n# Lists\\n@docs toList, fromList\\n\\n# Transform\\n@docs map, foldl, foldr, filter, partition\\n\\n",
    "unions": [
      {
        "name": "Set",
        "comment": " Represents a set of unique values. So `(Set Int)` is a set of integers and\\n`(Set String)` is a set of strings.\\n",
        "args": [
          "t"
        ],
        "cases": []
      }
    ],
    "aliases": [],
    "values": [
      {
        "name": "diff",
        "comment": " Get the difference between the first set and the second. Keeps values\\nthat do not appear in the second set.\\n",
        "type": "Set.Set comparable -> Set.Set comparable -> Set.Set comparable"
      },
      {
        "name": "empty",
        "comment": " Create an empty set.\\n",
        "type": "Set.Set a"
      },
      {
        "name": "filter",
        "comment": " Only keep elements that pass the given test.\\n\\n    import Set exposing (Set)\\n\\n    numbers : Set Int\\n    numbers =\\n      Set.fromList [-2,-1,0,1,2]\\n\\n    positives : Set Int\\n    positives =\\n      Set.filter (\\x -> x > 0) numbers\\n\\n    -- positives == Set.fromList [1,2]\\n",
        "type": "(comparable -> Basics.Bool) -> Set.Set comparable -> Set.Set comparable"
      },
      {
        "name": "foldl",
        "comment": " Fold over the values in a set, in order from lowest to highest.\\n",
        "type": "(a -> b -> b) -> b -> Set.Set a -> b"
      },
      {
        "name": "foldr",
        "comment": " Fold over the values in a set, in order from highest to lowest.\\n",
        "type": "(a -> b -> b) -> b -> Set.Set a -> b"
      },
      {
        "name": "fromList",
        "comment": " Convert a list into a set, removing any duplicates.\\n",
        "type": "List.List comparable -> Set.Set comparable"
      },
      {
        "name": "insert",
        "comment": " Insert a value into a set.\\n",
        "type": "comparable -> Set.Set comparable -> Set.Set comparable"
      },
      {
        "name": "intersect",
        "comment": " Get the intersection of two sets. Keeps values that appear in both sets.\\n",
        "type": "Set.Set comparable -> Set.Set comparable -> Set.Set comparable"
      },
      {
        "name": "isEmpty",
        "comment": " Determine if a set is empty.\\n",
        "type": "Set.Set a -> Basics.Bool"
      },
      {
        "name": "map",
        "comment": " Map a function onto a set, creating a new set with no duplicates.\\n",
        "type": "(comparable -> comparable2) -> Set.Set comparable -> Set.Set comparable2"
      },
      {
        "name": "member",
        "comment": " Determine if a value is in a set.\\n",
        "type": "comparable -> Set.Set comparable -> Basics.Bool"
      },
      {
        "name": "partition",
        "comment": " Create two new sets. The first contains all the elements that passed the\\ngiven test, and the second contains all the elements that did not.\\n",
        "type": "(comparable -> Basics.Bool) -> Set.Set comparable -> ( Set.Set comparable, Set.Set comparable )"
      },
      {
        "name": "remove",
        "comment": " Remove a value from a set. If the value is not found, no changes are made.\\n",
        "type": "comparable -> Set.Set comparable -> Set.Set comparable"
      },
      {
        "name": "singleton",
        "comment": " Create a set with one value.\\n",
        "type": "comparable -> Set.Set comparable"
      },
      {
        "name": "size",
        "comment": " Determine the number of elements in a set.\\n",
        "type": "Set.Set a -> Basics.Int"
      },
      {
        "name": "toList",
        "comment": " Convert a set into a list, sorted from lowest to highest.\\n",
        "type": "Set.Set a -> List.List a"
      },
      {
        "name": "union",
        "comment": " Get the union of two sets. Keep all values.\\n",
        "type": "Set.Set comparable -> Set.Set comparable -> Set.Set comparable"
      }
    ],
    "binops": []
  },
  {
    "name": "String",
    "comment": " A built-in representation for efficient string manipulation. String literals\\nare enclosed in `"double quotes"`. Strings are *not* lists of characters.\\n\\n# Strings\\n@docs String, isEmpty, length, reverse, repeat, replace\\n\\n# Building and Splitting\\n@docs append, concat, split, join, words, lines\\n\\n# Get Substrings\\n@docs slice, left, right, dropLeft, dropRight\\n\\n# Check for Substrings\\n@docs contains, startsWith, endsWith, indexes, indices\\n\\n# Int Conversions\\n@docs toInt, fromInt\\n\\n# Float Conversions\\n@docs toFloat, fromFloat\\n\\n# Char Conversions\\n@docs fromChar, cons, uncons\\n\\n# List Conversions\\n@docs toList, fromList\\n\\n# Formatting\\nCosmetic operations such as padding with extra characters or trimming whitespace.\\n\\n@docs toUpper, toLower, pad, padLeft, padRight, trim, trimLeft, trimRight\\n\\n# Higher-Order Functions\\n@docs map, filter, foldl, foldr, any, all\\n",
    "unions": [
      {
        "name": "String",
        "comment": " A `String` is a chunk of text:\\n\\n    "Hello!"\\n    "How are you?"\\n    "🙈🙉🙊"\\n\\n    -- strings with escape characters\\n    "this\\
\\t\\"that\\""\\n    "\\u{1F648}\\u{1F649}\\u{1F64A}" -- "🙈🙉🙊"\\n\\n    -- multiline strings\\n    \"\"\"Triple double quotes let you\\n    create "multiline strings" which\\n    can have unescaped quotes and newlines.\\n    \"\"\"\\n\\nA `String` can represent any sequence of [unicode characters][u]. You can use\\nthe unicode escapes from `\\u{0000}` to `\\u{10FFFF}` to represent characters\\nby their code point. You can also include the unicode characters directly.\\nUsing the escapes can be better if you need one of the many whitespace\\ncharacters with different widths.\\n\\n[u]: https://en.wikipedia.org/wiki/Unicode\\n\\n**Note:** JavaScript lets you use double quotes and single quotes interchangably.\\nThis is not true in Elm. You must use double quotes for a `String`, and you must\\nuse single quotes for a [`Char`](Char#Char).\\n",
        "args": [],
        "cases": []
      }
    ],
    "aliases": [],
    "values": [
      {
        "name": "all",
        "comment": " Determine whether *all* characters pass the test.\\n\\n    all isDigit "90210" == True\\n    all isDigit "R2-D2" == False\\n    all isDigit "heart" == False\\n",
        "type": "(Char.Char -> Basics.Bool) -> String.String -> Basics.Bool"
      },
      {
        "name": "any",
        "comment": " Determine whether *any* characters pass the test.\\n\\n    any isDigit "90210" == True\\n    any isDigit "R2-D2" == True\\n    any isDigit "heart" == False\\n",
        "type": "(Char.Char -> Basics.Bool) -> String.String -> Basics.Bool"
      },
      {
        "name": "append",
        "comment": " Append two strings. You can also use [the `(++)` operator](Basics#++)\\nto do this.\\n\\n    append "butter" "fly" == "butterfly"\\n",
        "type": "String.String -> String.String -> String.String"
      },
      {
        "name": "concat",
        "comment": " Concatenate many strings into one.\\n\\n    concat ["never","the","less"] == "nevertheless"\\n",
        "type": "List.List String.String -> String.String"
      },
      {
        "name": "cons",
        "comment": " Add a character to the beginning of a string.\\n\\n    cons 'T' "he truth is out there" == "The truth is out there"\\n",
        "type": "Char.Char -> String.String -> String.String"
      },
      {
        "name": "contains",
        "comment": " See if the second string contains the first one.\\n\\n    contains "the" "theory" == True\\n    contains "hat" "theory" == False\\n    contains "THE" "theory" == False\\n\\n",
        "type": "String.String -> String.String -> Basics.Bool"
      },
      {
        "name": "dropLeft",
        "comment": " Drop *n* characters from the left side of a string.\\n\\n    dropLeft 2 "The Lone Gunmen" == "e Lone Gunmen"\\n",
        "type": "Basics.Int -> String.String -> String.String"
      },
      {
        "name": "dropRight",
        "comment": " Drop *n* characters from the right side of a string.\\n\\n    dropRight 2 "Cigarette Smoking Man" == "Cigarette Smoking M"\\n",
        "type": "Basics.Int -> String.String -> String.String"
      },
      {
        "name": "endsWith",
        "comment": " See if the second string ends with the first one.\\n\\n    endsWith "the" "theory" == False\\n    endsWith "ory" "theory" == True\\n",
        "type": "String.String -> String.String -> Basics.Bool"
      },
      {
        "name": "filter",
        "comment": " Keep only the characters that pass the test.\\n\\n    filter isDigit "R2-D2" == "22"\\n",
        "type": "(Char.Char -> Basics.Bool) -> String.String -> String.String"
      },
      {
        "name": "foldl",
        "comment": " Reduce a string from the left.\\n\\n    foldl cons "" "time" == "emit"\\n",
        "type": "(Char.Char -> b -> b) -> b -> String.String -> b"
      },
      {
        "name": "foldr",
        "comment": " Reduce a string from the right.\\n\\n    foldr cons "" "time" == "time"\\n",
        "type": "(Char.Char -> b -> b) -> b -> String.String -> b"
      },
      {
        "name": "fromChar",
        "comment": " Create a string from a given character.\\n\\n    fromChar 'a' == "a"\\n",
        "type": "Char.Char -> String.String"
      },
      {
        "name": "fromFloat",
        "comment": " Convert a `Float` to a `String`.\\n\\n    String.fromFloat 123 == "123"\\n    String.fromFloat -42 == "-42"\\n    String.fromFloat 3.9 == "3.9"\\n\\nCheck out [`Debug.toString`](Debug#toString) to convert *any* value to a string\\nfor debugging purposes.\\n",
        "type": "Basics.Float -> String.String"
      },
      {
        "name": "fromInt",
        "comment": " Convert an `Int` to a `String`.\\n\\n    String.fromInt 123 == "123"\\n    String.fromInt -42 == "-42"\\n\\nCheck out [`Debug.toString`](Debug#toString) to convert *any* value to a string\\nfor debugging purposes.\\n",
        "type": "Basics.Int -> String.String"
      },
      {
        "name": "fromList",
        "comment": " Convert a list of characters into a String. Can be useful if you\\nwant to create a string primarily by consing, perhaps for decoding\\nsomething.\\n\\n    fromList ['a','b','c'] == "abc"\\n    fromList ['🙈','🙉','🙊'] == "🙈🙉🙊"\\n",
        "type": "List.List Char.Char -> String.String"
      },
      {
        "name": "indexes",
        "comment": " Get all of the indexes for a substring in another string.\\n\\n    indexes "i" "Mississippi"   == [1,4,7,10]\\n    indexes "ss" "Mississippi"  == [2,5]\\n    indexes "needle" "haystack" == []\\n",
        "type": "String.String -> String.String -> List.List Basics.Int"
      },
      {
        "name": "indices",
        "comment": " Alias for `indexes`. ",
        "type": "String.String -> String.String -> List.List Basics.Int"
      },
      {
        "name": "isEmpty",
        "comment": " Determine if a string is empty.\\n\\n    isEmpty "" == True\\n    isEmpty "the world" == False\\n",
        "type": "String.String -> Basics.Bool"
      },
      {
        "name": "join",
        "comment": " Put many strings together with a given separator.\\n\\n    join "a" ["H","w","ii","n"]        == "Hawaiian"\\n    join " " ["cat","dog","cow"]       == "cat dog cow"\\n    join "/" ["home","evan","Desktop"] == "home/evan/Desktop"\\n",
        "type": "String.String -> List.List String.String -> String.String"
      },
      {
        "name": "left",
        "comment": " Take *n* characters from the left side of a string.\\n\\n    left 2 "Mulder" == "Mu"\\n",
        "type": "Basics.Int -> String.String -> String.String"
      },
      {
        "name": "length",
        "comment": " Get the length of a string.\\n\\n    length "innumerable" == 11\\n    length "" == 0\\n\\n",
        "type": "String.String -> Basics.Int"
      },
      {
        "name": "lines",
        "comment": " Break a string into lines, splitting on newlines.\\n\\n    lines "How are you?\\
Good?" == ["How are you?", "Good?"]\\n",
        "type": "String.String -> List.List String.String"
      },
      {
        "name": "map",
        "comment": " Transform every character in a string\\n\\n    map (\\c -> if c == '/' then '.' else c) "a/b/c" == "a.b.c"\\n",
        "type": "(Char.Char -> Char.Char) -> String.String -> String.String"
      },
      {
        "name": "pad",
        "comment": " Pad a string on both sides until it has a given length.\\n\\n    pad 5 ' ' "1"   == "  1  "\\n    pad 5 ' ' "11"  == "  11 "\\n    pad 5 ' ' "121" == " 121 "\\n",
        "type": "Basics.Int -> Char.Char -> String.String -> String.String"
      },
      {
        "name": "padLeft",
        "comment": " Pad a string on the left until it has a given length.\\n\\n    padLeft 5 '.' "1"   == "....1"\\n    padLeft 5 '.' "11"  == "...11"\\n    padLeft 5 '.' "121" == "..121"\\n",
        "type": "Basics.Int -> Char.Char -> String.String -> String.String"
      },
      {
        "name": "padRight",
        "comment": " Pad a string on the right until it has a given length.\\n\\n    padRight 5 '.' "1"   == "1...."\\n    padRight 5 '.' "11"  == "11..."\\n    padRight 5 '.' "121" == "121.."\\n",
        "type": "Basics.Int -> Char.Char -> String.String -> String.String"
      },
      {
        "name": "repeat",
        "comment": " Repeat a string *n* times.\\n\\n    repeat 3 "ha" == "hahaha"\\n",
        "type": "Basics.Int -> String.String -> String.String"
      },
      {
        "name": "replace",
        "comment": " Replace all occurrences of some substring.\\n\\n    replace "." "-" "Json.Decode.succeed" == "Json-Decode-succeed"\\n    replace "," "/" "a,b,c,d,e"           == "a/b/c/d/e"\\n\\n**Note:** If you need more advanced replacements, check out the\\n[`elm/parser`][parser] or [`elm/regex`][regex] package.\\n\\n[parser]: /packages/elm/parser/latest\\n[regex]: /packages/elm/regex/latest\\n",
        "type": "String.String -> String.String -> String.String -> String.String"
      },
      {
        "name": "reverse",
        "comment": " Reverse a string.\\n\\n    reverse "stressed" == "desserts"\\n",
        "type": "String.String -> String.String"
      },
      {
        "name": "right",
        "comment": " Take *n* characters from the right side of a string.\\n\\n    right 2 "Scully" == "ly"\\n",
        "type": "Basics.Int -> String.String -> String.String"
      },
      {
        "name": "slice",
        "comment": " Take a substring given a start and end index. Negative indexes\\nare taken starting from the *end* of the list.\\n\\n    slice  7  9 "snakes on a plane!" == "on"\\n    slice  0  6 "snakes on a plane!" == "snakes"\\n    slice  0 -7 "snakes on a plane!" == "snakes on a"\\n    slice -6 -1 "snakes on a plane!" == "plane"\\n",
        "type": "Basics.Int -> Basics.Int -> String.String -> String.String"
      },
      {
        "name": "split",
        "comment": " Split a string using a given separator.\\n\\n    split "," "cat,dog,cow"        == ["cat","dog","cow"]\\n    split "/" "home/evan/Desktop/" == ["home","evan","Desktop", ""]\\n\\n",
        "type": "String.String -> String.String -> List.List String.String"
      },
      {
        "name": "startsWith",
        "comment": " See if the second string starts with the first one.\\n\\n    startsWith "the" "theory" == True\\n    startsWith "ory" "theory" == False\\n",
        "type": "String.String -> String.String -> Basics.Bool"
      },
      {
        "name": "toFloat",
        "comment": " Try to convert a string into a float, failing on improperly formatted strings.\\n\\n    String.toFloat "123" == Just 123.0\\n    String.toFloat "-42" == Just -42.0\\n    String.toFloat "3.1" == Just 3.1\\n    String.toFloat "31a" == Nothing\\n\\nIf you are extracting a number from some raw user input, you will typically\\nwant to use [`Maybe.withDefault`](Maybe#withDefault) to handle bad data:\\n\\n    Maybe.withDefault 0 (String.toFloat "42.5") == 42.5\\n    Maybe.withDefault 0 (String.toFloat "cats") == 0\\n",
        "type": "String.String -> Maybe.Maybe Basics.Float"
      },
      {
        "name": "toInt",
        "comment": " Try to convert a string into an int, failing on improperly formatted strings.\\n\\n    String.toInt "123" == Just 123\\n    String.toInt "-42" == Just -42\\n    String.toInt "3.1" == Nothing\\n    String.toInt "31a" == Nothing\\n\\nIf you are extracting a number from some raw user input, you will typically\\nwant to use [`Maybe.withDefault`](Maybe#withDefault) to handle bad data:\\n\\n    Maybe.withDefault 0 (String.toInt "42") == 42\\n    Maybe.withDefault 0 (String.toInt "ab") == 0\\n",
        "type": "String.String -> Maybe.Maybe Basics.Int"
      },
      {
        "name": "toList",
        "comment": " Convert a string to a list of characters.\\n\\n    toList "abc" == ['a','b','c']\\n    toList "🙈🙉🙊" == ['🙈','🙉','🙊']\\n",
        "type": "String.String -> List.List Char.Char"
      },
      {
        "name": "toLower",
        "comment": " Convert a string to all lower case. Useful for case-insensitive comparisons.\\n\\n    toLower "X-FILES" == "x-files"\\n",
        "type": "String.String -> String.String"
      },
      {
        "name": "toUpper",
        "comment": " Convert a string to all upper case. Useful for case-insensitive comparisons\\nand VIRTUAL YELLING.\\n\\n    toUpper "skinner" == "SKINNER"\\n",
        "type": "String.String -> String.String"
      },
      {
        "name": "trim",
        "comment": " Get rid of whitespace on both sides of a string.\\n\\n    trim "  hats  \\
" == "hats"\\n",
        "type": "String.String -> String.String"
      },
      {
        "name": "trimLeft",
        "comment": " Get rid of whitespace on the left of a string.\\n\\n    trimLeft "  hats  \\
" == "hats  \\
"\\n",
        "type": "String.String -> String.String"
      },
      {
        "name": "trimRight",
        "comment": " Get rid of whitespace on the right of a string.\\n\\n    trimRight "  hats  \\
" == "  hats"\\n",
        "type": "String.String -> String.String"
      },
      {
        "name": "uncons",
        "comment": " Split a non-empty string into its head and tail. This lets you\\npattern match on strings exactly as you would with lists.\\n\\n    uncons "abc" == Just ('a',"bc")\\n    uncons ""    == Nothing\\n",
        "type": "String.String -> Maybe.Maybe ( Char.Char, String.String )"
      },
      {
        "name": "words",
        "comment": " Break a string into words, splitting on chunks of whitespace.\\n\\n    words "How are \\t you? \\
 Good?" == ["How","are","you?","Good?"]\\n",
        "type": "String.String -> List.List String.String"
      }
    ],
    "binops": []
  },
  {
    "name": "Task",
    "comment": " Tasks make it easy to describe asynchronous operations that may fail, like\\nHTTP requests or writing to a database.\\n\\n# Tasks\\n@docs Task, perform, attempt\\n\\n# Chains\\n@docs andThen, succeed, fail, sequence\\n\\n# Maps\\n@docs map, map2, map3, map4, map5\\n\\n# Errors\\n@docs onError, mapError\\n\\n",
    "unions": [],
    "aliases": [
      {
        "name": "Task",
        "comment": " Here are some common tasks:\\n\\n- [`now : Task x Posix`][now]\\n- [`focus : String -> Task Error ()`][focus]\\n- [`sleep : Float -> Task x ()`][sleep]\\n\\n[now]: /packages/elm/time/latest/Time#now\\n[focus]: /packages/elm/browser/latest/Browser-Dom#focus\\n[sleep]: /packages/elm/core/latest/Process#sleep\\n\\nIn each case we have a `Task` that will resolve successfully with an `a` value\\nor unsuccessfully with an `x` value. So `Browser.Dom.focus` we may fail with an\\n`Error` if the given ID does not exist. Whereas `Time.now` never fails so\\nI cannot be more specific than `x`. No such value will ever exist! Instead it\\nalways succeeds with the current POSIX time.\\n\\nMore generally a task is a _description_ of what you need to do. Like a todo\\nlist. Or like a grocery list. Or like GitHub issues. So saying "the task is\\nto tell me the current POSIX time" does not complete the task! You need\\n[`perform`](#perform) tasks or [`attempt`](#attempt) tasks.\\n",
        "args": [
          "x",
          "a"
        ],
        "type": "Platform.Task x a"
      }
    ],
    "values": [
      {
        "name": "andThen",
        "comment": " Chain together a task and a callback. The first task will run, and if it is\\nsuccessful, you give the result to the callback resulting in another task. This\\ntask then gets run. We could use this to make a task that resolves an hour from\\nnow:\\n\\n    import Time -- elm install elm/time\\n    import Process\\n\\n    timeInOneHour : Task x Time.Posix\\n    timeInOneHour =\\n      Process.sleep (60 * 60 * 1000)\\n        |> andThen (\\_ -> Time.now)\\n\\nFirst the process sleeps for an hour **and then** it tells us what time it is.\\n",
        "type": "(a -> Task.Task x b) -> Task.Task x a -> Task.Task x b"
      },
      {
        "name": "attempt",
        "comment": " This is very similar to [`perform`](#perform) except it can handle failures!\\nSo we could _attempt_ to focus on a certain DOM node like this:\\n\\n    import Browser.Dom  -- elm install elm/browser\\n    import Task\\n\\n    type Msg\\n      = Click\\n      | Search String\\n      | Focus (Result Browser.DomError ())\\n\\n    focus : Cmd Msg\\n    focus =\\n      Task.attempt Focus (Browser.Dom.focus "my-app-search-box")\\n\\nSo the task is "focus on this DOM node" and we are turning it into the command\\n"Hey Elm, attempt to focus on this DOM node and give me a `Msg` about whether\\nyou succeeded or failed."\\n\\n**Note:** Definitely work through [`guide.elm-lang.org`][guide] to get a\\nfeeling for how commands fit into The Elm Architecture.\\n\\n[guide]: https://guide.elm-lang.org/\\n",
        "type": "(Result.Result x a -> msg) -> Task.Task x a -> Platform.Cmd.Cmd msg"
      },
      {
        "name": "fail",
        "comment": " A task that fails immediately when run. Like with `succeed`, this can be\\nused with `andThen` to check on the outcome of another task.\\n\\n    type Error = NotFound\\n\\n    notFound : Task Error a\\n    notFound =\\n      fail NotFound\\n",
        "type": "x -> Task.Task x a"
      },
      {
        "name": "map",
        "comment": " Transform a task. Maybe you want to use [`elm/time`][time] to figure\\nout what time it will be in one hour:\\n\\n    import Task exposing (Task)\\n    import Time -- elm install elm/time\\n\\n    timeInOneHour : Task x Time.Posix\\n    timeInOneHour =\\n      Task.map addAnHour Time.now\\n\\n    addAnHour : Time.Posix -> Time.Posix\\n    addAnHour time =\\n      Time.millisToPosix (Time.posixToMillis time + 60 * 60 * 1000)\\n\\n[time]: /packages/elm/time/latest/\\n",
        "type": "(a -> b) -> Task.Task x a -> Task.Task x b"
      },
      {
        "name": "map2",
        "comment": " Put the results of two tasks together. For example, if we wanted to know\\nthe current month, we could use [`elm/time`][time] to ask:\\n\\n    import Task exposing (Task)\\n    import Time -- elm install elm/time\\n\\n    getMonth : Task x Int\\n    getMonth =\\n      Task.map2 Time.toMonth Time.here Time.now\\n\\n**Note:** Say we were doing HTTP requests instead. `map2` does each task in\\norder, so it would try the first request and only continue after it succeeds.\\nIf it fails, the whole thing fails!\\n\\n[time]: /packages/elm/time/latest/\\n",
        "type": "(a -> b -> result) -> Task.Task x a -> Task.Task x b -> Task.Task x result"
      },
      {
        "name": "map3",
        "comment": "",
        "type": "(a -> b -> c -> result) -> Task.Task x a -> Task.Task x b -> Task.Task x c -> Task.Task x result"
      },
      {
        "name": "map4",
        "comment": "",
        "type": "(a -> b -> c -> d -> result) -> Task.Task x a -> Task.Task x b -> Task.Task x c -> Task.Task x d -> Task.Task x result"
      },
      {
        "name": "map5",
        "comment": "",
        "type": "(a -> b -> c -> d -> e -> result) -> Task.Task x a -> Task.Task x b -> Task.Task x c -> Task.Task x d -> Task.Task x e -> Task.Task x result"
      },
      {
        "name": "mapError",
        "comment": " Transform the error value. This can be useful if you need a bunch of error\\ntypes to match up.\\n\\n    type Error\\n      = Http Http.Error\\n      | WebGL WebGL.Error\\n\\n    getResources : Task Error Resource\\n    getResources =\\n      sequence\\n        [ mapError Http serverTask\\n        , mapError WebGL textureTask\\n        ]\\n",
        "type": "(x -> y) -> Task.Task x a -> Task.Task y a"
      },
      {
        "name": "onError",
        "comment": " Recover from a failure in a task. If the given task fails, we use the\\ncallback to recover.\\n\\n    fail "file not found"\\n      |> onError (\\msg -> succeed 42)\\n      -- succeed 42\\n\\n    succeed 9\\n      |> onError (\\msg -> succeed 42)\\n      -- succeed 9\\n",
        "type": "(x -> Task.Task y a) -> Task.Task x a -> Task.Task y a"
      },
      {
        "name": "perform",
        "comment": " Like I was saying in the [`Task`](#Task) documentation, just having a\\n`Task` does not mean it is done. We must command Elm to `perform` the task:\\n\\n    import Time  -- elm install elm/time\\n    import Task\\n\\n    type Msg\\n      = Click\\n      | Search String\\n      | NewTime Time.Posix\\n\\n    getNewTime : Cmd Msg\\n    getNewTime =\\n      Task.perform NewTime Time.now\\n\\nIf you have worked through [`guide.elm-lang.org`][guide] (highly recommended!)\\nyou will recognize `Cmd` from the section on The Elm Architecture. So we have\\nchanged a task like "make delicious lasagna" into a command like "Hey Elm, make\\ndelicious lasagna and give it to my `update` function as a `Msg` value."\\n\\n[guide]: https://guide.elm-lang.org/\\n",
        "type": "(a -> msg) -> Task.Task Basics.Never a -> Platform.Cmd.Cmd msg"
      },
      {
        "name": "sequence",
        "comment": " Start with a list of tasks, and turn them into a single task that returns a\\nlist. The tasks will be run in order one-by-one and if any task fails the whole\\nsequence fails.\\n\\n    sequence [ succeed 1, succeed 2 ] == succeed [ 1, 2 ]\\n\\n",
        "type": "List.List (Task.Task x a) -> Task.Task x (List.List a)"
      },
      {
        "name": "succeed",
        "comment": " A task that succeeds immediately when run. It is usually used with\\n[`andThen`](#andThen). You can use it like `map` if you want:\\n\\n    import Time -- elm install elm/time\\n\\n    timeInMillis : Task x Int\\n    timeInMillis =\\n      Time.now\\n        |> andThen (\\t -> succeed (Time.posixToMillis t))\\n\\n",
        "type": "a -> Task.Task x a"
      }
    ],
    "binops": []
  },
  {
    "name": "Tuple",
    "comment": " Elm has built-in syntax for tuples, so you can define 2D points like this:\\n\\n    origin : (Float, Float)\\n    origin =\\n      (0, 0)\\n\\n    position : (Float, Float)\\n    position =\\n      (3, 4)\\n\\nThis module is a bunch of helpers for working with 2-tuples.\\n\\n**Note 1:** For more complex data, it is best to switch to records. So instead\\nof representing a 3D point as `(3,4,5)` and not having any helper functions,\\nrepresent it as `{ x = 3, y = 4, z = 5 }` and use all the built-in record\\nsyntax!\\n\\n**Note 2:** If your record contains a bunch of `Bool` and `Maybe` values,\\nyou may want to upgrade to union types. Check out [Joël’s post][ut] for more\\ninfo on this. (Picking appropriate data structures is super important in Elm!)\\n\\n[ut]: https://robots.thoughtbot.com/modeling-with-union-types\\n\\n# Create\\n@docs pair\\n\\n# Access\\n@docs first, second\\n\\n# Map\\n@docs mapFirst, mapSecond, mapBoth\\n\\n",
    "unions": [],
    "aliases": [],
    "values": [
      {
        "name": "first",
        "comment": " Extract the first value from a tuple.\\n\\n    first (3, 4) == 3\\n    first ("john", "doe") == "john"\\n",
        "type": "( a, b ) -> a"
      },
      {
        "name": "mapBoth",
        "comment": " Transform both parts of a tuple.\\n\\n    import String\\n\\n    mapBoth String.reverse sqrt  ("stressed", 16) == ("desserts", 4)\\n    mapBoth String.length negate ("stressed", 16) == (8, -16)\\n",
        "type": "(a -> x) -> (b -> y) -> ( a, b ) -> ( x, y )"
      },
      {
        "name": "mapFirst",
        "comment": " Transform the first value in a tuple.\\n\\n    import String\\n\\n    mapFirst String.reverse ("stressed", 16) == ("desserts", 16)\\n    mapFirst String.length  ("stressed", 16) == (8, 16)\\n",
        "type": "(a -> x) -> ( a, b ) -> ( x, b )"
      },
      {
        "name": "mapSecond",
        "comment": " Transform the second value in a tuple.\\n\\n    mapSecond sqrt   ("stressed", 16) == ("stressed", 4)\\n    mapSecond negate ("stressed", 16) == ("stressed", -16)\\n",
        "type": "(b -> y) -> ( a, b ) -> ( a, y )"
      },
      {
        "name": "pair",
        "comment": " Create a 2-tuple.\\n\\n    -- pair 3 4 == (3, 4)\\n\\n    zip : List a -> List b -> List (a, b)\\n    zip xs ys =\\n      List.map2 Tuple.pair xs ys\\n",
        "type": "a -> b -> ( a, b )"
      },
      {
        "name": "second",
        "comment": " Extract the second value from a tuple.\\n\\n    second (3, 4) == 4\\n    second ("john", "doe") == "doe"\\n",
        "type": "( a, b ) -> b"
      }
    ],
    "binops": []
  }
]
"""
