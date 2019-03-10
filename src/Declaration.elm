module Declaration exposing
    ( Declaration(..)
    , parse
    )

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

import Elm.Type exposing (Type(..))
import Parser
    exposing
        ( (|.)
        , (|=)
        , DeadEnd
        , Parser
        , Step(..)
        , Trailing(..)
        , andThen
        , backtrackable
        , chompWhile
        , getChompedString
        , keyword
        , lazy
        , loop
        , map
        , oneOf
        , problem
        , sequence
        , succeed
        , symbol
        , variable
        )
import Set



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


type Declaration
    = Value String Type
    | TypeAlias String (List String) Type
    | CustomType String (List String) (List ( String, List Type ))


parse : String -> List Declaration
parse text =
    preProcess text
        |> List.filterMap
            (Parser.run declaration >> Result.toMaybe)


preProcess : String -> List String
preProcess text =
    String.lines text
        |> List.foldl
            (\line lines ->
                if String.trim line == "" then
                    lines

                else if String.startsWith " " line then
                    case lines of
                        previousLine :: rest ->
                            (previousLine ++ line) :: rest

                        [] ->
                            lines

                else
                    line :: lines
            )
            []


declaration : Parser Declaration
declaration =
    oneOf
        [ backtrackable value
        , backtrackable typeAlias
        , backtrackable customType
        ]


value : Parser Declaration
value =
    succeed (\( valueName, valueType ) -> Value valueName valueType)
        |= field


typeAlias : Parser Declaration
typeAlias =
    succeed TypeAlias
        |. keyword "type"
        |. spaces
        |. keyword "alias"
        |. spaces
        |= variable
            { start = Char.isUpper
            , inner = isInnerVarChar
            , reserved = Set.empty
            }
        |. spaces
        |= succeed []
        |. spaces
        |. symbol "="
        |. spaces
        |= tipe


customType : Parser Declaration
customType =
    succeed CustomType
        |. keyword "type"
        |. spaces
        |= variable
            { start = Char.isUpper
            , inner = isInnerVarChar
            , reserved = Set.empty
            }
        |. spaces
        |= typeVariables
        |. spaces
        |. symbol "="
        |. spaces
        |= constructors


typeVariables : Parser (List String)
typeVariables =
    loop [] typeVariablesHelp


typeVariablesHelp : List String -> Parser (Step (List String) (List String))
typeVariablesHelp revVariables =
    oneOf
        [ succeed (\variable -> Loop (variable :: revVariables))
            |= variable
                { start = Char.isLower
                , inner = isInnerVarChar
                , reserved = Set.empty
                }
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse revVariables))
        ]


type alias Constructor =
    ( String, List Type )


constructors : Parser (List Constructor)
constructors =
    succeed (::)
        |= constructor
        |. spaces
        |= loop [] constructorsHelp


constructorsHelp :
    List ( String, List Type )
    -> Parser (Step (List Constructor) (List Constructor))
constructorsHelp revConstructors =
    oneOf
        [ succeed (\constructor_ -> Loop (constructor_ :: revConstructors))
            |. symbol "|"
            |. spaces
            |= constructor
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse revConstructors))
        ]


constructor : Parser Constructor
constructor =
    succeed Tuple.pair
        |= variable
            { start = Char.isUpper
            , inner = isInnerVarChar
            , reserved = Set.empty
            }
        |. spaces
        |= typeParameters


typeParameters : Parser (List Type)
typeParameters =
    loop [] typeParametersHelp


typeParametersHelp : List Type -> Parser (Step (List Type) (List Type))
typeParametersHelp revTypes =
    oneOf
        [ succeed (\type_ -> Loop (type_ :: revTypes))
            |= tipe
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse revTypes))
        ]



-- Everything below is copied from https://github.com/elm/project-metadata-utils
-- and was originally licensed under the following license:
--
--   Copyright (c) 2017-present, Evan Czaplicki
--   All rights reserved.
--
--   Redistribution and use in source and binary forms, with or without
--   modification, are permitted provided that the following conditions are met:
--
--   * Redistributions of source code must retain the above copyright notice, this
--     list of conditions and the following disclaimer.
--
--   * Redistributions in binary form must reproduce the above copyright notice,
--     this list of conditions and the following disclaimer in the documentation
--     and/or other materials provided with the distribution.
--
--   * Neither the name of the {organization} nor the names of its
--     contributors may be used to endorse or promote products derived from
--     this software without specific prior written permission.
--
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
--   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
--   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
--   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
--   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
--   OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


tipe : Parser Type
tipe =
    lazy <|
        \_ ->
            andThen tipeHelp tipeTerm


tipeHelp : Type -> Parser Type
tipeHelp t =
    oneOf
        [ map (Lambda t) arrowAndType
        , succeed t
        ]


arrowAndType : Parser Type
arrowAndType =
    succeed identity
        |. backtrackable spaces
        |. arrow
        |. spaces
        |= tipe


arrow : Parser ()
arrow =
    symbol "->"


tipeTerm : Parser Type
tipeTerm =
    oneOf
        [ map Var lowVar
        , succeed Type
            |= qualifiedCapVar
            |= loop [] chompArgs
        , record
        , tuple
        ]


chompArgs : List Type -> Parser (Step (List Type) (List Type))
chompArgs revArgs =
    oneOf
        [ succeed identity
            |. backtrackable spaces
            |= term
            |> map (\arg -> Loop (arg :: revArgs))
        , map (\_ -> Done (List.reverse revArgs)) (succeed ())
        ]


term : Parser Type
term =
    oneOf
        [ map Var lowVar
        , map (\name -> Type name []) qualifiedCapVar
        , record
        , tuple
        ]



-- RECORDS


record : Parser Type
record =
    succeed (\ext fs -> Record fs ext)
        |. symbol "{"
        |. spaces
        |= extension
        |= recordEnd


extension : Parser (Maybe String)
extension =
    oneOf
        [ succeed Just
            |= backtrackable lowVar
            |. backtrackable spaces
            |. symbol "|"
            |. spaces
        , succeed Nothing
        ]


field : Parser ( String, Type )
field =
    succeed Tuple.pair
        |= lowVar
        |. spaces
        |. symbol ":"
        |. spaces
        |= tipe


type alias Fields =
    List ( String, Type )


recordEnd : Parser Fields
recordEnd =
    oneOf
        [ field
            |. spaces
            |> andThen (\f -> loop [ f ] recordEndHelp)
        , succeed []
            |. symbol "}"
        ]


recordEndHelp : Fields -> Parser (Step Fields Fields)
recordEndHelp revFields =
    oneOf
        [ succeed (\f -> Loop (f :: revFields))
            |. comma
            |. spaces
            |= field
            |. spaces
        , succeed (\_ -> Done (List.reverse revFields))
            |= symbol "}"
        ]



-- TUPLE


tuple : Parser Type
tuple =
    map tuplize <|
        sequence
            { start = "("
            , separator = ","
            , end = ")"
            , spaces = spaces
            , item = tipe
            , trailing = Forbidden
            }


tuplize : List Type -> Type
tuplize args =
    case args of
        [ arg ] ->
            arg

        _ ->
            Tuple args



-- VAR HELPERS


lowVar : Parser String
lowVar =
    var Char.isLower


capVar : Parser String
capVar =
    var Char.isUpper


isInnerVarChar : Char -> Bool
isInnerVarChar char =
    Char.isAlphaNum char || char == '_'


qualifiedCapVar : Parser String
qualifiedCapVar =
    getChompedString <|
        capVar
            |. loop () qualifiedCapVarHelp


qualifiedCapVarHelp : () -> Parser (Step () ())
qualifiedCapVarHelp _ =
    oneOf
        [ succeed (Loop ())
            |. symbol "."
            |. capVar
        , succeed (Done ())
        ]


var : (Char -> Bool) -> Parser String
var isFirst =
    variable
        { start = isFirst
        , inner = isInnerVarChar
        , reserved = Set.empty
        }



-- HELPERS


spaces : Parser ()
spaces =
    chompWhile (\char -> char == ' ')


comma : Parser ()
comma =
    symbol ","
