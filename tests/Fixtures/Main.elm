module Fixtures.Main exposing
    ( exposedUnions
    , exposedValues
    , src
    , values
    )

import Set exposing (Set)


exposedValues : Set String
exposedValues =
    Set.fromList []


values : Set String
values =
    Set.fromList <|
        [ "NoOp", "update" ]


exposedUnions : Set String
exposedUnions =
    Set.fromList []


src : String
src =
    """
module Main exposing ()


type alias Model =
    {}


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model
"""
