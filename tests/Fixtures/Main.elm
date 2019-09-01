module Fixtures.Main exposing
    ( exposedAliases
    , exposedConstructors
    , exposedTypes
    , exposedValues
    , src
    , values
    )

import Set exposing (Set)


exposedTypes : Set String
exposedTypes =
    Set.fromList []


exposedAliases : Set String
exposedAliases =
    Set.empty


exposedConstructors : Set String
exposedConstructors =
    Set.empty


exposedValues : Set String
exposedValues =
    Set.fromList []


values : Set String
values =
    Set.fromList [ "update" ]


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
