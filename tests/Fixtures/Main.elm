module Fixtures.Main exposing
    ( exposedConstructors
    , exposedTypes
    , exposedValues
    , src
    , values
    )

import Set exposing (Set)


exposedTypes : Set String
exposedTypes =
    Set.fromList [ "Model" ]


exposedConstructors : Set String
exposedConstructors =
    Set.fromList [ "Model" ]


exposedValues : Set String
exposedValues =
    Set.fromList []


values : Set String
values =
    Set.fromList [ "update" ]


src : String
src =
    """
module Main exposing (Model)


type alias User =
    { name : String
    , age : Int
    }


type alias Model =
    { users : List User
    }


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model
"""
