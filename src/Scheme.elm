module Scheme exposing
    ( Scheme(..)
    , apply
    , freeTypeVars
    )

import Canonical.Type exposing (Type)
import Dict exposing (Dict)
import Set exposing (Set)
import Type


type Scheme
    = ForAll (List String) Type


freeTypeVars : Scheme -> Set String
freeTypeVars (ForAll subst tipe) =
    Set.diff
        (Canonical.Type.freeTypeVars tipe)
        (Set.fromList subst)


apply : Dict String Type -> Scheme -> Scheme
apply subst (ForAll vars tipe) =
    let
        freeSubst =
            List.foldl Dict.remove subst vars
    in
    ForAll vars <|
        Canonical.Type.apply freeSubst tipe
