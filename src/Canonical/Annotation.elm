module Canonical.Annotation exposing
    ( Annotation(..)
    , apply
    , freeTypeVars
    , fromType
    , toString
    )

import Canonical.Type exposing (Type)
import Dict exposing (Dict)
import Set exposing (Set)


type Annotation
    = ForAll (List String) Type


freeTypeVars : Annotation -> Set String
freeTypeVars (ForAll subst tipe) =
    Set.diff
        (Canonical.Type.freeTypeVars tipe)
        (Set.fromList subst)


apply : Dict String Type -> Annotation -> Annotation
apply subst (ForAll vars tipe) =
    let
        freeSubst =
            List.foldl Dict.remove subst vars
    in
    ForAll vars <|
        Canonical.Type.apply freeSubst tipe


fromType : Type -> Annotation
fromType tipe =
    ForAll
        (Set.toList (Canonical.Type.freeTypeVars tipe))
        tipe


toString : Annotation -> String
toString (ForAll _ tipe) =
    Canonical.Type.toString tipe
