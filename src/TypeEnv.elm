module TypeEnv exposing
    ( TypeEnv
    , apply
    , diff
    , emptyEnv
    , extend
    , freeTypeVars
    , fromValues
    , join
    , lookup
    , toValues
    )

import Dict exposing (Dict)
import Elm.Type exposing (Type)
import Scheme exposing (Scheme(..))
import Set exposing (Set)
import Type


type TypeEnv
    = TypeEnv (Dict String Scheme)


extend : String -> Scheme -> TypeEnv -> TypeEnv
extend var scheme (TypeEnv schemes) =
    TypeEnv <|
        Dict.insert var scheme schemes


join : TypeEnv -> TypeEnv -> TypeEnv
join (TypeEnv schemesA) (TypeEnv schemesB) =
    TypeEnv (Dict.union schemesA schemesB)


emptyEnv : TypeEnv
emptyEnv =
    TypeEnv Dict.empty


diff : TypeEnv -> TypeEnv -> TypeEnv
diff (TypeEnv schemesA) (TypeEnv schemesB) =
    TypeEnv <|
        Dict.diff schemesA schemesB


lookup : String -> TypeEnv -> Maybe Scheme
lookup name (TypeEnv schemes) =
    Dict.get name schemes


apply : Dict String Type -> TypeEnv -> TypeEnv
apply subst (TypeEnv schemes) =
    TypeEnv (Dict.map (\_ -> Scheme.apply subst) schemes)


freeTypeVars : TypeEnv -> Set String
freeTypeVars (TypeEnv schemes) =
    Dict.foldl (\_ -> Scheme.freeTypeVars >> Set.union) Set.empty schemes


toValues : TypeEnv -> List ( String, Type )
toValues (TypeEnv schemes) =
    schemes
        |> Dict.map (\_ (ForAll _ tipe) -> tipe)
        |> Dict.toList


fromValues : List ( String, Type ) -> TypeEnv
fromValues values =
    values
        |> List.map
            (Tuple.mapSecond <|
                \tipe ->
                    ForAll
                        (Set.toList (Type.freeTypeVars tipe))
                        tipe
            )
        |> Dict.fromList
        |> TypeEnv
