module Src exposing (qualifiedName)


qualifiedName : List String -> String -> String
qualifiedName moduleName name =
    String.join "." (moduleName ++ [ name ])
