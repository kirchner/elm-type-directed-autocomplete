module Src exposing
    ( functionDeclarationAt
    , qualifiedName
    )

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)


qualifiedName : List String -> String -> String
qualifiedName moduleName name =
    String.join "." (moduleName ++ [ name ])


functionDeclarationAt : Range -> File -> Maybe Function
functionDeclarationAt holeRange file =
    functionDeclarationAtHelp holeRange file.declarations


functionDeclarationAtHelp : Range -> List (Node Declaration) -> Maybe Function
functionDeclarationAtHelp holeRange declarations =
    case declarations of
        [] ->
            Nothing

        (Node range (FunctionDeclaration function)) :: rest ->
            let
                containsHole =
                    ((range.start.row < holeRange.start.row)
                        || ((range.start.row == holeRange.start.row)
                                && (range.start.column <= holeRange.start.column)
                           )
                    )
                        && ((range.end.row > holeRange.end.row)
                                || ((range.end.row == holeRange.end.row)
                                        && (range.end.column >= holeRange.end.column)
                                   )
                           )
            in
            if containsHole then
                Just function

            else
                functionDeclarationAtHelp holeRange rest

        _ :: rest ->
            functionDeclarationAtHelp holeRange rest
