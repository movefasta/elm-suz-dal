module Routing exposing (..)

import Navigation exposing (Location)
import Models exposing (..)
import UrlParser exposing (..)


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map LinkRoute (s "ipfs" </> string </> string)
        , map HashRoute (s "ipfs" </> string)
        ]


parseLocation : Location -> Route
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


objectPath : String
objectPath =
    "ipfs"


linkPath : Hash -> Name -> String
linkPath hash name =
    "ipfs" ++ hash ++ name
