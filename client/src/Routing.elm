module Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)
import Models exposing (Route(..))

matchers : Parser (Route -> a) a
matchers = 
    oneOf
        [ map HomeRoute top
        , map OtherRoute (s "other")
        ]

parseLocation : Location -> Route
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route
        
        Nothing ->
            NotFoundRoute
            