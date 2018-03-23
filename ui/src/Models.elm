module Models exposing (..)

type alias Model = 
    { route : Route 
    }

initialModel : Route -> Model
initialModel route = 
    { route = route
    }

type Route
    = HomeRoute
    | OtherRoute
    | NotFoundRoute