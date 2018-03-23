module View exposing (..)
import Msgs exposing (Msg)
import Models exposing (Model)
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)

view : Model -> Html Msg
view model =
    div []
        [ page model ]

page : Model -> Html Msg
page model =
    case model.route of
        Models.HomeRoute ->
            div []
                [ img [ src "/logo.svg" ] []
                , h1 [] [ text "Your Elm App is working!" ]
                ]
        Models.OtherRoute ->
            div []
                [ img [ src "/logo.svg" ] []
                , h1 [] [ text "Other route!" ]
                ]
        Models.NotFoundRoute ->
            h1 [] [ text "404 - Page not found" ]
