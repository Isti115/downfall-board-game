module Downfall.Main
    exposing
        ( Model
        , Msg
        , init
        , subscriptions
        , update
        , view
        )

import Dict
import Downfall.Game
import Downfall.Types
import Downfall.Update
import Html


-- INIT


type alias Model =
    Downfall.Types.Game


init : Downfall.Types.GameDescriptor -> ( Model, Cmd Msg )
init gameDescriptor =
    Downfall.Game.init gameDescriptor ! []



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.pre []
        (Dict.toList model.containers
            |> List.map (\( k, c ) -> Html.p [] [ Html.text <| toString c ])
        )



-- UPDATE


type alias Msg =
    Downfall.Types.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    Downfall.Update.update msg model ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
