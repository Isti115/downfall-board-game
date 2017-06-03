module Main exposing (..)

import Dict exposing (Dict)
import Downfall.Game
import Downfall.Model
import Downfall.Types
import Downfall.Update
import Html exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    Downfall.Model.Model


init : ( Model, Cmd Msg )
init =
    Downfall.Game.init ! []



-- UPDATE


type alias Msg =
    Downfall.Types.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    Downfall.Update.update msg model ! []



-- VIEW


view : Model -> Html Msg
view model =
    pre []
        (Dict.toList model.containers
            |> List.map (\( k, c ) -> p [] [ text <| toString c ])
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
