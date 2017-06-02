module Main exposing (..)

import Dict exposing (Dict)
import Downfall.Model
    exposing
        ( Angle
        , Container
        , Identifier
        , Model
        , connect
        , insertContainer
        , makeInput
        , makeOutput
        , makeSlot
        , makeWheel
        )
import Downfall.Update
    exposing
        ( Msg(Rotate)
        , update
        )
import Html exposing (..)


-- import Html.Attributes exposing (..)
-- import Html.Events exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL
-- type alias Model =
--     Downfall.Model.Model


init : ( Model, Cmd Msg )
init =
    { containers =
        [ makeInput "A" (List.repeat 5 "Green")
        , makeWheel "1" 0 (List.map makeSlot [ 45, 135, 225, 315 ])
        , makeWheel "2" 0 (List.map makeSlot [ 0, 120, 240 ])
        , makeWheel "3" 0 (List.map makeSlot [ 90, 270 ])
        , makeOutput "X" []
        ]
            |> List.foldl insertContainer Dict.empty
            |> connect "A" "1" 115
            |> connect "1" "2" 230
            |> connect "2" "3" 180
            |> connect "3" "X" 160
    }
        ! []
        |> Tuple.first
        |> update (Rotate "1" 0)
        |> Tuple.first
        |> update (Rotate "2" 70)
        |> Tuple.first
        |> update (Rotate "2" -50)



-- UPDATE
-- type alias Msg =
--     Downfall.Update.Msg
--
-- update : Msg -> Model -> ( Model, Cmd Msg )
-- update msg model =
--     Downfall.Update.update msg model ! []
--
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
