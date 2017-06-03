module Main exposing (..)

import Downfall.Main as Downfall
import Html


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { downfall : Downfall.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( downfallInit, downfallCmd ) =
            Downfall.init
    in
    { downfall = downfallInit
    }
        ! [ Cmd.map DownfallMsg downfallCmd ]



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.map DownfallMsg (Downfall.view model.downfall)
        ]



-- UPDATE


type Msg
    = DownfallMsg Downfall.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DownfallMsg downfallMsg ->
            let
                ( downfall, downfallCmd ) =
                    Downfall.update downfallMsg model.downfall
            in
            { model | downfall = downfall } ! [ Cmd.map DownfallMsg downfallCmd ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map DownfallMsg (Downfall.subscriptions model.downfall)
        ]
