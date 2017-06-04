port module Main exposing (..)

import Config
import Downfall.Game
import Downfall.Main as Downfall
import Downfall.Types
import Html


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias RotationRecord =
    { identifier : String
    , angle : Int
    }


port rotationInput : (RotationRecord -> msg) -> Sub msg


port statusOutput : ( Downfall.Types.Status, Downfall.Types.Status ) -> Cmd msg



-- INIT


type alias Model =
    { downfall : Downfall.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( downfall, downfallCmd ) =
            Downfall.init Config.currentGameDescriptor
    in
    { downfall = downfall
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
    | IncomingRotation RotationRecord


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DownfallMsg downfallMsg ->
            let
                ( downfall, downfallCmd ) =
                    Downfall.update downfallMsg model.downfall
            in
            { model | downfall = downfall } ! [ Cmd.map DownfallMsg downfallCmd ]

        IncomingRotation rotationRecord ->
            let
                ( downfall, downfallCmd ) =
                    Downfall.update
                        (Downfall.Types.Rotate rotationRecord.identifier rotationRecord.angle)
                        model.downfall
            in
            { model | downfall = downfall }
                ! [ Cmd.map DownfallMsg downfallCmd
                  , ( Downfall.Game.getStatus downfall
                    , Downfall.Game.getStatus downfall
                    )
                        |> statusOutput
                  ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map DownfallMsg (Downfall.subscriptions model.downfall)
        , rotationInput IncomingRotation
        ]
