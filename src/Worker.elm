port module Main exposing (..)

import Config
import Downfall.Game
import Downfall.Main as Downfall
import Downfall.Types


main : Program Never Model Msg
main =
    Platform.program
        { init = init
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
    { downfall1 : Downfall.Model
    , downfall2 : Downfall.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( downfall1, downfall1Cmd ) =
            Downfall.init Config.currentGameDescriptor

        ( downfall2, downfall2Cmd ) =
            Downfall.init Config.currentGameDescriptor
    in
    { downfall1 = downfall1
    , downfall2 = downfall2
    }
        ! [ Cmd.map Downfall1Msg downfall1Cmd
          , Cmd.map Downfall2Msg downfall2Cmd
          ]



-- UPDATE


type Msg
    = Downfall1Msg Downfall.Msg
    | Downfall2Msg Downfall.Msg
    | IncomingRotation RotationRecord


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Downfall1Msg downfall1Msg ->
            let
                ( downfall1, downfall1Cmd ) =
                    Downfall.update downfall1Msg model.downfall1
            in
            { model | downfall1 = downfall1 } ! [ Cmd.map Downfall1Msg downfall1Cmd ]

        Downfall2Msg downfall2Msg ->
            let
                ( downfall2, downfall2Cmd ) =
                    Downfall.update downfall2Msg model.downfall2
            in
            { model | downfall2 = downfall2 } ! [ Cmd.map Downfall2Msg downfall2Cmd ]

        IncomingRotation rotationRecord ->
            let
                ( downfall1, downfall1Cmd ) =
                    Downfall.update
                        (Downfall.Types.Rotate rotationRecord.identifier rotationRecord.angle)
                        model.downfall1

                ( downfall2, downfall2Cmd ) =
                    Downfall.update
                        (Downfall.Types.Rotate rotationRecord.identifier rotationRecord.angle)
                        model.downfall2
            in
            { model
                | downfall1 = downfall1
                , downfall2 = downfall2
            }
                ! [ Cmd.map Downfall1Msg downfall1Cmd
                  , Cmd.map Downfall2Msg downfall2Cmd
                  , ( Downfall.Game.getStatus downfall1
                    , Downfall.Game.getStatus downfall2
                    )
                        |> statusOutput
                  ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Downfall1Msg (Downfall.subscriptions model.downfall1)
        , Sub.map Downfall2Msg (Downfall.subscriptions model.downfall2)
        , rotationInput IncomingRotation
        ]
