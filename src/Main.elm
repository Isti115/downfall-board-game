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


type alias StatusRecord =
    { ownData : List Float
    , ownCount : ( Int, Int )
    , otherCount : ( Int, Int )
    , prevTwoRotated : List Downfall.Types.Identifier
    }


port statusOutput : ( StatusRecord, StatusRecord ) -> Cmd msg



-- INIT


type alias Model =
    { downfall1 : Downfall.Model
    , downfall2 : Downfall.Model
    , prevTwoRotated : List Downfall.Types.Identifier
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
    , prevTwoRotated = [ "0", "0" ]
    }
        ! [ Cmd.map Downfall1Msg downfall1Cmd
          , Cmd.map Downfall2Msg downfall2Cmd
          ]



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.map Downfall1Msg (Downfall.view model.downfall1)
        , Html.hr [] []
        , Html.map Downfall2Msg (Downfall.view model.downfall2)
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

                downfall1Status =
                    Downfall.Game.getStatus downfall1

                downfall2Status =
                    Downfall.Game.getStatus downfall2

                newPrevTwoRotated =
                    case model.prevTwoRotated of
                        [ a, b ] ->
                            [ rotationRecord.identifier, a ]

                        _ ->
                            Debug.crash "prevTwoRotated: Should always have a length of two."
            in
            { model
                | downfall1 = downfall1
                , downfall2 = downfall2
                , prevTwoRotated = newPrevTwoRotated
            }
                ! [ Cmd.map Downfall1Msg downfall1Cmd
                  , Cmd.map Downfall2Msg downfall2Cmd
                  , ( { ownData =
                            downfall1Status.neuralData
                                ++ [ (\( a, b ) -> toFloat a / toFloat b)
                                        downfall2Status.outputCount
                                   ]
                      , ownCount = downfall1Status.outputCount
                      , otherCount = downfall2Status.outputCount
                      , prevTwoRotated = newPrevTwoRotated
                      }
                    , { ownData =
                            downfall2Status.neuralData
                                ++ [ (\( a, b ) -> toFloat a / toFloat b)
                                        downfall1Status.outputCount
                                   ]
                      , ownCount = downfall2Status.outputCount
                      , otherCount = downfall1Status.outputCount
                      , prevTwoRotated = newPrevTwoRotated
                      }
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
