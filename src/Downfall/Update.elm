module Downfall.Update exposing (Msg, update)

import Downfall.Model
    exposing
        ( Angle
        , Container
        , Identifier
        , Model
        )


type Msg
    = Rotate Identifier Angle


updateContainers : Identifier -> Angle -> List Container -> List Container
updateContainers identifier angle containers =
    containers


update : Msg -> Model -> Model
update msg model =
    case msg of
        Rotate identifier angle ->
            if angle > 0 then
                let
                    newModel =
                        { model | containers = model.containers }
                in
                update (Rotate identifier (angle - 1)) newModel
            else if angle < 0 then
                update (Rotate identifier (angle + 1)) model
            else
                model
