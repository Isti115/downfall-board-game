module Downfall.Update exposing (Msg(Rotate), update)

import Downfall.Model
    exposing
        ( Angle
        , Container(Input, Output, Wheel)
        , Identifier
        , Model
        , rotateWheel
        )
import Utilities


type Msg
    = Rotate Identifier Angle



-- updateContainer angle currentContainer


updateContainers : Identifier -> Angle -> List Container -> List Container
updateContainers identifier angle containers =
    containers


performRotation : Identifier -> Angle -> List Container -> List Container
performRotation identifier angle containers =
    case containers of
        ((Input containerRecord) as currentContainer) :: rest ->
            currentContainer :: performRotation identifier angle rest

        ((Wheel containerRecord) as currentContainer) :: rest ->
            if containerRecord.identifier == identifier then
                rotateWheel angle currentContainer :: rest
            else
                currentContainer :: performRotation identifier angle rest

        ((Output containerRecord) as currentContainer) :: rest ->
            currentContainer :: performRotation identifier angle rest

        [] ->
            []


update : Msg -> Model -> Model
update msg model =
    case msg of
        Rotate identifier angle ->
            let
                newModel =
                    { model
                        | containers =
                            performRotation
                                identifier
                                (Utilities.sign angle)
                                model.containers
                    }
            in
            if angle > 0 then
                update (Rotate identifier (angle - 1)) newModel
            else if angle < 0 then
                update (Rotate identifier (angle + 1)) newModel
            else
                model
