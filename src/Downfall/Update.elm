module Downfall.Update exposing (Msg(Rotate), update)

import Downfall.Model
    exposing
        ( Angle
        , Container(Input, Output, Wheel)
        , ContainerStore
        , Identifier
        , Model
        , getConnections
        , getContainer
        , getIdentifier
        , rotateWheel
        , setContainer
        )
import Utilities


type Msg
    = Rotate Identifier Angle


updateContainers : Identifier -> List Identifier -> ContainerStore -> ContainerStore
updateContainers identifier connectedContainerIndentifiers containers =
    containers


performRotation : Identifier -> Angle -> ContainerStore -> ContainerStore
performRotation identifier angle containers =
    let
        currentContainer : Container
        currentContainer =
            getContainer identifier containers
    in
    case currentContainer of
        Input containerRecord ->
            Debug.crash "Cannot rotate an input."

        Wheel containerRecord ->
            containers
                |> setContainer identifier (rotateWheel angle currentContainer)

        Output containerRecord ->
            Debug.crash "Cannot rotate an output."


update : Msg -> Model -> Model
update msg model =
    case msg of
        Rotate identifier angle ->
            let
                rotatedModel : Model
                rotatedModel =
                    { model
                        | containers =
                            performRotation
                                identifier
                                (Utilities.sign angle)
                                model.containers
                    }

                rotatedContainer : Container
                rotatedContainer =
                    getContainer identifier model.containers

                rotatedContainerIdentifier : Identifier
                rotatedContainerIdentifier =
                    getIdentifier rotatedContainer

                connectedContainerIndentifiers : List Identifier
                connectedContainerIndentifiers =
                    getConnections rotatedContainer
                        |> List.map
                            (\c ->
                                if c.from /= rotatedContainerIdentifier then
                                    c.from
                                else
                                    c.to
                            )

                updatedModel =
                    { model
                        | containers =
                            updateContainers
                                identifier
                                connectedContainerIndentifiers
                                model.containers
                    }
            in
            if angle > 0 then
                update (Rotate identifier (angle - 1)) rotatedModel
            else if angle < 0 then
                update (Rotate identifier (angle + 1)) rotatedModel
            else
                model
