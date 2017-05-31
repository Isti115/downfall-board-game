module Downfall.Update exposing (Msg(Rotate), update)

import Downfall.Model
    exposing
        ( Angle
        , Container(Input, Output, Wheel)
        , Identifier
        , Model
        , getConnections
        , getIdentifier
        , makeWheel
        , rotateWheel
        )
import Utilities


type Msg
    = Rotate Identifier Angle


getContainer : Identifier -> List Container -> Maybe Container
getContainer identifier containers =
    case containers of
        ((Input containerRecord) as currentContainer) :: rest ->
            if containerRecord.identifier == identifier then
                Just currentContainer
            else
                getContainer identifier rest

        ((Wheel containerRecord) as currentContainer) :: rest ->
            if containerRecord.identifier == identifier then
                Just currentContainer
            else
                getContainer identifier rest

        ((Output containerRecord) as currentContainer) :: rest ->
            if containerRecord.identifier == identifier then
                Just currentContainer
            else
                getContainer identifier rest

        [] ->
            Nothing


setContainer : Identifier -> Container -> List Container -> List Container
setContainer identifier newContainer containers =
    case containers of
        ((Input containerRecord) as currentContainer) :: rest ->
            if containerRecord.identifier == identifier then
                newContainer :: rest
            else
                currentContainer :: setContainer identifier newContainer rest

        ((Wheel containerRecord) as currentContainer) :: rest ->
            if containerRecord.identifier == identifier then
                newContainer :: rest
            else
                currentContainer :: setContainer identifier newContainer rest

        ((Output containerRecord) as currentContainer) :: rest ->
            if containerRecord.identifier == identifier then
                newContainer :: rest
            else
                currentContainer :: setContainer identifier newContainer rest

        [] ->
            []


updateContainers : Identifier -> List Container -> List Container
updateContainers identifier containers =
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
                        |> Maybe.withDefault (makeWheel "0" 0 [])

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
                                model.containers
                    }
            in
            if angle > 0 then
                update (Rotate identifier (angle - 1)) rotatedModel
            else if angle < 0 then
                update (Rotate identifier (angle + 1)) rotatedModel
            else
                model
