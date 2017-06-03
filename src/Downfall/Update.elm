module Downfall.Update exposing (Msg(Rotate), update)

import Downfall.Model
    exposing
        ( Angle
        , Connection
        , Container(Input, Output, Wheel)
        , ContainerStore
        , Direction(In, Out)
        , Identifier
        , Model
        , Slot
        , SlotState(Empty, Occupied)
        , getConnections
        , getContainer
        , getIdentifier
        , getSlotAt
        , pullFromInput
        , pushToOutput
        , rotateContainer
        , setContainer
        , setSlotAt
        )
import Utilities


type Msg
    = Rotate Identifier Angle


updateContainers : Connection -> ( Container, Container ) -> ( Container, Container )
updateContainers connection ( rotatedContainer, connectedContainer ) =
    case rotatedContainer of
        Input _ ->
            Debug.crash "updateContainers: Inputs should never be rotated."

        Wheel rotatedContainerRecord ->
            let
                maybeRotatedContainerSlot : Maybe Slot
                maybeRotatedContainerSlot =
                    getSlotAt connection.localAngle rotatedContainer

                rotatedCotnainerSlot =
                    case maybeRotatedContainerSlot of
                        Just slot ->
                            slot

                        Nothing ->
                            Debug.crash "updateContainers: Filtered connection must have a related slot."
            in
            case connectedContainer of
                Input containerRecord ->
                    let
                        -- updatedConnectedContainer : Container
                        -- maybeCurrentColor : Maybe Color
                        ( updatedConnectedContainer, maybeCurrentColor ) =
                            pullFromInput connectedContainer
                    in
                    case maybeCurrentColor of
                        Just currentColor ->
                            ( setSlotAt connection.localAngle (Occupied currentColor) rotatedContainer
                            , updatedConnectedContainer
                            )

                        Nothing ->
                            ( rotatedContainer, connectedContainer )

                Wheel containerRecord ->
                    let
                        maybeConnectedContainerSlot : Maybe Slot
                        maybeConnectedContainerSlot =
                            getSlotAt connection.remoteAngle connectedContainer
                    in
                    case maybeConnectedContainerSlot of
                        Just connectedContainerSlot ->
                            case ( connection.direction, rotatedCotnainerSlot.state, connectedContainerSlot.state ) of
                                ( In, Empty as local, (Occupied currentColor) as remote ) ->
                                    ( setSlotAt connection.localAngle remote rotatedContainer
                                    , setSlotAt connection.remoteAngle local connectedContainer
                                    )

                                ( Out, (Occupied currentColor) as local, Empty as remote ) ->
                                    ( setSlotAt connection.localAngle remote rotatedContainer
                                    , setSlotAt connection.remoteAngle local connectedContainer
                                    )

                                _ ->
                                    ( rotatedContainer, connectedContainer )

                        Nothing ->
                            ( rotatedContainer, connectedContainer )

                Output containerRecord ->
                    case rotatedCotnainerSlot.state of
                        Occupied currentColor ->
                            ( setSlotAt connection.localAngle Empty rotatedContainer
                            , pushToOutput currentColor connectedContainer
                            )

                        Empty ->
                            ( rotatedContainer, connectedContainer )

        Output _ ->
            Debug.crash "updateContainers: Outputs should never be rotated."


processConnection : Connection -> ( Container, ContainerStore ) -> ( Container, ContainerStore )
processConnection connection ( currentRotatedContainer, containers ) =
    let
        -- updatedCurrentRotatedContainer : Container
        -- updatedConnectedContainer : Container
        ( updatedCurrentRotatedContainer, updatedConnectedContainer ) =
            updateContainers
                connection
                ( currentRotatedContainer
                , getContainer
                    connection.target
                    containers
                )
    in
    ( updatedCurrentRotatedContainer
    , containers |> setContainer updatedConnectedContainer
    )


processConnections : Container -> List Connection -> ContainerStore -> ContainerStore
processConnections rotatedContainer filteredConnections containers =
    let
        -- updatedRotatedContainer : Container
        -- updatedContainers : ContainerStore
        ( updatedRotatedContainer, updatedContainers ) =
            filteredConnections
                |> List.foldl
                    processConnection
                    ( rotatedContainer, containers )
    in
    updatedContainers |> setContainer updatedRotatedContainer


connectionFilter : Container -> Connection -> Bool
connectionFilter rotatedContainer connection =
    case rotatedContainer of
        Input _ ->
            Debug.crash "connectionFilter: Inputs do not have slots."

        Wheel containerRecord ->
            containerRecord.slots
                |> List.any
                    (\slot ->
                        (containerRecord.angle + slot.angle)
                            % 360
                            == connection.localAngle
                            && (if connection.direction == In then
                                    slot.state == Empty
                                else if connection.direction == Out then
                                    slot.state /= Empty
                                else
                                    False
                               )
                    )

        Output _ ->
            Debug.crash "connectionFilter: Outputs do not have slots."


getUpdatedModel : Identifier -> Angle -> Model -> Model
getUpdatedModel identifier angle model =
    let
        currentContainer : Container
        currentContainer =
            getContainer identifier model.containers

        rotatedContainer : Container
        rotatedContainer =
            rotateContainer (Utilities.sign angle) currentContainer

        filteredConnections : List Connection
        filteredConnections =
            getConnections rotatedContainer
                |> List.filter (connectionFilter rotatedContainer)

        updatedContainers : ContainerStore
        updatedContainers =
            model.containers
                |> setContainer rotatedContainer
                |> processConnections rotatedContainer filteredConnections

        updatedModel : Model
        updatedModel =
            { model | containers = updatedContainers }
    in
    updatedModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rotate identifier angle ->
            if angle > 0 then
                update (Rotate identifier (angle - 1)) (getUpdatedModel identifier angle model)
            else if angle < 0 then
                update (Rotate identifier (angle + 1)) (getUpdatedModel identifier angle model)
            else
                model ! []
