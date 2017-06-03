module Downfall.Connection
    exposing
        ( addConnection
        , connect
        , getConnections
        )

import Downfall.ContainerStore exposing (getContainer, setContainer)
import Downfall.Types
    exposing
        ( Angle
        , Connection
        , Container(Input, Output, Wheel)
        , ContainerStore
        , Direction(In, Out)
        , Identifier
        )


addConnection : Connection -> Container -> Container
addConnection connection currentContainer =
    case currentContainer of
        Input containerRecord ->
            Input { containerRecord | connections = connection :: containerRecord.connections }

        Wheel containerRecord ->
            Wheel { containerRecord | connections = connection :: containerRecord.connections }

        Output containerRecord ->
            Output { containerRecord | connections = connection :: containerRecord.connections }


connect : Identifier -> Identifier -> Angle -> ContainerStore -> ContainerStore
connect fromId toId fromAngle containers =
    let
        fromConnection : Connection
        fromConnection =
            { direction = Out
            , target = toId
            , localAngle = fromAngle
            , remoteAngle = (fromAngle + 180) % 360
            }

        toConnection : Connection
        toConnection =
            { direction = In
            , target = fromId
            , localAngle = (fromAngle + 180) % 360
            , remoteAngle = fromAngle
            }

        fromContainer : Container
        fromContainer =
            getContainer fromId containers

        toContainer : Container
        toContainer =
            getContainer toId containers
    in
    containers
        |> setContainer (addConnection fromConnection fromContainer)
        |> setContainer (addConnection toConnection toContainer)


getConnections : Container -> List Connection
getConnections currentContainer =
    case currentContainer of
        Input { connections } ->
            connections

        Wheel { connections } ->
            connections

        Output { connections } ->
            connections
