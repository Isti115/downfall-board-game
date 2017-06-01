module Downfall.Model
    exposing
        ( Angle
        , Container(Input, Output, Wheel)
        , ContainerStore
        , Identifier
        , Model
        , connect
        , getConnections
        , getContainer
        , getIdentifier
        , insertContainer
        , makeInput
        , makeOutput
        , makeSlot
        , makeWheel
        , rotateWheel
        , setContainer
        )

import Dict exposing (Dict)


type alias ContainerStore =
    Dict String Container


type alias Model =
    { containers : ContainerStore
    }



--


type alias Identifier =
    String


type alias CircleCount =
    Int


type alias Angle =
    Float



--


type Container
    = Input InputRecord
    | Wheel WheelRecord
    | Output OutputRecord


type alias InputRecord =
    { identifier : Identifier
    , circleCount : CircleCount
    , connections : List Connection
    }


type alias WheelRecord =
    { identifier : Identifier
    , angle : Angle
    , slots : List Slot
    , connections : List Connection
    }


type alias OutputRecord =
    { identifier : Identifier
    , circleCount : CircleCount
    , connections : List Connection
    }


makeInput : Identifier -> CircleCount -> Container
makeInput identifier circleCount =
    Input
        { identifier = identifier
        , circleCount = circleCount
        , connections = []
        }


makeWheel : Identifier -> Angle -> List Slot -> Container
makeWheel identifier angle slots =
    Wheel
        { identifier = identifier
        , angle = angle
        , slots = slots
        , connections = []
        }


makeOutput : Identifier -> CircleCount -> Container
makeOutput identifier circleCount =
    Output
        { identifier = identifier
        , circleCount = circleCount
        , connections = []
        }


getIdentifier : Container -> Identifier
getIdentifier currentContainer =
    case currentContainer of
        Input { identifier } ->
            identifier

        Wheel { identifier } ->
            identifier

        Output { identifier } ->
            identifier


rotateWheel : Angle -> Container -> Container
rotateWheel angle currentContainer =
    case currentContainer of
        Input containerRecord ->
            Input containerRecord

        Wheel containerRecord ->
            Wheel { containerRecord | angle = containerRecord.angle + angle }

        Output containerRecord ->
            Output containerRecord



--


insertContainer : Container -> ContainerStore -> ContainerStore
insertContainer currentContainer containerStore =
    Dict.insert (getIdentifier currentContainer) currentContainer containerStore


getContainer : Identifier -> ContainerStore -> Container
getContainer identifier containers =
    let
        maybeCurrentContainer : Maybe Container
        maybeCurrentContainer =
            Dict.get identifier containers
    in
    case maybeCurrentContainer of
        Just currentContainer ->
            currentContainer

        Nothing ->
            Debug.crash
                ("getContainer: Container with identifier '"
                    ++ identifier
                    ++ "' does not exist."
                )


setContainer : Identifier -> Container -> ContainerStore -> ContainerStore
setContainer identifier newContainer containers =
    let
        maybeCurrentContainer : Maybe Container
        maybeCurrentContainer =
            Dict.get identifier containers
    in
    case maybeCurrentContainer of
        Just currentContainer ->
            Dict.insert identifier newContainer containers

        Nothing ->
            Debug.crash
                ("setContainer: Container with identifier '"
                    ++ identifier
                    ++ "' does not exist."
                )



--


type SlotState
    = Empty
    | Occupied


type alias Slot =
    { angle : Angle
    , state : SlotState
    }


makeSlot : Angle -> Slot
makeSlot angle =
    { angle = angle
    , state = Empty
    }


type alias Connection =
    { from : Identifier
    , to : Identifier
    , angle : Angle
    }


connectHelper :
    Connection
    -> { a | connections : List Connection }
    -> { a | connections : List Connection }
connectHelper connection containerRecord =
    { containerRecord | connections = connection :: containerRecord.connections }


addConnection : Connection -> Container -> Container
addConnection connection currentContainer =
    case currentContainer of
        Input containerRecord ->
            Input (connectHelper connection containerRecord)

        Wheel containerRecord ->
            Wheel (connectHelper connection containerRecord)

        Output containerRecord ->
            Output (connectHelper connection containerRecord)


connect : Identifier -> Identifier -> Angle -> ContainerStore -> ContainerStore
connect fromId toId angle containers =
    let
        currentConnection : Connection
        currentConnection =
            { from = fromId
            , to = toId
            , angle = angle
            }

        fromContainer : Container
        fromContainer =
            getContainer fromId containers

        toContainer : Container
        toContainer =
            getContainer toId containers
    in
    containers
        |> setContainer fromId (addConnection currentConnection fromContainer)
        |> setContainer toId (addConnection currentConnection toContainer)


getConnections : Container -> List Connection
getConnections currentContainer =
    case currentContainer of
        Input { connections } ->
            connections

        Wheel { connections } ->
            connections

        Output { connections } ->
            connections
