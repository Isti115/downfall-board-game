module Downfall.Model
    exposing
        ( Angle
        , Connection
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
        , rotateContainer
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
    Int



--


type Container
    = Input InputRecord
    | Wheel WheelRecord
    | Output OutputRecord


type alias InputRecord =
    { identifier : Identifier
    , colors : List Color
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
    , colors : List Color
    , connections : List Connection
    }


makeInput : Identifier -> List Color -> Container
makeInput identifier colors =
    Input
        { identifier = identifier
        , colors = colors
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


makeOutput : Identifier -> List Color -> Container
makeOutput identifier colors =
    Output
        { identifier = identifier
        , colors = colors
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


rotateContainer : Angle -> Container -> Container
rotateContainer angle currentContainer =
    case currentContainer of
        Input containerRecord ->
            Debug.crash "rotateContainer: Cannot rotate an input."

        Wheel containerRecord ->
            Wheel { containerRecord | angle = (containerRecord.angle + angle) % 360 }

        Output containerRecord ->
            Debug.crash "rotateContainer: Cannot rotate an output."



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


setContainer : Container -> ContainerStore -> ContainerStore
setContainer newContainer containers =
    let
        identifier : Identifier
        identifier =
            getIdentifier newContainer

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


type alias Color =
    String


type SlotState
    = Empty
    | Occupied Color


type alias Slot =
    { angle : Angle
    , state : SlotState
    }


makeSlot : Angle -> Slot
makeSlot angle =
    { angle = angle
    , state = Empty
    }


getSlots : Container -> List Slot
getSlots currentContainer =
    case currentContainer of
        Input _ ->
            Debug.crash "getSlots: Inputs do not have slots."

        Wheel { slots } ->
            slots

        Output _ ->
            Debug.crash "getSlots: Outputs do not have slots."


getSlotAt : (Slot -> Angle) -> Angle -> Container -> Slot
getSlotAt direction angle currentContainer =
    let
        currentSlots : List Slot
        currentSlots =
            getSlots currentContainer

        foundSlot : Maybe Slot
        foundSlot =
            currentSlots
                |> List.filter (\slot -> direction slot == angle)
                |> List.head
    in
    case foundSlot of
        Just slot ->
            slot

        Nothing ->
            Debug.crash
                ("getSlotAt: Container "
                    ++ getIdentifier currentContainer
                    ++ " doesn't have a slot at "
                    ++ toString angle
                )



--


type alias Connection =
    { from : Identifier
    , to : Identifier
    , fromAngle : Angle
    , toAngle : Angle
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
connect fromId toId fromAngle containers =
    let
        currentConnection : Connection
        currentConnection =
            { from = fromId
            , to = toId
            , fromAngle = fromAngle
            , toAngle = (fromAngle + 180) % 360
            }

        fromContainer : Container
        fromContainer =
            getContainer fromId containers

        toContainer : Container
        toContainer =
            getContainer toId containers
    in
    containers
        |> setContainer (addConnection currentConnection fromContainer)
        |> setContainer (addConnection currentConnection toContainer)


getConnections : Container -> List Connection
getConnections currentContainer =
    case currentContainer of
        Input { connections } ->
            connections

        Wheel { connections } ->
            connections

        Output { connections } ->
            connections
