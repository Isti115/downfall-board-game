module Downfall.Model
    exposing
        ( Angle
        , Container(Input, Output, Wheel)
        , Identifier
        , Model
        , connect
        , makeInput
        , makeOutput
        , makeSlot
        , makeWheel
        , rotateWheel
        )


type alias Model =
    { containers : List Container
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
    -> ({ containerRecord | connections : List Connection, identifier : Identifier } -> Container)
    -> { containerRecord | connections : List Connection, identifier : Identifier }
    -> Identifier
    -> Identifier
    -> Angle
    -> List Container
    -> List Container
connectHelper currentConnection wrapper currentContainer fromId toId angle rest =
    let
        connectedRest =
            connect fromId toId angle rest
    in
    if
        currentContainer.identifier
            == fromId
            || currentContainer.identifier
            == toId
    then
        wrapper
            { currentContainer
                | connections = currentConnection :: currentContainer.connections
            }
            :: connectedRest
    else
        wrapper currentContainer :: connectedRest


connect : Identifier -> Identifier -> Angle -> List Container -> List Container
connect fromId toId angle containers =
    let
        currentConnection =
            { from = fromId
            , to = toId
            , angle = angle
            }
    in
    case containers of
        (Input currentContainer) :: rest ->
            connectHelper currentConnection Input currentContainer fromId toId angle rest

        (Wheel currentContainer) :: rest ->
            connectHelper currentConnection Wheel currentContainer fromId toId angle rest

        (Output currentContainer) :: rest ->
            connectHelper currentConnection Output currentContainer fromId toId angle rest

        [] ->
            []
