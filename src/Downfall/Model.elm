module Downfall.Model
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
        , connect
        , getConnections
        , getContainer
        , getIdentifier
        , getSlotAt
        , insertContainer
        , makeInput
        , makeOutput
        , makeSlot
        , makeWheel
        , pullFromInput
        , pushToOutput
        , rotateContainer
        , setContainer
        , setSlotAt
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


getAngle : Container -> Angle
getAngle currentContainer =
    case currentContainer of
        Input _ ->
            Debug.crash "getAngle: Cannot get angle of an input."

        Wheel { angle } ->
            angle

        Output _ ->
            Debug.crash "getAngle: Cannot get angle of an output."


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


setSlots : List Slot -> Container -> Container
setSlots slots currentContainer =
    case currentContainer of
        Input _ ->
            Debug.crash "setSlots: Inputs do not have slots."

        Wheel containerRecord ->
            Wheel { containerRecord | slots = slots }

        Output _ ->
            Debug.crash "setSlots: Outputs do not have slots."


getSlotAt : Angle -> Container -> Maybe Slot
getSlotAt angle currentContainer =
    let
        currentSlots : List Slot
        currentSlots =
            getSlots currentContainer

        currentAngle : Angle
        currentAngle =
            getAngle currentContainer

        foundSlot : Maybe Slot
        foundSlot =
            currentSlots
                |> List.filter (\slot -> (slot.angle + currentAngle) % 360 == angle)
                |> List.head
    in
    -- case foundSlot of
    --     Just slot ->
    --         slot
    --     Nothing ->
    --         Debug.crash
    --             ("getSlotAt: Container "
    --                 ++ getIdentifier currentContainer
    --                 ++ " doesn't have a slot at "
    --                 ++ toString angle
    --             )
    foundSlot


setSlotAt : Angle -> SlotState -> Container -> Container
setSlotAt angle slotState currentContainer =
    let
        currentSlots : List Slot
        currentSlots =
            getSlots currentContainer

        currentAngle : Angle
        currentAngle =
            getAngle currentContainer

        updatedSlots : List Slot
        updatedSlots =
            currentSlots
                |> List.map
                    (\slot ->
                        if (slot.angle + currentAngle) % 360 == angle then
                            { slot | state = slotState }
                        else
                            slot
                    )
    in
    setSlots updatedSlots currentContainer



--


pullFromInput : Container -> ( Container, Maybe Color )
pullFromInput inputContainer =
    case inputContainer of
        Input containerRecord ->
            case containerRecord.colors of
                currentColor :: rest ->
                    ( Input { containerRecord | colors = rest }, Just currentColor )

                [] ->
                    ( inputContainer, Nothing )

        Wheel _ ->
            Debug.crash "pullFromInput: Should only be called for inputs."

        Output _ ->
            Debug.crash "pullFromInput: Should only be called for inputs."


pushToOutput : Color -> Container -> Container
pushToOutput currentColor outputContainer =
    case outputContainer of
        Input _ ->
            Debug.crash "pushToOutput: Should only be called for outputs."

        Wheel _ ->
            Debug.crash "pushToOutput: Should only be called for outputs."

        Output containerRecord ->
            Output { containerRecord | colors = currentColor :: containerRecord.colors }



--


type Direction
    = Out
    | In


type alias Connection =
    { direction : Direction
    , target : Identifier
    , localAngle : Angle
    , remoteAngle : Angle
    }



-- connectHelper :
--     Connection
--     -> { a | connections : List Connection }
--     -> { a | connections : List Connection }
-- connectHelper connection containerRecord =
--     { containerRecord | connections = connection :: containerRecord.connections }


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
