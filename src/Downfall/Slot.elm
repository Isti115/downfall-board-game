module Downfall.Slot
    exposing
        ( getSlotAt
        , getSlots
        , makeSlot
        , setSlotAt
        , setSlots
        )

import Downfall.Container exposing (getAngle)
import Downfall.Types
    exposing
        ( Angle
        , Container(Input, Output, Wheel)
        , Slot
        , SlotState(Empty, Occupied)
        )


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
