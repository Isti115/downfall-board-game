module Downfall.Game exposing (..)

import Dict exposing (Dict)
import Downfall.Connection exposing (connect)
import Downfall.Container
    exposing
        ( makeInput
        , makeOutput
        , makeWheel
        )
import Downfall.ContainerStore exposing (insertContainer)
import Downfall.Slot
    exposing
        ( makeSlot
        )
import Downfall.Types
    exposing
        ( ConnectionDescriptor
        , Container(Input, Output, Wheel)
        , ContainerStore
        , Game
        , GameDescriptor
        , InputDescriptor
        , Msg(Rotate)
        , OutputDescriptor
        , WheelDescriptor
        )


inputFromDescriptor : InputDescriptor -> Container
inputFromDescriptor inputDescriptor =
    makeInput inputDescriptor.identifier (List.repeat inputDescriptor.circleCount inputDescriptor.colorType)


wheelFromDescriptor : WheelDescriptor -> Container
wheelFromDescriptor wheelDescriptor =
    makeWheel wheelDescriptor.identifier wheelDescriptor.angle (List.map makeSlot wheelDescriptor.slotAngles)


outputFromDescriptor : OutputDescriptor -> Container
outputFromDescriptor outputDescriptor =
    makeOutput outputDescriptor.identifier []


applyConnectionDescriptor : ConnectionDescriptor -> ContainerStore -> ContainerStore
applyConnectionDescriptor connectionDescriptor containers =
    containers |> connect connectionDescriptor.from connectionDescriptor.to connectionDescriptor.angle


init : GameDescriptor -> Game
init gameDescriptor =
    let
        containers =
            List.foldl insertContainer
                Dict.empty
                ([]
                    ++ List.map inputFromDescriptor gameDescriptor.inputs
                    ++ List.map wheelFromDescriptor gameDescriptor.wheels
                    ++ List.map outputFromDescriptor gameDescriptor.outputs
                )
    in
    { containers =
        List.foldl applyConnectionDescriptor containers gameDescriptor.connections
    }
