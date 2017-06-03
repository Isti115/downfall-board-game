module Downfall.Types
    exposing
        ( Angle
          -- , CircleCount
        , Color
        , Connection
        , Container(Input, Output, Wheel)
        , ContainerStore
        , Direction(In, Out)
        , Game
        , Identifier
        , InputRecord
        , Msg(Rotate)
        , OutputRecord
        , Slot
        , SlotState(Empty, Occupied)
        , WheelRecord
        )

import Dict exposing (Dict)


-- Game


type alias Game =
    { containers : ContainerStore
    }



-- ContainerStore


type alias ContainerStore =
    Dict String Container



-- Container


type alias Identifier =
    String



-- type alias CircleCount =
--     Int


type alias Angle =
    Int


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



-- Slot


type alias Color =
    String


type SlotState
    = Empty
    | Occupied Color


type alias Slot =
    { angle : Angle
    , state : SlotState
    }



-- Connection


type Direction
    = Out
    | In


type alias Connection =
    { direction : Direction
    , target : Identifier
    , localAngle : Angle
    , remoteAngle : Angle
    }



-- Update


type Msg
    = Rotate Identifier Angle
