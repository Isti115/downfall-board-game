module Downfall.Types
    exposing
        ( Angle
        , Color
        , Connection
        , ConnectionDescriptor
        , Container(Input, Output, Wheel)
        , ContainerStore
        , Direction(In, Out)
        , Game
        , GameDescriptor
        , Identifier
        , InputDescriptor
        , InputRecord
        , Msg(Rotate)
        , OutputDescriptor
        , OutputRecord
        , Slot
        , SlotState(Empty, Occupied)
        , Status
        , WheelDescriptor
        , WheelRecord
        )

import Dict exposing (Dict)


-- Game


type alias InputDescriptor =
    { identifier : Identifier
    , colorType : Color
    , circleCount : Int
    }


type alias WheelDescriptor =
    { identifier : Identifier
    , angle : Angle
    , slotAngles : List Angle
    }


type alias OutputDescriptor =
    { identifier : Identifier
    }


type alias ConnectionDescriptor =
    { from : Identifier
    , to : Identifier
    , angle : Angle
    }


type alias GameDescriptor =
    { inputs : List InputDescriptor
    , wheels : List WheelDescriptor
    , outputs : List OutputDescriptor
    , connections : List ConnectionDescriptor
    }


type alias Game =
    { containers : ContainerStore
    }


type alias Status =
    { outputCount : Int
    }



-- ContainerStore


type alias ContainerStore =
    Dict String Container



-- Container


type alias Identifier =
    String


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
