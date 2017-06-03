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
        ( Container(Input, Output, Wheel)
        , ContainerStore
        , Game
        , Msg(Rotate)
        )


init : Game
init =
    { containers =
        [ makeInput "A" (List.repeat 5 "Green")
        , makeWheel "1" 0 (List.map makeSlot [ 45, 135, 225, 315 ])
        , makeWheel "2" 0 (List.map makeSlot [ 0, 120, 240 ])
        , makeWheel "3" 0 (List.map makeSlot [ 90, 270 ])
        , makeOutput "X" []
        ]
            |> List.foldl insertContainer Dict.empty
            |> connect "A" "1" 115
            |> connect "1" "2" 230
            |> connect "2" "3" 180
            |> connect "3" "X" 160
    }
