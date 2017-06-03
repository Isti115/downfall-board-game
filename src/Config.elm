module Config exposing (currentGameDescriptor)

import Downfall.Types
    exposing
        ( ConnectionDescriptor
        , GameDescriptor
        , InputDescriptor
        , OutputDescriptor
        , WheelDescriptor
        )


currentGameDescriptor : GameDescriptor
currentGameDescriptor =
    { inputs =
        [ InputDescriptor "A" "Green" 5
        ]
    , wheels =
        [ WheelDescriptor "1" 0 [ 45, 135, 225, 315 ]
        , WheelDescriptor "2" 0 [ 0, 120, 240 ]
        , WheelDescriptor "3" 0 [ 90, 270 ]
        ]
    , outputs =
        [ OutputDescriptor "X"
        ]
    , connections =
        [ ConnectionDescriptor "A" "1" 115
        , ConnectionDescriptor "1" "2" 230
        , ConnectionDescriptor "2" "3" 180
        , ConnectionDescriptor "3" "X" 160
        ]
    }
