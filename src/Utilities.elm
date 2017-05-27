module Utilities exposing (sign)

-- betweenAngles : Float -> Float -> Float -> Bool
-- betweenAngles _ _ _ =
--     False


sign : comparable -> comparable
sign x =
    if x < 0 then
        -1
    else if x > 0 then
        1
    else
        0
