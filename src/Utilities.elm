module Utilities exposing (sign)


sign : comparable -> comparable
sign x =
    if x < 0 then
        -1
    else if x > 0 then
        1
    else
        0
