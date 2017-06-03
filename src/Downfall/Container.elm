module Downfall.Container
    exposing
        ( getAngle
        , getIdentifier
        , makeInput
        , makeOutput
        , makeWheel
        , pullFromInput
        , pushToOutput
        , rotateContainer
        )

import Downfall.Types
    exposing
        ( Angle
        , Color
        , Container(Input, Output, Wheel)
        , Identifier
        , Slot
        )


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
