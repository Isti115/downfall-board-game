module Downfall.ContainerStore
    exposing
        ( getContainer
        , getNeuralData
        , getSumOfOutputs
        , insertContainer
        , pullFromInput
        , pushToOutput
        , setContainer
        )

import Dict exposing (Dict)
import Downfall.Container exposing (getIdentifier)
import Downfall.Types
    exposing
        ( Color
        , Connection
        , Container(Input, Output, Wheel)
        , ContainerStore
        , Identifier
        , SlotState(Empty, Occupied)
        )


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


connectionsToNeuralData : List Connection -> List Float
connectionsToNeuralData connections =
    connections
        |> List.map (\connection -> toFloat connection.localAngle / 360)


getNeuralData : ContainerStore -> List Float
getNeuralData containers =
    Dict.toList containers
        |> List.concatMap
            (\( k, c ) ->
                case c of
                    Input containerRecord ->
                        [ toFloat (List.length containerRecord.colors) / toFloat containerRecord.initialCount ]
                            ++ connectionsToNeuralData containerRecord.connections

                    Wheel containerRecord ->
                        (toFloat containerRecord.angle / 360)
                            :: (containerRecord.slots
                                    |> List.concatMap
                                        (\slot ->
                                            [ toFloat slot.angle / 360
                                            , case slot.state of
                                                Empty ->
                                                    0

                                                Occupied _ ->
                                                    1
                                            ]
                                        )
                               )
                            ++ connectionsToNeuralData containerRecord.connections

                    Output containerRecord ->
                        connectionsToNeuralData containerRecord.connections
                            ++ [ toFloat (List.length containerRecord.colors) / toFloat containerRecord.finishedCount ]
            )


getSumOfOutputs : ContainerStore -> ( Int, Int )
getSumOfOutputs containers =
    Dict.foldl
        (\k v ( s, sm ) ->
            case v of
                Input _ ->
                    ( s, sm )

                Wheel _ ->
                    ( s, sm )

                Output containerRecord ->
                    ( s + List.length containerRecord.colors
                    , sm + containerRecord.finishedCount
                    )
        )
        ( 0, 0 )
        containers
