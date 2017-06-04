module Downfall.ContainerStore
    exposing
        ( getContainer
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
        , Container(Input, Output, Wheel)
        , ContainerStore
        , Identifier
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


getSumOfOutputs : ContainerStore -> Int
getSumOfOutputs containers =
    Dict.foldl
        (\k v s ->
            case v of
                Input _ ->
                    s

                Wheel _ ->
                    s

                Output containerRecord ->
                    s + List.length containerRecord.colors
        )
        0
        containers
