module Downfall.Update exposing (Msg(Rotate), update)

import Downfall.Model
    exposing
        ( Angle
        , Connection
        , Container(Input, Output, Wheel)
        , ContainerStore
        , Identifier
        , Model
        , getConnections
        , getContainer
        , getIdentifier
        , rotateContainer
        , setContainer
        )
import Utilities


type Msg
    = Rotate Identifier Angle


updateContainers : Connection -> ( Container, Container ) -> ( Container, Container )
updateContainers connection ( rotatedContainer, currentContainer ) =
    case rotatedContainer of
        Input _ ->
            Debug.crash "updateContainers: Inputs should never be rotated."

        Wheel rotatedContainerRecord ->
            case currentContainer of
                Input containerRecord ->
                    -- if getSlotAt
                    ( rotatedContainer, currentContainer )

                Wheel containerRecord ->
                    ( rotatedContainer, currentContainer )

                Output containerRecord ->
                    ( rotatedContainer, currentContainer )

        Output _ ->
            Debug.crash "updateContainers: Outputs should never be rotated."


processConnection : Identifier -> Connection -> ( Container, ContainerStore ) -> ( Container, ContainerStore )
processConnection identifier connection ( currentRotatedContainer, currentContainers ) =
    let
        ( updatedCurrentRotatedContainer, updatedCurrentContainer ) =
            updateContainers
                connection
                ( currentRotatedContainer
                , getContainer
                    (if connection.from /= identifier then
                        connection.from
                     else
                        connection.to
                    )
                    currentContainers
                )
    in
    ( updatedCurrentRotatedContainer
    , currentContainers |> setContainer updatedCurrentContainer
    )


processConnections : Identifier -> Container -> List Connection -> ContainerStore -> ContainerStore
processConnections identifier rotatedContainer filteredConnections containers =
    let
        -- updatedRotatedContainer : Container
        -- updatedContainers : ContainerStore
        ( updatedRotatedContainer, updatedContainers ) =
            filteredConnections
                |> List.foldl
                    (processConnection identifier)
                    ( rotatedContainer, containers )
    in
    updatedContainers |> setContainer updatedRotatedContainer


connectionFilter : Container -> Connection -> Bool
connectionFilter currentContainer connection =
    case currentContainer of
        Input _ ->
            Debug.crash "connectionFilter: Inputs do not have slots."

        Wheel containerRecord ->
            containerRecord.slots
                |> List.any
                    (\slot ->
                        containerRecord.angle
                            + slot.angle
                            == connection.fromAngle
                            || containerRecord.angle
                            + slot.angle
                            == connection.toAngle
                    )

        Output _ ->
            Debug.crash "connectionFilter: Outputs do not have slots."


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rotate identifier angle ->
            let
                currentContainer : Container
                currentContainer =
                    getContainer identifier model.containers

                rotatedContainer : Container
                rotatedContainer =
                    rotateContainer (Utilities.sign angle) currentContainer

                filteredConnections =
                    getConnections rotatedContainer
                        |> List.filterMap (connectionFilter rotatedContainer)

                updatedContainers : ContainerStore
                updatedContainers =
                    model.containers
                        |> setContainer rotatedContainer
                        |> processConnections identifier rotatedContainer filteredConnections

                updatedModel : Model
                updatedModel =
                    { model | containers = updatedContainers }
            in
            if angle > 0 then
                update (Rotate identifier (angle - 1)) updatedModel
            else if angle < 0 then
                update (Rotate identifier (angle + 1)) updatedModel
            else
                model ! []
