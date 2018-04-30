module Update exposing (..)

import Commands exposing (setData, getObject, getPureData)
import Models exposing (Model)
import Msgs exposing (Msg)
-- import Routing exposing (parseLocation)
import RemoteData

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Msgs.UpdateQuery value ->
        { model | hash = value } ! []

    Msgs.UpdateData value ->
        { model | data = value } ! []

    Msgs.SetDataRequest ->
        ( model, setData model.data model.hash )

    Msgs.GetObjectRequest hash ->
        ( model, getObject hash )

    Msgs.GetObject response ->
        ( { model | object = response }, Cmd.none )

    Msgs.GetIpfsHash response ->
        { model | headers = response } ! [ getPureData <| RemoteData.withDefault "" model.headers ]

    Msgs.UpdatePureData response ->
        ( { model | pure_data = response }, Cmd.none )

{-

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.OnFetchPlayers response ->
            ( { model | players = response }, Cmd.none )

        Msgs.OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )

        Msgs.ChangeLevel player howMuch ->
            let
                updatedPlayer =
                    { player | level = player.level + howMuch }
            in
                ( model, savePlayerCmd updatedPlayer )

        Msgs.OnPlayerSave (Ok player) ->
            ( updatePlayer model player, Cmd.none )

        Msgs.OnPlayerSave (Err error) ->
            ( model, Cmd.none )


updatePlayer : Model -> Player -> Model
updatePlayer model updatedPlayer =
    let
        pick currentPlayer =
            if updatedPlayer.id == currentPlayer.id then
                updatedPlayer
            else
                currentPlayer

        updatePlayerList players =
            List.map pick players

        updatedPlayers =
            RemoteData.map updatePlayerList model.players
    in
        { model | players = updatedPlayers }

-}