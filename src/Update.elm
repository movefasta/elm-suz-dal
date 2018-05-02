module Update exposing (..)

import Commands exposing (setData, getObject, getPureData, removeLink, addLink)
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
        ( { model | hash = hash }, Cmd.batch [ getObject hash, getPureData hash ] )

    Msgs.GetObject response ->
        ( { model | object = response }, Cmd.none )

    Msgs.GetModifiedObject response ->
        let
            ipfs_hash = RemoteData.withDefault "" response
        
        in        
            ( { model | hash = ipfs_hash }, getObject <| ipfs_hash )

    Msgs.GetIpfsHash response ->
        let
            ipfs_hash = RemoteData.withDefault "" response

        in
            ( { model | headers = response, hash = ipfs_hash }, getObject <| ipfs_hash )

    Msgs.UpdatePureData response ->
        ( { model | pure_data = response }, Cmd.none )

    Msgs.RemoveLink link ->
        ( model, removeLink model.hash link )

    Msgs.AddLink name hash ->
       ( model, addLink model.hash name hash )
{-
updatedLinks = { response | links = List.filter (\x -> x.name /= link.name) response.links }


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