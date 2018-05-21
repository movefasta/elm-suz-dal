module Update exposing (..)

import Commands exposing (dagGet, pathUpdate, pathForUrl, setData, getObject, removeLink, addLink)
import Models exposing (Model, Node)
import Msgs exposing (Msg)
import RemoteData
import Ports

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Msgs.UpdateQuery value ->
        { model | hash = value } ! []

    Msgs.UpdateData value ->
        { model | data = value } ! []

    Msgs.SetDataRequest ->
        ( model, setData model.data model.hash )

    Msgs.GetObjectRequest name hash ->
        ( { model | hash = hash, path = pathUpdate (name, hash) [] model.path }
            , Cmd.batch [ getObject hash ] )

    Msgs.GetObject response ->
        let
            object =
                RemoteData.withDefault {links = [], data = ""} response
        in
            ( { model | object = response }, Cmd.none )

    Msgs.GetModifiedObject response ->
        let
            ipfs_hash = RemoteData.withDefault "" response
        
        in
            ( { model | hash = ipfs_hash }, Cmd.batch [ getObject ipfs_hash, dagGet ipfs_hash ] )

    Msgs.GetIpfsHash response ->
        let
            ipfs_hash = RemoteData.withDefault "" response

        in
            ( { model | headers = response, hash = ipfs_hash }, getObject ipfs_hash )

    Msgs.UpdatePureData response ->
        ( { model | raw_dag = response }, Cmd.none )

    Msgs.RemoveLink link ->
        ( model, removeLink model.hash link )

    Msgs.AddLink name hash ->
       ( model, addLink model.hash name hash )

    Msgs.DagGet hash ->
        ( model, dagGet hash )

    Msgs.DagPut data ->
        ( model, Ports.sendData data )

    Msgs.DagHash data ->
        ( { model | hash = data }, Cmd.none )


{-
-}