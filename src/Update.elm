module Update exposing (..)

import Commands exposing (pathUpdate, pathForUrl, setData, getObject, getPureData, removeLink, addLink)
import Models exposing (Model, Node)
import Msgs exposing (Msg)
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

    Msgs.GetObjectRequest name hash ->
        ( { model | hash = hash, path = pathUpdate (name, hash) [] model.path }
            , Cmd.batch [ getObject hash ] )

    Msgs.GetObject response ->
        let
            object =
                RemoteData.withDefault {links = [], data = ""} response
        in
            ( { model | object = object }, Cmd.batch <| List.map (\a -> getPureData a.hash) object.links )

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
        let
            newData = RemoteData.withDefault "" response
        in
            ( { model | data = newData }, Cmd.none )

    Msgs.RemoveLink link ->
        ( model, removeLink model.hash link )

    Msgs.AddLink name hash ->
       ( model, addLink model.hash name hash )
{-
    Msgs.GetData link ->
        ( model, getPureData link.hash )}
-}