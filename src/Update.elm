module Update exposing (..)

import Commands exposing (..)
import Models exposing (Model, Node)
import Msgs exposing (Msg)
import RemoteData
import Json.Decode as Decode
import Ports


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.UpdateQuery value ->
            { model | hash = value } ! []

        Msgs.UpdateData value ->
            { model | data = value } ! []

        Msgs.DagGet hash ->
            ( model, dagGet hash )

        Msgs.DagPut data ->
            ( model, Ports.sendData data )



-- ДОСТОЯНИЕ ИСТОРИИ
{-

   Msgs.GetNodeFromJS array ->
       let
           decode_result = Result.withDefault {typ="",data=[]}
           <| Decode.decodeString multihashDecoder array
       in
       ( { model | multihash = decode_result, hash = UTF.toSingleByte <| Result.withDefault "cant" <| UTF8.toString decode_result.data  }, Cmd.none )

   Msgs.DagHash data ->
       ( { model | hash = data }, Cmd.none )

   Msgs.SetDataRequest ->
       ( model, Ports.setData { multihash = model.hash, data = model.data } )

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

-}
