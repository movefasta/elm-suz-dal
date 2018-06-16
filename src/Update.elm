module Update exposing (..)

import Commands exposing (..)
import Models exposing (..)
import Msgs exposing (Msg)
import RemoteData
import Json.Decode as Decode
import Ports
import Dom
import Task


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.NoOp ->
            model ! []
        Msgs.UpdateQuery value ->
            ( { model | hash = value, path = [ ( "Home", value ) ] }, Cmd.none )

        Msgs.UpdateData value ->
            { model | data = value } ! []

        Msgs.DagGet name hash ->
            let
                newPath =
                    pathUpdate (name, hash) [] model.path
            in
                ( { model | path = newPath, hash = hash }, Cmd.batch [ dagGet hash, previewGet hash ] )

        Msgs.DagPut data ->
            ( model, Ports.sendData data )

        Msgs.UpdateNode response ->
            let
                object =
                    RemoteData.withDefault "response fails" response
            in
                ( { model | node = Result.withDefault [ { name = "", size = 0, cid = "", description = "нет ссылок", status = Completed } ]
                              <| Decode.decodeString linksDecoder object }, Cmd.none )

        Msgs.PreviewGet link ->
            ( { model | link = link, data = link.description }, previewGet link.cid )

        Msgs.UpdatePreview response ->
            ( { model | raw_dag = response }, Cmd.none )

        Msgs.UpdateDescription desc ->
            let
                updateLink x = { x | description = desc }
            in
                { model | link = updateLink model.link } ! []

        Msgs.UpdateLink link ->
            let
                updateLink x =
                    if x.name == link.name then
                        { link | status = Completed }
                    else
                        x

                updateLinkStatus =
                    { link | status = Completed }
            in
                { model | node = List.map updateLink model.node, link = updateLinkStatus } ! []

        Msgs.EditText link ->
            let
                updateLinkStatus =
                    { link | status = Editing }

                focus =
                    Dom.focus ("link-" ++ link.name)
            in
                { model | link = updateLinkStatus } ! [ Task.attempt (\_ -> Msgs.NoOp) focus ]
{-

        Msgs.AddLink link ->
            ( { model | node = { data = model.node.data, links = model.node.links ++ [ link ] } }, Cmd.none )

        Msgs.AddLink link ->
          let
            newNode =
              { node | links = model.links ++ link }
          in
            { model | node = newNode } ! [ Cmd.none ]

        Msgs.AddFile file ->

        FileUpload ->
            ( model, model.file |> Maybe.map sendFileToServer |> Maybe.withDefault Cmd.none )

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

   Msgs.RemoveLink link ->
       ( model, removeLink model.hash link )

   Msgs.AddLink name hash ->
      ( model, addLink model.hash name hash )

-}
