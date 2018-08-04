module Update exposing (..)

import Commands exposing (..)
import Models exposing (..)
import Msgs exposing (Msg)
import RemoteData
import Json.Decode as Decode
import Ports
import Dom
import Task
import DropZone exposing (DropZoneMessage(Drop), dropZoneEventHandlers, isHovering)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        defaultLink =
            { name = "defaultLink", size = 0, cid = "", obj_type = 2, status = Completed }
    in
            
    case msg of
        Msgs.NoOp ->
            model ! []

        Msgs.UpdateQuery value ->
            ( { model | hash = value }, previewGet value )

        Msgs.UpdateData value ->
            { model | data = value } ! []
        -- api/v0/ls?arg=hash
        Msgs.LsObjects hash ->
            ( model, lsObjects hash )                        
        -- api/v0/dag/get?arg=hash
        Msgs.DagGet name hash ->
            let
                newPath =
                    pathStackUpdate (name, hash) model.path
            in
                ( { model | path = newPath, hash = hash }, Cmd.batch [ lsObjects hash ] )

        Msgs.PathInit hash ->
            ( { model | path = [("Home", hash)] }, lsObjects hash )

        Msgs.AddNodeToPath (name, hash) ->
            ( { model | path = (name, hash) :: model.path }, lsObjects hash )

        Msgs.DraftUpdate response ->
            case response of
                Ok links ->
                    ( { model | draft = model.draft ++ links }, 
                        Task.attempt Msgs.PatchObjectUpdate <| patchObject model.link.cid links )
                Err error ->
                    ( { model | data = Basics.toString <| error }, Cmd.none )

        Msgs.PatchObjectUpdate response ->
            case response of
                Ok hash ->
                    ( model, Cmd.batch [ lsObjects hash,
                                Task.attempt Msgs.PathPatchUpdate 
                                    <| patchPath [(model.link.name, hash)] model.path hash ] )
                Err error ->
                    ( { model | data = Basics.toString <| error }, Cmd.none )

        Msgs.PathPatchUpdate response ->
            case response of
                Ok path ->
                    ( { model | path = path }, Cmd.none )
                Err error ->
                    ( { model | data = Basics.toString <| error }, Cmd.none )

        Msgs.UpdateNode response ->
            let
                object =
                    RemoteData.withDefault "response fails" response
                
                newNode =
                    Result.withDefault [ defaultLink ] <| Decode.decodeString objectsDecoder object
            in
                ( { model | node = newNode, draft = newNode, raw_dag = response }, Cmd.none )

        Msgs.PreviewGet link ->
            ( { model | link = link, data = link.name }, previewGet link.cid )

        Msgs.UpdatePreview response ->
            ( { model | raw_dag = response }, Cmd.none )

        Msgs.UpdateLink link status ->
            let
                updateNodeLinksList x =
                    if x.name == link.name then
                        { link | status = status }
                    else
                        x

                updateLinkStatus =
                    { link | status = status }

                cmd =
                    case status of 
                        Editing ->
                            Task.attempt (\_ -> Msgs.NoOp) <| Dom.focus ("link-" ++ link.name)
                        Completed ->
                            Ports.sendData <| objectEncoder model.data model.node
            in
                { model | node = List.map updateNodeLinksList model.node, link = updateLinkStatus }
                    ! [ cmd ]

        Msgs.DnD (Drop files) ->
            ( { model
                | dropZone =
                    DropZone.update (Drop files) model.dropZone
                , files = 
                    files
            }
            , addFiles files model.link model.path
            )

        Msgs.DnD a ->
            ( { model | dropZone = DropZone.update a model.dropZone }, Cmd.none )


        Msgs.AddLink response ->
            let
                link =
                    RemoteData.withDefault defaultLink response

                newNode =
                    model.node ++ [ link ]
            in
                ( { model | node = newNode }, Cmd.none)

        Msgs.GetModifiedObject response ->
            let
                ipfs_hash = RemoteData.withDefault "" response
            in
                ( { model | hash = ipfs_hash }, Cmd.batch [ dagGet ipfs_hash ] )

        Msgs.DagPut data ->
            ( model, Ports.sendData data )

        Msgs.DagPutPB data ->
            ( model, Ports.sendDataPB data )


{-

        Msgs.UpdateDescription name ->
            let
                updateLink x = { x | name = name }
            in
                { model | link = updateLink model.link } ! []

        Msgs.GetObject response ->
            let
                object =
                    RemoteData.withDefault {links = [], data = ""} response
            in
                ( { model | files = response }, Cmd.none )

        FileReadSucceeded str ->
            -- this happens when an effect has finished and the file has successfully been loaded
            ( { model
                | contents = str :: model.contents
                , message = "Successfully loaded at least one file"
              }
            , Cmd.none
            )

        FileReadFailed err ->
            -- this happens when an effect has finished and there was an error loading hte file
            ( { model | message = FileReader.prettyPrint err }
            , Cmd.none
            )

        Msgs.AddLink link ->
          let
            newNode =
              { node | links = model.links ++ link }
          in
            { model | node = newNode } ! [ Cmd.none ]

        FileUpload ->
            ( model, model.file |> Maybe.map sendFileToServer |> Maybe.withDefault Cmd.none )

       Msgs.DagHash data ->
           ( { model | hash = data }, Cmd.none )

       Msgs.SetDataRequest ->
           ( model, Ports.setData { multihash = model.hash, data = model.data } )

       Msgs.GetObjectRequest name hash ->
           ( { model | hash = hash, path = pathUpdate (name, hash) [] model.path }
               , Cmd.batch [ getObject hash ] )

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
