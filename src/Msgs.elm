module Msgs exposing (..)

import Models exposing (..)
import RemoteData exposing (WebData)
import Json.Encode exposing (Value)
import DropZone exposing (DropZoneMessage(Drop))
import FileReader exposing (NativeFile)

type Msg 
    = UpdateQuery Hash
    | UpdateData Data
    | DagGet Name Hash
    | DagPut Value
    | DagPutPB Value
    | PreviewGet Link
    | UpdateNode (WebData String)
    | UpdatePreview (WebData String)
    | UpdateDescription Data
    | UpdateLink Link EntryStatus
    | NoOp
    | DnD (DropZone.DropZoneMessage (List NativeFile))
    | AddLink (WebData Link)
    | GetModifiedObject (WebData Hash)
    | LsObjects Hash
    | PathUpdate Path