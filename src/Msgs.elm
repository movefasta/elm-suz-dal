module Msgs exposing (..)

import Models exposing (..)
import RemoteData exposing (WebData)
import Json.Encode exposing (Value)
import DropZone exposing (DropZoneMessage(Drop))
import FileReader exposing (NativeFile)
import Http exposing (Error)

type Msg 
    = UpdateQuery Hash
    | NoOp
    | UpdateData Data
    | DagGet Name Hash
    | DagPut Value
    | DagPutPB Value
    | PreviewGet Link
    | UpdateNode (WebData String)
    | UpdatePreview (WebData String)
    | UpdateLink Link EntryStatus
    | DnD (DropZone.DropZoneMessage (List NativeFile))
    | AddLink (WebData Link)
    | GetModifiedObject (WebData Hash)
    | LsObjects Hash
    | PathPatchUpdate (Result Http.Error Path)
    | DraftUpdate (Result Http.Error (List Link))
    | PatchObjectUpdate (Result Http.Error Hash)
    | PathInit Hash
    | AddNodeToPath Node