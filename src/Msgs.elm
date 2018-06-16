module Msgs exposing (..)

import Models exposing (..)
import RemoteData exposing (WebData)
import Json.Encode exposing (Value)


type Msg 
    = UpdateQuery Hash
    | UpdateData Data
    | DagGet Name Hash
    | DagPut Value
    | PreviewGet Link
    | UpdateNode (WebData String)
    | UpdatePreview (WebData String)
    | UpdateDescription Data
    | UpdateLink Link
    | EditText Link
    | NoOp