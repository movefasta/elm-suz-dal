module Msgs exposing (..)

import Models exposing (..)
import RemoteData exposing (WebData)
import Json.Encode exposing (Value)


type Msg 
    = UpdateQuery Hash
    | UpdateData Data
    | DagGet Hash
    | DagPut Value

{-
    | DagHash String
    | GetObjectRequest Name Hash
    | SetDataRequest
    | RemoveLink Link
    | AddLink Name Hash
    | GetModifiedObject (WebData Hash)
    | GetObject (WebData Object)
    | GetIpfsHash (WebData Hash)
    | UpdatePureData (WebData String)
    | GetNodeFromJS String
-}