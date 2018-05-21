module Msgs exposing (..)

import Models exposing (Level, Name, Hash, Object, ModifiedObject, Link, Data)
import RemoteData exposing (WebData)
import Json.Encode exposing (Value)


type Msg 
    = UpdateQuery Hash
    | UpdateData Data
    | GetObjectRequest Name Hash
    | SetDataRequest
    | RemoveLink Link
    | AddLink Name Hash
    | GetModifiedObject (WebData Hash)
    | GetObject (WebData Object)
    | GetIpfsHash (WebData Hash)
    | UpdatePureData (WebData String)
    | DagGet Hash
    | DagPut Value
    | DagHash String
