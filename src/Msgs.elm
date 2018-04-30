module Msgs exposing (..)

import Models exposing (Hash, Object, ModifiedObject, Link, Data, File)
-- import Navigation exposing (Location)
import RemoteData exposing (WebData)

type Msg 
    = UpdateQuery Hash
    | UpdateData Data
    | GetObjectRequest Hash
    | SetDataRequest
    | GetObject (WebData Object)
    | GetIpfsHash (WebData Hash)
    | UpdatePureData (WebData String)

{-

type Msg
    = OnFetchPlayers (WebData (List Player))
    | OnLocationChange Location
    | ChangeLevel Player Int
    | OnPlayerSave (Result Http.Error Player)

-}