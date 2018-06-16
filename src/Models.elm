module Models exposing (..)

import RemoteData exposing (WebData)
--import FileReader exposing (NativeFile)

type alias Model =
    { hash : Hash
    , data : Data
    , node : List Link
    , link : Link
    , draft : List Link
    , raw_dag : WebData String
    , path : List Node
--    , file : Maybe NativeFile
    }

type alias Cid =
    { slash : Hash }

type EntryStatus 
    = Editing
    | Completed

type alias Node =
    ( Name, Hash )

type alias Object =
    { data : Data
    , links : List Link
    }

type alias Link =
    { name : Name
    , size : Int
    , cid : Hash
    , description : String
    , status : EntryStatus
    }

type LinkType 
    = Folder
    | File

type alias Hash =
    String

type alias Name =
    String

type alias Data =
    String

type alias Level =
    Int