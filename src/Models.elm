module Models exposing (..)

import RemoteData exposing (WebData)
import DropZone
import FileReader exposing (NativeFile)

type alias Model =
    { hash : Hash -- query hash
    , data : Data -- some data to send
    , node : List Link
    , link : Link
    , draft : List Link -- file list
    , raw_dag : WebData String -- daw dag for response debugging
    , path : List Node -- breadthumbs or dag nodes navigation
    , dropZone :
        DropZone.Model
    , files : List NativeFile
    }

type alias Cid =
    { slash : Hash }

type EntryStatus 
    = Editing
    | Completed

type alias Path =
    List Node

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
    , obj_type : Int
    , status : EntryStatus
    }

type alias AddFileResponse =
    { name : Name
    , size : Int
    , hash : Hash
    }

type LinkType 
    = Raw
    | Directory
    | File
    | Metadata
    | Symlink

type alias Hash =
    String

type alias Name =
    String

type alias Data =
    String

type alias Level =
    Int