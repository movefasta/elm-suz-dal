module Main exposing (..)

import Commands exposing (..)
import Update exposing (update)
import View exposing (view)
import Html exposing (program)
import RemoteData
import Ports
import DropZone
import MimeType exposing (MimeType)

init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.batch [ getObject initModel.hash ] )

initHash : Hash
initHash =
    "QmaaWo8DMGGkdVvMFz7Ld4DzdeyhRHNGy2aBpM7TcCKWLu"

type Msg 
    = UpdateQuery String
    | NoOp
    | UpdateData String
    | ObjectPut (List Value)
    | UpdateNode (WebData Object)
    | UpdatePreview (WebData String)
    | UpdateLink Link EntryStatus
    | AddLink (WebData Link)
    | GetModifiedObject (WebData Hash)
    | PathPatchUpdate (Result Http.Error Path)
    | DraftUpdate (Result Http.Error (List Link))
    | PatchObjectUpdate (Result Http.Error Hash)
    | PathInit Hash
    | AddNodeToPath Node
    | FileCat Hash
    | FileGet (WebData String)
    | DnD (DropZone.DropZoneMessage (List NativeFile))


initModel : Model
initModel =
    { root = initHash
    , data = ""
    , links = []
    , content = []
    , raw_dag = RemoteData.Loading
    , path = [("Home", initHash)]
    , draft = []
    , dropZone =
        DropZone.init
    , files_to_add = []
    }

type alias Model =
    { root : Hash -- query hash (root)
    , data : Data -- some data to send
    , links : List Node
    , content : List File -- file list
    , path : List Node -- dag nodes navigation
    , raw_dag : WebData String -- daw dag for response debugging
    , dropZone :
        DropZone.Model
    , files_to_add : List NativeFile
    }

type EntryStatus 
    = Editing
    | Completed

type alias Object =
    { data : Data
    , links : List Node
    }

type alias Node =
    { name : Maybe String
    , cid : String
    , size : Int
    , children : List Node
    , parent : Maybe Node
    , content : List File
    , title : Maybe String
    }

type alias File =
    { name : String
    , cid : String
    , size : Int
    , mimetype : MimeType
    }

type alias Link =
    { name : String
    , size : Int
    , cid : String
    , status : EntryStatus
    }

subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveData Msgs.UpdateQuery

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }