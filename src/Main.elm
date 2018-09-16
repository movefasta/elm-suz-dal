module Main exposing (..)

import View exposing (view, UpdateQuery)
import Html exposing (program)
import RemoteData
import Ports
import DropZone


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.batch [ getObject initModel.hash ] )

initHash : Hash
initHash =
    "QmaaWo8DMGGkdVvMFz7Ld4DzdeyhRHNGy2aBpM7TcCKWLu"


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

subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveData UpdateQuery

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }