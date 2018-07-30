module Main exposing (..)

import Commands exposing (..)
import Models exposing (..)
import Msgs exposing (Msg)
import Update exposing (update)
import View exposing (view)
import Html exposing (program)
import RemoteData
import Ports
import DropZone


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.batch [ lsObjects initModel.hash, previewGet initModel.hash ] )


initHash : Hash
initHash =
    "QmaaWo8DMGGkdVvMFz7Ld4DzdeyhRHNGy2aBpM7TcCKWLu"

initLink : Link
initLink =
    { name = "имя ссылки"
    , size = 0  
    , cid = "адрес(мультихэш)"
    , obj_type = 2
    , status = Completed
    }

initModel : Model
initModel =
    { hash = initHash
    , data = ""
    , node = []
    , link = initLink
    , raw_dag = RemoteData.Loading
    , path = [ ( "Home", initHash ) ]
    , draft = []
    , dropZone =
        DropZone.init
    , files = []
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
        