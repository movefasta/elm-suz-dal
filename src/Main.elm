module Main exposing (..)

import Commands exposing (..)
import Models exposing (..)
import Msgs exposing (Msg)
import Update exposing (update)
import View exposing (view)
import Html exposing (program)
import RemoteData
import Ports


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.batch [ dagGet initModel.hash, previewGet initModel.hash ] )


initHash : Hash
initHash =
    "zdpuAsSX2eshk21XSNw68jKxNrNiNjyfait15FddyzaxATGhx"

initLink : Link
initLink =
    { name = "имя ссылки"
    , size = 0
    , cid = "адрес(мультихэш)"
    , description = "описание ссылки"
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
