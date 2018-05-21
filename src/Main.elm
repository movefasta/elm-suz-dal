module Main exposing (..)

import Commands exposing (getObject)
import Models exposing (Node, Model, Link, Object, Hash)
import Msgs exposing (Msg)
import Update exposing (update)
import View exposing (view)
import Html exposing (program)
import RemoteData
import Ports


init : ( Model, Cmd Msg )
init =
    (initModel, getObject initModel.hash)


initHash : Hash
initHash = "QmaTTU7VZ3kD7Sz8ffrRVD7eLisDT8uJxgnNj92hcTZsFT"


initModel : Model
initModel =
    { object = RemoteData.Loading
    , hash = initHash
    , data = ""
    , headers = RemoteData.NotAsked
    , raw_dag = RemoteData.Loading
    , path = [ ("Home", initHash) ]
    }

subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveData Msgs.DagHash

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
