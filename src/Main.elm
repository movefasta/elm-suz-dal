module Main exposing (..)

import Commands exposing (getObject)
import Models exposing (Node, Model, Link, Object, Hash)
import Msgs exposing (Msg)
import Update exposing (update)
import View exposing (view)
import Html exposing (program)
import RemoteData


init : ( Model, Cmd Msg )
init =
    (initModel, getObject initModel.hash)


initHash : Hash
initHash = "QmUuGJ2gVET4JycdoJTGVk8VYHiRpf3B8WRhqWwiFG2idZ"


initModel : Model
initModel =
    { object = RemoteData.Loading
    , hash = initHash
    , data = ""
    , headers = RemoteData.NotAsked
    , pure_data = RemoteData.Loading
    , path = [ ("Home", initHash) ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
