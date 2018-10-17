port module Ports exposing (..)

import Json.Encode as Encode exposing (Value)

port sendData : Value -> Cmd msg
port receiveData : (String -> msg) -> Sub msg
port sendDataPB : List Value -> Cmd msg