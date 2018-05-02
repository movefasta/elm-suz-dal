module Main exposing (..)

import Commands exposing (..)
import Models exposing (Model, Link, Object, Hash, initModel)
import Msgs exposing (Msg)
import Html exposing (programWithFlags)
import Commands exposing (getObject)
import Navigation exposing (Location)
import Routing
import Update exposing (update)
import View exposing (view)

init : Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute =
            Routing.parseLocation location
    in
        (initModel, Commands.getObject initModel.hash)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Navigation.program Msgs.OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
-}