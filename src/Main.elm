module Main exposing (..)

-- import Commands exposing (..)
import Models exposing (Model, Link, Object, Hash, initModel)
import Msgs exposing (Msg)
import Html exposing (programWithFlags)
-- import Navigation exposing (Location)
-- import Routing
import Update exposing (update)
import View exposing (view)

    
init : (Model, Cmd Msg)
init =
  (initModel, Cmd.none)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


{-
init : Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute =
            Routing.parseLocation location
    in
        ( initialModel currentRoute, fetchPlayers )


main : Program Never Model Msg
main =
    Navigation.program Msgs.OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
-}