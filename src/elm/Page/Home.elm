module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Hash)
import Api.Endpoint as Endpoint
import Browser.Dom as Dom
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Http
import Loading
import Log
import Page
import PaginatedList exposing (PaginatedList)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Url.Builder
import Username exposing (Username)



-- MODEL


type alias Model =
    { session : Session
    , widgets : List Widget
    }


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed


type Widget
    = Content Hash
    | Feed Hash


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , widgets = []
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Element Msg }
view model =
    { title = "Conduit"
    , content = E.none
    }



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
