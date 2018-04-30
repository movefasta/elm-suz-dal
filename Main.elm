module Main exposing (..)

import Html exposing (program, text, div, input, button, p, h3, a, Html)
import Html.Attributes exposing (href, value, attribute)
import Html.Events exposing (..)
-- import Html.Keyed as Keyed
import Http exposing (get, send, Error, Response, Error(..))
import Dict
-- import HttpBuilder exposing (..)
import Json.Decode as Decode
-- import Json.Encode as Encode
import Json.Decode.Pipeline exposing (decode, required, hardcoded)
-- import Base64
import Result

-- MODEL

type alias Header =
    { hash : String }

type alias Link =
    { name : String
    , hash : String
    , size : Int
    }

type alias Object =
    { data : String
    , links : List Link
    }

type alias ModifiedObject =
    { hash : String
    , links : List Link
    }

type alias Model =
  { object : Object
  , modified_object : ModifiedObject
  , modified_hash : String
  , hash : String
  , data : String
  , headers : String
  , pure_data : String
  }

init : (Model, Cmd Msg)
init =
  (Model
    { data = "", links = [] } 
    { hash = "", links = [] }
    "" "QmaHzKAq5XbJXbNS7PHwdHw35eU6fwHBmrC5eA5uSbdcyC" "" "" "", Cmd.none)


-- UPDATE

type Msg 
    = UpdateQuery String
    | UpdateData String
    | UpdatePureData (Result Http.Error String)
    | SetData (Result Http.Error String)
    | SetDataRequest
    | GetObjectRequest
    | GetHeaders (Result Http.Error String)
    | GetObject (Result Http.Error Object)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateQuery value ->
        { model | hash = value } ! []

    UpdateData value ->
        { model | data = value } ! []

    SetDataRequest ->
        ( model, setData model.data model.hash )

    GetObjectRequest ->
        ( model, Cmd.batch [ getObject model.hash, getPureData model.hash ] )

    SetData (Ok headers) ->
        { model | headers = headers } ! [ getPureData headers ]

    SetData (Err _) ->
        ( model, Cmd.none )

    GetObject (Ok object) ->
        { model | object = object } ! []

    GetObject (Err _) ->
        ( model, Cmd.none )

    GetHeaders (Ok value) ->
        { model | headers = value } ! []

    GetHeaders (Err _) ->
        ( model, Cmd.none )

    UpdatePureData (Ok data) ->
        { model | pure_data = data } ! []

    UpdatePureData (Err _) ->
        ( model, Cmd.none )


getPureData : String -> Cmd Msg
getPureData hash =
  let
    url =
      "http://localhost:8080/ipfs/" ++ hash

    request =
      Http.getString url
  in
    Http.send UpdatePureData request

getObject : String -> Cmd Msg
getObject hash =
  let
    url =
      "http://localhost:5001/api/v0/object/get?arg=" ++ hash

    request =
      Http.get url objectDecoder
  in
    Http.send GetObject request


setData : String -> String -> Cmd Msg
setData data hash =
  let
    url =
      "http://localhost:8080/ipfs/" ++ hash

    request =
      put url data
  in
    Http.send SetData request


put : String -> String -> Http.Request String
put url text =
  Http.request
    { method = "PUT"
    , headers = []
    , url = url
    , body = stringtoBody text
    , expect = Http.expectStringResponse getHeader
    , timeout = Nothing
    , withCredentials = False
    }

getHeader : Http.Response String -> Result String String
getHeader response =
    Dict.get "ipfs-hash" (Debug.log "headers" response.headers)
        |> Result.fromMaybe ("ipfs header not found")


stringtoBody : String -> Http.Body
stringtoBody value =
  Http.stringBody "text/plain" value


objectModifiedDecoder : Decode.Decoder ModifiedObject
objectModifiedDecoder =
    decode ModifiedObject
        |> required "Hash" Decode.string
        |> required "Links" linksDecoder


objectDecoder : Decode.Decoder Object
objectDecoder =
    decode Object
        |> required "Data" Decode.string
        |> required "Links" linksDecoder


linksDecoder : Decode.Decoder (List Link)
linksDecoder =
    Decode.list linkDecoder


linkDecoder : Decode.Decoder Link
linkDecoder =
    decode Link
        |> required "Name" Decode.string
        |> required "Hash" Decode.string
        |> required "Size" Decode.int

headerDecoder : Decode.Decoder ModifiedObject
headerDecoder =
    decode ModifiedObject
        |> required "Ipfs-Hash" Decode.string
        |> hardcoded []


isFile: String -> Bool
isFile hash =
    True

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ onInput UpdateQuery, value model.hash ] []
    , button [ onClick GetObjectRequest ] [ text "Get Object Data" ]
    , input [ onInput UpdateData, value model.data, onEnter SetDataRequest ] []
    , button [ onClick SetDataRequest ] [ text "Set Object Data" ]
    , p [] [ text ( "New hash: " ++ model.headers ) ]
    , p [] [ text ( "New data: " ++ model.pure_data ) ] 
    , p [] [ text "Links: " ]
    , div [] (List.map (\{ name, hash } -> 
             p [] [ a [ href ("http://localhost:8080/ipfs/" ++ hash) ] [ text (name) ] ] )
                model.object.links)
    ]


onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg
            else
                Decode.fail "not ENTER"
    in
        on "keydown" (Decode.andThen isEnter keyCode)



main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
