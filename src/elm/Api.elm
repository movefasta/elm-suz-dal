port module Api exposing (Hash, add, addServerError, application, decodeErrors, get, pathDecoder, pathEncoder, post, put, settings, storePathWith, storeSettings, viewerChanges)

{-| This module is responsible for communicating to the IPFS API.

It exposes an opaque Endpoint type which is guaranteed to point to the correct URL.

-}

import Api.Endpoint as Endpoint exposing (Endpoint)
import Avatar exposing (Avatar)
import Browser
import Browser.Navigation as Nav
import Bytes
import Bytes.Encode
import Http exposing (Body, Expect)
import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, string)
import Json.Decode.Pipeline as Pipeline exposing (optional, required, requiredAt)
import Json.Encode as Encode
import Result exposing (Result)
import Route exposing (Path)
import Task
import Url exposing (Url)
import Username exposing (Username)


type alias Hash =
    String



-- PERSISTENCE


cidDecoder : Decode.Decoder Hash
cidDecoder =
    Decode.at [ "Cid", "/" ] Decode.string


port onStoreChange : (Value -> msg) -> Sub msg


viewerChanges : (Path -> msg) -> Decoder Path -> Sub msg
viewerChanges toMsg decoder =
    onStoreChange (\value -> toMsg (decodeFromChange decoder value))


decodeFromChange : Decoder Path -> Value -> Path
decodeFromChange dec val =
    case Decode.decodeValue dec val of
        Ok path ->
            case path.cid of
                "" ->
                    { path | cid = "zdpuB3bAS3AsWGeVa8mUfVkzrDTzKL4NX1yDFafxa9DkPwLFL" }

                x ->
                    path

        Err _ ->
            { cid = "zdpuB3bAS3AsWGeVa8mUfVkzrDTzKL4NX1yDFafxa9DkPwLFL", location = [] }


storePathWith : Path -> Avatar -> Cmd msg
storePathWith path avatar =
    let
        json =
            Encode.object
                [ ( "path"
                  , Encode.object
                        [ ( "cid", Encode.string path.cid )
                        , ( "path", Encode.list Encode.string path.location )
                        , ( "image", Avatar.encode avatar )
                        ]
                  )
                ]
    in
    storeCache (Just json)


storeSettings : Path -> Cmd msg
storeSettings path =
    let
        json =
            Encode.object
                [ ( "path"
                  , Encode.object
                        [ ( "cid", Encode.string path.cid )
                        , ( "location", Encode.list Encode.string path.location )
                        ]
                  )
                ]
    in
    storeCache (Just json)


port storeCache : Maybe Value -> Cmd msg



-- SERIALIZATION


pathDecoder : Decoder Path
pathDecoder =
    Decode.succeed Path
        |> requiredAt [ "path", "cid" ] Decode.string
        |> requiredAt [ "path", "location" ] (Decode.list Decode.string)


pathEncoder : Path -> Encode.Value
pathEncoder path =
    Encode.object
        [ ( "cid", Encode.string path.cid )
        , ( "location", Encode.list Encode.string path.location )
        ]



-- APPLICATION


application :
    Decoder Path
    ->
        { init : Path -> Url -> Nav.Key -> ( model, Cmd msg )
        , onUrlChange : Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        , subscriptions : model -> Sub msg
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        }
    -> Program Value model msg
application dec config =
    let
        init flags url navKey =
            let
                path =
                    decodeFromChange pathDecoder flags
            in
            config.init path url navKey
    in
    Browser.application
        { init = init
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        , subscriptions = config.subscriptions
        , update = config.update
        , view = config.view
        }



-- HTTP


get : Endpoint -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
get url trigger decoder =
    Endpoint.request
        { method = "GET"
        , url = url
        , expect = Http.expectJson trigger decoder
        , headers = []
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


post : Endpoint -> Body -> (Result Http.Error Hash -> msg) -> Cmd msg
post url body trigger =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = Http.expectJson trigger cidDecoder
        , headers = []
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }


add : Http.Body -> Http.Resolver x a -> Task.Task x a
add body resolver =
    Endpoint.task
        { method = "POST"
        , headers = []
        , url = Endpoint.add
        , body = body
        , resolver = resolver
        , timeout = Nothing
        }


put : Value -> (Result Http.Error Hash -> msg) -> Cmd msg
put value trigger =
    let
        body =
            Http.multipartBody
                [ turnToBytesPart "whatever" "application/json" value ]
    in
    post Endpoint.dagPut body trigger


settings : Http.Body -> (Result Http.Error Hash -> msg) -> Cmd msg
settings body trigger =
    post Endpoint.dagPut body trigger


turnToBytesPart : String -> String -> Encode.Value -> Http.Part
turnToBytesPart message mime json =
    Encode.encode 0 json
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
        |> Http.bytesPart message mime



-- ERRORS


addServerError : List String -> List String
addServerError list =
    "Server error" :: list



-- Request Helpers


expect : Decode.Decoder a -> Http.Response String -> Result Http.Error a
expect decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Ok value ->
                    Ok value

                Err err ->
                    Err (Http.BadBody (Decode.errorToString err))


decodeErrors : Http.Error -> List String
decodeErrors error =
    case error of
        Http.BadStatus _ ->
            [ "Server error" ]

        err ->
            [ "Server error" ]


errorsDecoder : Decoder (List String)
errorsDecoder =
    Decode.keyValuePairs (Decode.list Decode.string)
        |> Decode.map (List.concatMap fromPair)


fromPair : ( String, List String ) -> List String
fromPair ( field, errors ) =
    List.map (\error -> field ++ " " ++ error) errors



-- LOCALSTORAGE KEYS


urlStorageKey : String
urlStorageKey =
    "url"


settingsStorageKey : String
settingsStorageKey =
    "settings"


cacheStorageKey : String
cacheStorageKey =
    "cache"


credStorageKey : String
credStorageKey =
    "cred"
