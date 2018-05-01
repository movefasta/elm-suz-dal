module Commands exposing (..)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, hardcoded)
import Msgs exposing (Msg)
import Models exposing (Hash, Object, ModifiedObject, Link)
import RemoteData
import Result
import Dict


-- REQUESTS 

ipfsApiUrl : String
ipfsApiUrl = 
    "http://localhost:5001/api/v0/"

ipfsGatewayUrl : String
ipfsGatewayUrl = 
    "http://localhost:8080/ipfs/"


getPureData : Hash -> Cmd Msg
getPureData hash =
    Http.getString ( ipfsGatewayUrl ++ hash )
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.UpdatePureData


getObject : Hash -> Cmd Msg
getObject hash =
    Http.get (ipfsApiUrl ++ "object/get?arg=" ++ hash) objectDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.GetObject


setData : String -> Hash -> Cmd Msg
setData data hash =
    put ( ipfsGatewayUrl ++ hash ) data
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.GetIpfsHash

removeLink : Hash -> Link -> Cmd Msg
removeLink hash link =
    Http.get ( ipfsApiUrl ++ "object/patch/rm-link?arg=" ++ hash ++ "&arg=" ++ link.name ) onlyHashDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.GetModifiedObject

{-
get : Hash -> Cmd Msg
get hash =
    Http.getString (ipfsApiUrl ++ "get?arg=" ++ hash)
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.Download
-}

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


getHeader : Http.Response String -> Result Hash String
getHeader response =
    Dict.get "ipfs-hash" (Debug.log "headers" response.headers)
        |> Result.fromMaybe ("ipfs header not found")


stringtoBody : String -> Http.Body
stringtoBody value =
    Http.stringBody "text/plain" value


-- DECODERS

onlyHashDecoder : Decode.Decoder Hash
onlyHashDecoder =
    Decode.field "Hash" Decode.string


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

