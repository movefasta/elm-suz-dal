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


{-

-- ex-Commands.elm

fetchPlayers : Cmd Msg
fetchPlayers =
    Http.get fetchPlayersUrl playersDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.OnFetchPlayers


fetchPlayersUrl : String
fetchPlayersUrl =
    "http://localhost:4000/players"


savePlayerUrl : PlayerId -> String
savePlayerUrl playerId =
    "http://localhost:4000/players/" ++ playerId


savePlayerRequest : Player -> Http.Request Player
savePlayerRequest player =
    Http.request
        { body = playerEncoder player |> Http.jsonBody
        , expect = Http.expectJson playerDecoder
        , headers = []
        , method = "PATCH"
        , timeout = Nothing
        , url = savePlayerUrl player.id
        , withCredentials = False
        }


savePlayerCmd : Player -> Cmd Msg
savePlayerCmd player =
    savePlayerRequest player
        |> Http.send Msgs.OnPlayerSave



-- DECODERS


playersDecoder : Decode.Decoder (List Player)
playersDecoder =
    Decode.list playerDecoder


playerDecoder : Decode.Decoder Player
playerDecoder =
    decode Player
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> required "level" Decode.int


playerEncoder : Player -> Encode.Value
playerEncoder player =
    let
        attributes =
            [ ( "id", Encode.string player.id )
            , ( "name", Encode.string player.name )
            , ( "level", Encode.int player.level )
            ]
    in
        Encode.object attributes

-}