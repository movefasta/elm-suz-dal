module Commands exposing (..)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, hardcoded, optional, requiredAt)
import Json.Encode as Encode exposing (Value, object)
import Msgs exposing (Msg)
import Models exposing (..)
import RemoteData
import Result

-- URL's

ipfsApiUrl : String
ipfsApiUrl = 
    "http://localhost:5001/api/v0/"

ipfsGatewayUrl : String
ipfsGatewayUrl = 
    "http://localhost:8080/ipfs/"

-- REQUESTS 

dagGet : Hash -> Cmd Msg
dagGet hash =
    Http.getString ( ipfsApiUrl ++ "dag/get?arg=" ++ hash )
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.UpdateNode

previewGet : Hash -> Cmd Msg
previewGet hash =
    Http.getString ( ipfsApiUrl ++ "dag/get?arg=" ++ hash )
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.UpdatePreview



-- DECODERS

nodeDecoder : Decode.Decoder Object -> Value -> Result String Object
nodeDecoder objectDecoder value =
    Decode.decodeValue objectDecoder value

{-}
objectDecoder : Decode.Decoder Object
objectDecoder =
    decode Object
        |> optional "data" Decode.string 
        |> required "links" linksDecoder
-}

linksDecoder : Decode.Decoder (List Link)
linksDecoder =
    Decode.field "links" <| Decode.list linkDecoder

linkDecoder : Decode.Decoder Link
linkDecoder =
    decode Link
        |> required "Name" Decode.string
        |> required "Size" Decode.int
        |> requiredAt ["Cid", "/"] Decode.string
        |> optional "Description" Decode.string ""
        |> hardcoded Completed

cidDecoder : Decode.Decoder Cid
cidDecoder =
    decode Cid
        |> required "/" Decode.string

-- ENCODERS

objectEncoder : Data -> List Link -> Value
objectEncoder data list =
    Encode.object
        [ ("data", Encode.string data)
        , ("links", Encode.list <| List.map linkEncoder list)
        ]

linkEncoder : Link -> Value
linkEncoder link =
    Encode.object
        [ ("Name", Encode.string link.name)
        , ("Size", Encode.int link.size)
        , ("Cid", Encode.object [ ( "/", Encode.string link.cid ) ] )
        , ("Description", Encode.string link.description)
        ]

-- HELPERS

pathUpdate : Node -> List Node -> List Node -> List Node
pathUpdate node acc path =
    case path of
        x :: xs -> 
            case x == node of
                True -> 
                    acc ++ [x]
                False -> 
                    pathUpdate node (acc ++ [x]) xs 
        [] -> acc ++ [node]


addLink :  Int -> Hash -> Name -> Link
addLink id cid name =
    { name = name
    , size = 0
    , cid = cid
    , description = ""
    , status = Completed
    }

{-

pathForUrl : List Node -> String
pathForUrl path = 
    List.foldr (\( name, hash ) list -> (name ++ "/") ++ list) "" path

multihashDecoder : Decode.Decoder MultihashFromJS
multihashDecoder =
    decode MultihashFromJS
        |> required "type" Decode.string
        |> required "data" uint8arrayDecoder

uint8arrayDecoder : Decode.Decoder (List Int)
uint8arrayDecoder =
    Decode.list Decode.int

onlyHashDecoder : Decode.Decoder Hash
onlyHashDecoder =
    Decode.field "Hash" Decode.string

objectModifiedDecoder : Decode.Decoder ModifiedObject
objectModifiedDecoder =
    decode ModifiedObject
        |> required "Hash" Decode.string
        |> required "Links" linksDecoder

headerDecoder : Decode.Decoder ModifiedObject
headerDecoder =
    decode ModifiedObject
        |> required "Ipfs-Hash" Decode.string
        |> hardcoded []
-}

-- ДОСТОЯНИЕ ИСТОРИИ

{-

setData : String -> Hash -> Cmd Msg
setData data hash =
    put ( ipfsGatewayUrl ++ hash ) data
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.GetIpfsHash


removeLink : Hash -> Link -> Cmd Msg
removeLink hash link =
    Http.get ( ipfsApiUrl 
        ++ "object/patch/rm-link?arg=" ++ hash 
        ++ "&arg=" ++ link.name ) onlyHashDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.GetModifiedObject


addLink : Hash -> Name -> Hash -> Cmd Msg
addLink node_hash name link_hash =
    Http.get ( ipfsApiUrl
        ++ "object/patch/add-link?arg=" ++ node_hash 
        ++ "&arg=" ++ link_hash 
        ++ "&arg=" ++ name ) onlyHashDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.GetModifiedObject

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

-}

