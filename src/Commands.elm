module Commands exposing (..)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, hardcoded, optional, requiredAt)
import Json.Encode as Encode exposing (Value, object)
import Msgs exposing (Msg)
import RemoteData
import Result
import FileReader exposing (NativeFile, filePart)
import Task
import Json.Decode.Extra as DecodeExtra exposing (parseInt)
import MimeType

-- URL's

ipfsApiUrl : String
ipfsApiUrl = 
    "http://localhost:5001/api/v0/"

ipfsGatewayUrl : String
ipfsGatewayUrl = 
    "http://localhost:8080/ipfs/"

-- REQUESTS 

addText : String -> Cmd Msg
addText text =
    let
        body =
            Http.multipartBody
            [ Http.stringPart "textpart" text
            ]
    in
        Http.post (ipfsApiUrl ++ "add") body fileLinkDecoder
            |> RemoteData.sendRequest
            |> Cmd.map Msgs.AddLink

fileCat : Hash -> Cmd Msg
fileCat hash =
    Http.getString ( ipfsApiUrl ++ "cat?arg=" ++ hash )
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.FileGet

-- BUNCH OF REQUESTS FOR ADDING FILES
-- [ add files, patch current node, patch path, patch root node ]

addFileRequest : NativeFile -> Task.Task Http.Error Link
addFileRequest nf =
    let
        body =
            Http.multipartBody [ FileReader.filePart nf.name nf ]
    in
        Http.post (ipfsApiUrl ++ "add") body fileLinkDecoder
            |> Http.toTask

addFiles : List NativeFile -> Link -> Path -> Cmd Msg
addFiles nf_list object path =
    (List.map (\file -> addFileRequest file) nf_list |> Task.sequence)
        |> Task.attempt Msgs.DraftUpdate
        
addLinkRequest : Hash -> Link -> Task.Task Http.Error Hash
addLinkRequest parent_hash link =
    Http.get ( ipfsApiUrl
            ++ "object/patch/add-link?arg=" ++ parent_hash
            ++ "&arg=" ++ link.name
            ++ "&arg=" ++ link.cid )
            (Decode.field "Hash" Decode.string)
        |> Http.toTask

patchObject : Hash -> List Link -> Task.Task Http.Error Hash
patchObject parent links =
    case links of
        x :: xs ->
            addLinkRequest parent x
                |> Task.andThen (\hash -> patchObject hash xs)
        [] -> 
            Task.succeed parent

patchPath : Path -> Path -> Hash -> Task.Task Http.Error Path
patchPath acc path hash =
    case path of
        (childname, childhash) :: (parentname, parenthash) :: xs ->
            addLinkRequest parenthash 
                { name = childname, size = 0, cid = hash, obj_type = 2, status = Completed }
                |> Task.andThen
                    (\hash -> 
                        patchPath (acc ++ [(parentname, hash)]) ((parentname, parenthash) :: xs) hash )
        x :: [] -> 
            Task.succeed acc
        [] ->
            Task.succeed acc

-- IPFS OBJECT API REQUESTS

getObject : Hash -> Cmd Msg
getObject hash =
    Http.get ( ipfsApiUrl ++ "object/get?arg=" ++ hash ) objectDecoder 
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.UpdateNode

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


-- DECODERS

nativeFileDecoder : Decode.Decoder NativeFile
nativeFileDecoder =
    Decode.map4 NativeFile
        (Decode.field "name" Decode.string)
        (Decode.field "size" Decode.int)
        mtypeDecoder
        Decode.value

mtypeDecoder : Decode.Decoder (Maybe MimeType.MimeType)
mtypeDecoder =
    Decode.map MimeType.parseMimeType (Decode.field "type" Decode.string)

fileLinkDecoder : Decode.Decoder Link
fileLinkDecoder =
    decode Link
        |> required "Name" Decode.string
        |> required "Size" sizeDecoder
        |> required "Hash" Decode.string
        |> hardcoded Completed

sizeDecoder : Decode.Decoder Int
sizeDecoder =
    Decode.oneOf [ Decode.int, DecodeExtra.parseInt ]

objectDecoder : Decode.Decoder Object
objectDecoder =
    decode Object
        |> optional "data" Decode.string 
        |> required "links" linksDecoder

linksDecoder : Decode.Decoder (List Link)
linksDecoder =
    Decode.field "links" <| Decode.list pbLinkDecoder

pbLinkDecoder : Decode.Decoder Link
pbLinkDecoder =
    decode Link
        |> required "Name" Decode.string
        |> required "Size" Decode.string
        |> required "Hash" Decode.string
        |> hardcoded Completed

onlyHashDecoder : Decode.Decoder Hash
onlyHashDecoder =
    Decode.field "Hash" Decode.string

objectModifiedDecoder : Decode.Decoder ModifiedObject
objectModifiedDecoder =
    decode ModifiedObject
        |> required "Hash" Decode.string
        |> required "Links" linksDecoder


-- ENCODERS

objectEncoderPB : Data -> List Link -> Value
objectEncoderPB data list =
    Encode.object
        [ ("Data", Encode.string data)
        , ("Links", Encode.list <| List.map linkEncoderPB list)
        ]

dagNodePbEncoder : List Link -> List Value
dagNodePbEncoder list =
    List.map linkApiEncoderPB list

linkEncoderPB : Link -> Value
linkEncoderPB link =
    Encode.object
        [ ("name", Encode.string link.name)
        , ("size", Encode.int link.size)
        , ("multihash", Encode.string link.cid )
        ]

objectApiEncoder : Data -> List Link -> Value
objectApiEncoder data list =
    Encode.object
        [ ("Data", Encode.string data)
        , ("Links", Encode.list <| List.map linkEncoderPB list)
        ]

linkApiEncoderPB : Link -> Value
linkApiEncoderPB link =
    Encode.object
        [ ("Name", Encode.string link.name)
        , ("Size", Encode.int link.size)
        , ("Hash", Encode.string link.cid)
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

pathStackUpdate : Node -> List Node -> List Node
pathStackUpdate node path =
    case path of
        x :: xs -> 
            case x == node of
                True -> 
                    path
                False -> 
                    pathStackUpdate node xs 
        [] -> [node] ++ path

pathToUrl : List Node -> String
pathToUrl path = 
    List.foldr (\( name, hash ) list -> (name ++ "/") ++ list) "" path


{-
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
        ]

nodeDecoder : Decode.Decoder Object -> Value -> Result String Object
nodeDecoder objectDecoder value =
    Decode.decodeValue objectDecoder value

objectsDecoder : Decode.Decoder (List Link)
objectsDecoder = 
    Decode.field "Objects" <| Decode.index 0 <| Decode.field "Links" <| Decode.list fileLinkDecoder

objectHashDecoder : Decode.Decoder String
objectHashDecoder =
    Decode.field "Objects" <| Decode.index 0 <| Decode.field "Hash" Decode.string

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

dagLinkDecoder : Decode.Decoder Link
dagLinkDecoder =
    decode Link
        |> required "Name" Decode.string
        |> required "Size" Decode.string
        |> requiredAt ["Cid", "/"] Decode.string
        |> optional "Description" Decode.string ""
        |> hardcoded Completed

cidDecoder : Decode.Decoder Cid
cidDecoder =
    decode Cid
        |> required "/" Decode.string


addLink :  Int -> Hash -> Name -> Link
addLink id cid name =
    { name = name
    , size = ""
    , cid = cid
    , description = ""
    , status = Completed
    }

multihashDecoder : Decode.Decoder MultihashFromJS
multihashDecoder =
    decode MultihashFromJS
        |> required "type" Decode.string
        |> required "data" uint8arrayDecoder

uint8arrayDecoder : Decode.Decoder (List Int)
uint8arrayDecoder =
    Decode.list Decode.int


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

