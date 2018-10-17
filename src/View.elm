module View exposing (..)

import Ports

-- ELM CORE LIBS
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, hardcoded, optional, requiredAt)
import Json.Encode as Encode exposing (Value, object)
import Task exposing (Task)
import Result
import Html exposing (Html)
import Html.Attributes
-- import Dom

-- STYLISH ELEPHANTS
import Element as E exposing (..)
import Element.Events as Event
import Element.Input as Input
import Element.Border as Border
import Element.Font as Font exposing (Font)
import Element.Background as Background

-- EXTERNAL LIBRARIES
import Color exposing (Color)
import RemoteData exposing (WebData)
import Material.Icons.Image as Icon exposing (edit)
import Svg exposing (svg)
import DropZone exposing (dropZoneEventHandlers, isHovering)
import FileReader exposing (Error(..), FileRef, NativeFile, readAsTextFile, filePart)
import Json.Decode.Extra as DecodeExtra exposing (parseInt)
import MimeType as Mime exposing (MimeType, MimeText)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)

-- CONSTANTS

ipfsApiUrl : String
ipfsApiUrl = 
    "http://localhost:5001/api/v0/"

ipfsGatewayUrl : String
ipfsGatewayUrl = 
    "http://localhost:8080/ipfs/"

defaultNode : Node
defaultNode =
    { name = "ROOT"
    , cid = "QmaaWo8DMGGkdVvMFz7Ld4DzdeyhRHNGy2aBpM7TcCKWLu"
    , size = 0
    , title = "THERE IS NO NODE"
--    , children = Children []
--    , parent = Nothing
--    , content = []
    }

type alias Model =
    { root : Hash -- query hash (root)
    , data : Data -- some data to send
    , dag : Zipper Node
    , raw_dag : String -- daw dag for response debugging
    , dropZone :
        DropZone.Model
    , files_to_add : List NativeFile
    , content : List File -- file list
    , tree : Tree Node
--    , path : List Node -- dag nodes path
    }

type Status
    = Editing
    | Completed
    | Selected

type alias Node =
        { name : String
        , cid : String
        , size : Int
        , title : String
--        , children : Children
--        , parent : Maybe Parent
--        , content : List File
        }

type Children = Children (List Node)
type Parent = Parent Node

type alias File =
    { name : String
    , cid : String
    , size : Int
    , mimetype : MimeType
    , status : Status
    }

-- OBJECT API TYPES FOR HANDLING IPFS RESPONSES

type alias Object =
    { links : List Node
    , data : String
    }

type alias ModifiedObject =
    { hash : Hash
    , links : List Link
    }

type alias Link =
    { name : String
    , cid : String
    , size : Int
    }

type alias Hash = String
type alias Data = String

type Msg 
    = NoOp
    | UpdateQuery String
    | UpdateData String
    | GetNode Hash
    | ObjectPut Value
    | UpdateNode (Result Http.Error Object)
    | UpdateZipper (Result Http.Error (Tree Node))
    | UpdateRawDag (Result Http.Error String)
    | AddFile (Result Http.Error Link)
    | AddText String
--    | GetModifiedObject (WebData Hash)
--    | DraftUpdate (Result Http.Error (List Link))
--    | PatchObjectUpdate (Result Http.Error Hash)
--    | FileCat Hash
    | DnD (DropZone.DropZoneMessage (List NativeFile))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        defaultFile =
            { name = "defaultLink"
            , size = 0
            , cid = ""
            , mimetype = Mime.Text Mime.PlainText
            , status = Completed 
            }
    in
    case msg of
        NoOp ->
            model ! []

        UpdateQuery hash ->
            ( { model | root = hash }, Cmd.none )

        UpdateData text ->
            { model | data = text } ! []

        UpdateRawDag response ->
            case response of 
                Ok x ->
                    ({model | raw_dag = x}, Cmd.none)      

                Err _ ->
                    ( model, Cmd.none )
                     
        -- api/v0/object/get?arg=hash
        GetNode hash ->
            ( model, Task.attempt 
                UpdateZipper
                <| getTree 2
                <| Tree.tree { name = "HOME", size = 0, cid = hash, title = ""} []
            )

        AddText text ->
            ( model, addText text )

        ObjectPut value ->
            ( model, Ports.sendData value )

        UpdateZipper result ->
            case result of
                Ok x -> 
                    ({ model | tree = x }, Cmd.none)
                Err _ ->
                    ({ model | raw_dag = "UPDATE ZIPPER TASK FAIL" }, Cmd.none)

        UpdateNode response ->
            case response of
                Ok x ->
                    ( { model | raw_dag = x.data }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        AddFile response ->
            case response of
                Ok link ->
                    let
                        file = File link.name link.cid link.size (Mime.Text Mime.PlainText) Completed
                    in    
                        ( { model | content = ( file :: model.content ) }, Cmd.none )
                Err _ ->
                    ( model, Cmd.none )

        DnD (DropZone.Drop files) ->
            ( { model | dropZone =
                    DropZone.update (DropZone.Drop files) model.dropZone
                , files_to_add = 
                    files
            }, Cmd.none
--            , addFiles files
            )

        DnD a ->
            ( { model | dropZone = DropZone.update a model.dropZone }, Cmd.none )

--VIEW

view : Model -> Html Msg
view model =
    layoutWith { options = 
        [ focusStyle 
            { borderColor = Nothing
            , backgroundColor = Nothing
            , shadow = Nothing 
            } ] } [] <|
        column
            [ Font.size 12
            , spacing 10
            , padding 10
            , width fill
            ]
            [ viewControls model
            , row [ spacing 5 ] []
            , column
                [] 
                [ viewTable model.tree ]
            , column
                [ spacing 10 ]
                [ column [] <| List.map viewFile model.content -- Show Files
                , renderDropZone model.dropZone
                , viewTree model.tree
                ]
            ]

viewFile : File -> Element Msg
viewFile file =
    let
        link_src =
            { url = ("/ipfs/" ++ file.cid)
            , label = text file.cid
            }
    in
    column [] <| 
        [ image []
            { src = link_src.url
            , description = "it's not an image" 
            }
        , el
            [ padding 2
            , Event.onClick <| NoOp
                -- FileCat file.cid
            ]
          <| text "cat_file"
        ]

viewRawDag : Data -> Element Msg
viewRawDag raw_dag =
    el 
        [ Font.size 10
        , htmlAttribute <| Html.Attributes.style [ ( "overflow-wrap", "break-word" ) ] 
        ] <| 
        paragraph [ padding 5 ] [ text raw_dag ]

viewCell : Tree Node -> Color -> Element Msg
viewCell node color =
    el
        [ Background.color color
        , padding 10
        , htmlAttribute <| Html.Attributes.style [ ( "overflow-wrap", "break-word" ) ]
        , pointer
        , Font.size 13
        , Font.bold
        , width fill
        ] <| el [ centerX, centerY ] <| text <| .name <| Tree.label node

viewSector : Tree Node -> Color -> Element Msg
viewSector tree color =
    let
        childMap child =
            case ( String.toInt <| .name <| Tree.label child ) of
                Ok int ->
                    row
                        [ spacing 5
                        , padding 5
                        , width fill
                        ] <| [ el [ padding 10, centerX, centerY, width fill ] <| text <| .name <| Tree.label child ] ++
                            (List.map
                                (\x -> 
                                    viewCell x <| ( if int == 0 then white 1.0 else color )
                                ) <| Tree.children child
                            )
                Err _ ->
                    none
    in
    column 
        [ Border.width 1
        , Border.color Color.darkGrey
        ] 
        [ el 
            [ Font.size 12
            , centerX
            , centerY
            , width fill
            ] <|
            text <| .title <| Tree.label tree
        , column [] <| List.map childMap <| Tree.children tree
        ]

viewTable : Tree Node -> Element Msg
viewTable tree =
    let
        alpha =
            1.0

        color i =
            case i of
                0 -> yellow
                1 -> orange
                2 -> violet
                3 -> cyan
                4 -> blue
                5 -> green
                _ -> white

        childMap child =
            case ( String.toInt <| .name <| Tree.label child ) of
                Ok int ->
                    viewSector child <| color int <| alpha
                Err _ ->
                    none
    in
    column [ spacing 5 ] <| List.map childMap <| Tree.children tree

viewTree : Tree Node -> Element Msg
viewTree tree =
    let
        style =
            [ padding 5
            , Border.width 1
            , Border.color Color.darkGrey
            , mouseOver <| [ Background.color Color.lightGrey ]
            ]
    in
    column [] <|
        [ el style <| text <| .name <| Tree.label tree ] ++
        (List.map 
                (\x ->
                    case (Tree.children x) of
                        [] -> el style <| text <| .name <| Tree.label x
                        _ -> viewTree x
                ) <| Tree.children tree
            )



-- navigation in breadcrumbs

viewNodeinPath : Node -> Element Msg
viewNodeinPath node =
    Input.button 
        [ padding 5
        , mouseOver <| [ Background.color Color.lightGrey ]
        ]
        { label = text (node.name ++ " > ")
        , onPress = Just <| GetNode node.cid
        }

viewControls : Model -> Element Msg
viewControls model =
    let
        style  =
            [ padding 5
            , Border.width 1
            , Border.color Color.darkGrey
            , mouseOver <| [ Background.color Color.lightGrey ]
            ]
     in
        row
        [ spacing 5, width fill ]
        [ Input.text
            [ padding 5
            , Event.onLoseFocus <| GetNode model.root
            ]
            { onChange = Just UpdateQuery
            , text = model.root
            , placeholder = Just <| Input.placeholder [] <| text "Enter query hash here"
            , label = Input.labelLeft [ padding 5 ] <| text "Root Hash"
            }
        , Input.button
            style
            { onPress = Just NoOp
                -- Just <| ObjectPut <| objectEncoderPB model.data <| Zipper.children model.dag
            , label = text "obj put"
            }
        , Input.button
            style
            { onPress = Just <| NoOp 
            , label = text "empty"
            }
        , Input.button
            style
            { onPress = Just <| NoOp 
            , label = text "empty"
            }
        , Input.button
            style
            { onPress = Just <| NoOp 
            , label = text "empty"
            }
        ]

viewData : Data -> Element Msg
viewData data =
    column
        [ Font.color Color.charcoal
        , Background.color Color.white
        , Font.family fontFamily      
        ]
        [ Input.multiline
            [ padding 5 ]
            { onChange = Just UpdateData
            , placeholder = Just <| Input.placeholder [] <| text "Data" 
            , label = Input.labelLeft [] <| text "Data" 
            , text = "text field"
            , spellcheck = False
            }
        ]

editIcon : Link -> Element Msg
editIcon link =
    el
        [ pointer
        , padding 5
        , Event.onClick <| NoOp
        ] 
        <| html 
        <| Svg.svg 
            [ Html.Attributes.width 18
            , Html.Attributes.height 18
            ]
            [ Icon.edit Color.black 18 ]

renderDropZone : DropZone.Model -> Element Msg
renderDropZone dropZoneModel =
    map DnD (el (renderZoneAttributes dropZoneModel) <| 
        el
            [ Font.size 20
            , centerX
            , centerY
            , Font.color Color.white
            , Font.bold ]
            <| text "DRAG HERE" )

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
            |> Http.send AddFile

-- BUNCH OF REQUESTS FOR ADDING FILES
-- [ add files, patch current node, patch path, patch root node ]

{-}
addFileRequest : NativeFile -> Task.Task Http.Error Link
addFileRequest nf =
    let
        body =
            Http.multipartBody [ FileReader.filePart nf.name nf ]
    in
        Http.post (ipfsApiUrl ++ "add") body fileLinkDecoder
            |> Http.toTask

addFiles : List NativeFile -> Cmd Msg
addFiles nf_list =
    (List.map (\file -> addFileRequest file) nf_list |> Task.sequence)
        |> Task.attempt DraftUpdate
        
addLinkRequest : Hash -> String -> Hash -> Task.Task Http.Error Hash
addLinkRequest parent name cid =
    Http.get ( ipfsApiUrl
                    ++ "object/patch/add-link?arg=" ++ parent
                    ++ "&arg=" ++ name
                    ++ "&arg=" ++ cid )
                (Decode.field "Hash" Decode.string)
                |> Http.toTask

patchObject : Node -> Hash -> Task.Task Http.Error Hash
patchObject node new_cid =
    case node.parent of
        Just (Parent parent) ->
            addLinkRequest parent.cid node.name new_cid
                |> Task.andThen (\hash -> patchObject parent hash)
        Nothing -> 
            Task.succeed node.cid



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
-}


-- IPFS OBJECT API REQUESTS

getNode : Node -> Cmd Msg
getNode node =
    Http.get ( ipfsApiUrl ++ "object/get?arg=" ++ node.cid ) objectDecoder
        |> Http.send UpdateNode

getNodeTask : Node -> Task Http.Error (Tree Node)
getNodeTask node =
    let
        updateTitle =
            (\x data -> Tree.tree { x | title = data } [])
    in
    Http.get ( ipfsApiUrl ++ "object/get?arg=" ++ node.cid ) objectDecoder
        |> Http.toTask
        |> Task.andThen
            (\object ->  
                object.links
                    |> List.map
                        (\link -> 
                            getData (Debug.log "get data from" link)
                            |> Task.map2 updateTitle (Task.succeed link)
                        )
                    |> Task.sequence
                    |> Task.andThen
                        (\links_list ->
                            Task.succeed <| Tree.tree { node | title = object.data } links_list  
                        )
            )

getTree : Int -> Tree Node -> Task Http.Error (Tree Node)
getTree depth tree =
    case ((Debug.log "depth level" depth) == 0) of
        True ->
            Task.succeed tree
        False ->
            Tree.label tree
            |> getNodeTask
            |> Task.andThen
                (\x -> 
                    Tree.children x
                    |> List.map 
                        (\child ->
                            getNodeTask (Tree.label child)
                            |> Task.andThen (getTree (depth - 1))
                        )
                    |> Task.sequence
                )
            |> Task.andThen (\list -> Task.succeed <| Tree.replaceChildren list tree )

getData : Node -> Task Http.Error Data
getData node =
    Http.getString ( ipfsApiUrl ++ "object/data?arg=" ++ node.cid )
        |> Http.toTask

dataDecoder : Decode.Decoder String
dataDecoder =
    Decode.field "Data" Decode.string

{-}
removeLink : Node -> Cmd Msg
removeLink node =
    Http.get ( ipfsApiUrl 
        ++ "object/patch/rm-link?arg=" ++ (case node.parent of Just parent -> parent.cid Nothing) 
        ++ "&arg=" ++ link.name ) onlyHashDecoder
        |> RemoteData.sendRequest
        |> Cmd.map GetModifiedObject
addLink : Hash -> String -> Hash -> Cmd Msg
addLink node_hash name link_hash =
    Http.get ( ipfsApiUrl
        ++ "object/patch/add-link?arg=" ++ node_hash 
        ++ "&arg=" ++ link_hash 
        ++ "&arg=" ++ name ) onlyHashDecoder
        |> RemoteData.sendRequest
        |> Cmd.map GetModifiedObject
-}

-- DECODERS

nativeFileDecoder : Decode.Decoder NativeFile
nativeFileDecoder =
    Decode.map4 NativeFile
        (Decode.field "name" Decode.string)
        (Decode.field "size" Decode.int)
        mtypeDecoder
        Decode.value

mtypeDecoder : Decode.Decoder (Maybe MimeType)
mtypeDecoder =
    Decode.map Mime.parseMimeType (Decode.field "type" Decode.string)

fileLinkDecoder : Decode.Decoder Link
fileLinkDecoder =
    decode Link
        |> required "Name" Decode.string
        |> required "Hash" Decode.string
        |> required "Size" sizeDecoder

nodeDecoder : Decode.Decoder Node
nodeDecoder =    
    decode Node
        |> required "Name" Decode.string
        |> required "Hash" Decode.string
        |> required "Size" Decode.int
        |> optional "Title" Decode.string ""

objectDecoder : Decode.Decoder Object
objectDecoder =
    decode Object
        |> required "Links" (Decode.list nodeDecoder)
        |> required "Data" Decode.string 

pbLinkDecoder : Decode.Decoder Link
pbLinkDecoder =
    decode Link
        |> required "Name" Decode.string
        |> required "Hash" Decode.string
        |> required "Size" Decode.int

sizeDecoder : Decode.Decoder Int
sizeDecoder =
    Decode.oneOf [ Decode.int, DecodeExtra.parseInt ]

onlyHashDecoder : Decode.Decoder Hash
onlyHashDecoder =
    Decode.field "Hash" Decode.string

objectModifiedDecoder : Decode.Decoder ModifiedObject
objectModifiedDecoder =
    decode ModifiedObject
        |> required "Hash" Decode.string
        |> required "Links" (Decode.list pbLinkDecoder)


-- ENCODERS

objectEncoderPB : Data -> List Node -> Value
objectEncoderPB data list =
    Encode.object
        [ ("Data", Encode.string data)
        , ("Links", Encode.list <| List.map linkEncoderPB list)
        ]

dagNodePbEncoder : List Link -> List Value
dagNodePbEncoder list =
    List.map linkApiEncoderPB list

linkEncoderPB : Node -> Value
linkEncoderPB node =
    Encode.object
        [ ("name", Encode.string node.name)
        , ("size", Encode.int node.size)
        , ("multihash", Encode.string node.cid )
        ]

objectApiEncoder : Data -> List Node -> Value
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

maybeRemote : (a -> Element Msg) -> WebData a -> Element Msg
maybeRemote viewFunction response =
    case response of
        RemoteData.NotAsked ->
            text ""
        RemoteData.Loading ->
            text "Loading..."
        RemoteData.Success object ->
            viewFunction object
        RemoteData.Failure error ->
            text (toString error)

renderZoneAttributes : DropZone.Model -> 
    List (Attribute (DropZone.DropZoneMessage (List NativeFile)))
renderZoneAttributes dropZoneModel =
    (if DropZone.isHovering dropZoneModel then
        renderStyle Color.darkGrey
        -- style the dropzone differently depending on whether the user is hovering
    else
        renderStyle Color.lightGrey
    )
        ++ -- add the necessary DropZone event wiring
           ( List.map htmlAttribute <| dropZoneEventHandlers FileReader.parseDroppedFiles )

renderStyle : Color -> List (Attribute a)
renderStyle color =
    [ width fill
    , height fill
    , Border.width 1
    , Border.color color
    , Border.dashed
    ]


-- COLOR PROPERTIES

white : Float -> Color
white alpha =
    Color.rgba 255 255 255 alpha

orange : Float -> Color
orange alpha = 
    Color.rgba 255 122 0 alpha

yellow : Float -> Color
yellow alpha = 
    Color.rgba 255 214 0 alpha

green : Float -> Color
green alpha = 
    Color.rgba 152 237 0 alpha

cyan : Float -> Color
cyan alpha = 
    Color.rgba 2 142 155 alpha

blue : Float -> Color
blue alpha = 
    Color.rgba 62 19 175 alpha

violet : Float -> Color
violet alpha = 
    Color.rgba 210 0 107 alpha

-- FONTS

fontFamily : List Font
fontFamily =
    [ Font.typeface "Source Sans Pro" 
    , Font.typeface "Trebuchet MS"
    , Font.typeface "Lucida Grande"
    , Font.typeface "Bitstream Vera Sans"
    , Font.typeface "Helvetica Neue"
    , Font.typeface "sans-serif"
    ]


{-


        UpdateFiles response ->
            case response of
                Ok links ->
                    ( { model | content = model.content ++ links }, 
                        Task.attempt PatchObjectUpdate <| patchObject model.link.cid links )
                Err error ->
                    ( { model | data = Basics.toString <| error }, Cmd.none )


        GetModifiedObject response ->
            let
                ipfs_hash = RemoteData.withDefault "" response
            in
                ( { model | hash = ipfs_hash }, dagGet ipfs_hash )


findLinkByName : String -> WebData Object -> Maybe Link
findLinkByName link_name object =
    case RemoteData.toMaybe object of
        Just object ->
            object.links
                |> List.filter (\link -> link.name == link_name)
                |> List.head

        Nothing ->
            Nothing

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

fileCat : Hash -> Cmd Msg
fileCat hash =
    Http.getString ( ipfsApiUrl ++ "cat?arg=" ++ hash )
        |> RemoteData.sendRequest
        |> Cmd.map FileGet


nodeProperties : Node -> Element Msg
nodeProperties node =
    let
        div =
            case link.status of 
                Editing ->
                    Input.text
                        [ padding 5
                        , htmlAttribute <| Html.Attributes.id ("link-" ++ link.name)
                        , Event.onLoseFocus <| AddText link Completed
                        , Font.size 20
                        ]
                        { onChange = Just UpdateData
                        , text = model.data
                        , placeholder = Just <| Input.placeholder [] <| text "Текст"
                        , label = Input.labelLeft [] <| text "Editing"
                        }
                Completed -> 
                    el [] <| text link.name
    in
        column 
            [ spacing 5 ]
            [ el [ Event.onClick <| UpdateLink link Editing ] <| div
            , el [] <| text link.cid
            ]



        PatchObjectUpdate response ->
            let
                draft_link = 
                    (model.link.name, model.link.cid)
            in
            case response of
                Ok hash ->
                    ( model, Cmd.batch [ lsObjects hash,
                        case List.head model.path == Just draft_link of
                            True ->
                                Task.attempt PathPatchUpdate 
                                    <| patchPath [(model.link.name, hash)] model.path hash
                            False ->
                                Task.attempt PathPatchUpdate
                                    <| patchPath [(model.link.name, hash)] ( [ draft_link ] ++ model.path ) hash
                                     ] )
                Err error ->
                    ( { model | data = Basics.toString <| error }, Cmd.none )


        UpdateNode response ->
            let
                object =
                    RemoteData.withDefault "response fails" response
                
                newNode =
                    Result.withDefault [ defaultLink ] <| Decode.decodeString objectDecoder object
            in
                ( { model | node = newNode, draft = newNode, raw_dag = response }, Cmd.none )

        UpdateLink link status ->
            let
                updateNodeLinksList x =
                    if x.name == link.name then
                        { link | status = status }
                    else
                        x

                updateLinkStatus =
                    { link | status = status }

                cmd =
                    case status of 
                        Editing ->
                            Task.attempt (\_ -> NoOp) <| Dom.focus ("link-" ++ link.name)
                        Completed ->
                            Ports.sendData <| objectEncoderPB model.data model.node
            in
                { model | node = List.map updateNodeLinksList model.node, link = updateLinkStatus }
                    ! [ cmd ]



-}