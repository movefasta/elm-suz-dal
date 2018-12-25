module Main exposing (main)

import Browser exposing (Document, document)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font exposing (Font)
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Http
import Json.Decode as Decode
import Json.Decode.Extra as DecodeExtra exposing (parseInt)
import Json.Decode.Pipeline exposing (hardcoded, optional, required, requiredAt)
import Json.Encode as Encode exposing (Value, object)
import MimeType as Mime exposing (MimeText, MimeType)
import RemoteData exposing (WebData)
import Result
import String.Extra
import Svg exposing (svg)
import Task exposing (Task, attempt)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


main : Program Value Model Msg
main =
    Browser.document
        { init = \_ -> ( initModel, getNode initNode.cid )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- INIT VALUES


initModel : Model
initModel =
    { root = initNode.cid
    , data = ""
    , zipper = Zipper.fromTree <| initTree
    , raw_dag = ""
    , content = []
    , path = [ initNode ]
    }


initNode : Node
initNode =
    { name = "ROOT"
    , cid = "QmPZqBbCpVNQhrCoEWPGEx3ZxAQDCxmiSN216nGs1RHGHN"
    , size = 0
    , title = ""
    , status = Completed
    , id = 0
    }


initTree : Tree Node
initTree =
    Tree.tree initNode
        [ Tree.tree { initNode | name = "CHILD tree" }
            [ Tree.singleton { initNode | name = "grand CHILD1" }
            , Tree.tree { initNode | name = "grand CHILD2" }
                [ Tree.singleton { initNode | name = "grand grand CHILD tree" }
                ]
            ]
        , Tree.singleton { initNode | name = "CHILD node" }
        ]



-- CONSTANTS


ipfsApiUrl : String
ipfsApiUrl =
    "http://localhost:5001/api/v0/"


ipfsGatewayUrl : String
ipfsGatewayUrl =
    "http://localhost:8080/ipfs/"



-- TYPES


type alias Model =
    { root : Hash -- query hash (root)
    , data : String -- some data to send
    , zipper : Zipper Node
    , raw_dag : String -- daw dag for response debugging
    , content : List File -- file list
    , path : List Node -- dag nodes path
    }


type Status
    = Editing
    | Completed
    | Selected


type alias Node =
    { name : String
    , cid : String
    , size : Int
    , title : Data
    , status : Status
    , id : Int
    }


type Children
    = Children (List Node)



-- type Parent = Parent Node


type alias File =
    { name : String
    , cid : String
    , size : Int
    , mimetype : MimeType
    , status : Status
    }



-- OBJECT API TYPES FOR DECODE IPFS RESPONSES


type alias Object =
    { links : List Node
    , data : String
    }


type alias ObjectStat =
    { hash : String
    , numLinks : Int
    , blockSize : Int
    , linksSize : Int
    , dataSize : Int
    , cumulativeSize : Int
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


type alias Hash =
    String


type alias Data =
    String


type Msg
    = NoOp
    | UpdateQuery Hash
    | UpdateData String
    | GetNode Hash
    | ObjectPut Value
    | UpdateZipper (Result Http.Error (Tree Node))
    | AddFile (Result Http.Error Link)
    | AddText String
    | GetPath Node
    | UpdateFocus Node


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        defaultNode hash =
            { name = "defaultLink"
            , cid = hash
            , size = 0
            , title = ""
            , status = Completed
            , id = 0
            }
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateQuery hash ->
            ( { model | root = hash }, Cmd.none )

        UpdateData text ->
            ( { model | data = text }, Cmd.none )

        UpdateFocus node ->
            let
                updatedLabel =
                    { node | title = model.data }
            in
            ( { model | zipper = Zipper.replaceLabel updatedLabel model.zipper }, Cmd.none )

        GetPath node ->
            let
                focusedLabel =
                    Zipper.label model.zipper

                completeFocusedLabel =
                    Zipper.replaceLabel { focusedLabel | status = Completed } model.zipper

                --                    if (focusedLabel.id > node.id) then Zipper.findNext else Zipper.findPrevious
                newZipper =
                    case Zipper.findFromRoot (\x -> x.id == node.id) completeFocusedLabel of
                        Just x ->
                            Zipper.mapLabel
                                (\label ->
                                    if label.id == node.id then
                                        { label | status = Selected }

                                    else
                                        label
                                )
                                x

                        Nothing ->
                            model.zipper
            in
            ( { model
                | path = getPath newZipper []
                , zipper = newZipper
                , data = node.title
              }
            , Cmd.none
            )

        GetNode cid ->
            ( model, getNode cid )

        AddText text ->
            ( model, addText text )

        ObjectPut value ->
            ( model, Cmd.none )

        UpdateZipper result ->
            let
                indexedTree tree =
                    Tree.indexedMap (\idx val -> { val | id = idx }) tree
            in
            case result of
                Ok x ->
                    ( { model | zipper = Zipper.fromTree <| indexedTree x }, Cmd.none )

                Err _ ->
                    ( { model | raw_dag = "UPDATE ZIPPER TASK FAIL" }, Cmd.none )

        AddFile response ->
            case response of
                Ok link ->
                    let
                        file =
                            File link.name link.cid link.size (Mime.Text Mime.PlainText) Completed
                    in
                    ( { model | content = file :: model.content }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



--VIEW


view : Model -> Document Msg
view model =
    { title = "Суз-Даль Сервис v0"
    , body =
        [ layoutWith
            { options =
                [ focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            []
          <|
            column
                [ Font.size 12
                , spacing 10
                , padding 10
                , width fill
                ]
                [ viewControls model
                , viewPath model.path
                , viewDataInput model.data
                , viewTable <| Zipper.toTree <| Zipper.root model.zipper
                ]
        ]
    }


viewTable : Tree Node -> Element Msg
viewTable tree =
    let
        alpha =
            1.0

        color i =
            case i of
                0 ->
                    yellow

                1 ->
                    orange

                2 ->
                    violet

                3 ->
                    cyan

                4 ->
                    blue

                5 ->
                    green

                _ ->
                    white

        childMap child =
            case String.toInt <| .name <| Tree.label child of
                Just int ->
                    viewSector child <| color int <| alpha

                Nothing ->
                    none
    in
    column [ height fill, width fill, spacing 5 ] <| List.map childMap <| Tree.children tree


viewTree : Tree Node -> Element Msg
viewTree tree =
    let
        style =
            [ padding 5
            , Border.width 1
            , Border.color <| darkGrey 1.0
            , mouseOver <| [ Background.color <| lightGrey 1.0 ]
            , width fill
            ]
    in
    column style <|
        [ viewCell tree <| white 1.0 ]
            ++ (List.map
                    (\x ->
                        case Tree.children x of
                            [] ->
                                el [] <| text <| .name <| Tree.label x

                            _ ->
                                viewTree x
                    )
                <|
                    Tree.children tree
               )


viewSector : Tree Node -> Color -> Element Msg
viewSector tree color =
    let
        childMap child =
            case String.toInt <| .name <| Tree.label child of
                Just int ->
                    row
                        [ spacing 5
                        , padding 5
                        , width fill
                        ]
                    <|
                        [ viewCell child <| white 1.0 ]
                            ++ (List.map
                                    (\x ->
                                        viewCell x <|
                                            if int == 0 then
                                                white 1.0

                                            else
                                                color
                                    )
                                <|
                                    Tree.children child
                               )

                Nothing ->
                    none
    in
    column
        [ spacing 5
        , width fill
        ]
        [ viewCell tree <| white 1.0
        , column [] <| List.map childMap <| Tree.children tree
        ]


viewCell : Tree Node -> Color -> Element Msg
viewCell tree color =
    let
        node =
            Tree.label tree
    in
    el
        [ if node.status == Selected then
            Background.color <| lightGrey 1.0

          else
            Background.color color
        , Border.width 1
        , Border.color <| darkGrey 1.0
        , padding 10
        , htmlAttribute <| Html.Attributes.id <| String.concat [ "cell-", String.fromInt node.id ]
        , htmlAttribute <| Html.Attributes.style "overflow-wrap" "break-word"
        , Font.size 13
        , Font.bold
        , Event.onClick <| GetPath node
        , width fill
        ]
    <|
        column
            [ centerY
            , centerX
            , width fill
            ]
            [ el [ centerX ] <| text <| .name node
            , el [ centerX ] <| text <| Debug.toString node.id
            , el [ centerX ] <| text <| .title node
            ]


viewDataInput : Data -> Element Msg
viewDataInput data =
    Input.text
        [ padding 5
        , Event.onLoseFocus <| NoOp
        ]
        { onChange = UpdateData
        , text = data
        , placeholder = Just <| Input.placeholder [] <| text "Введите данные сюда"
        , label = Input.labelLeft [ padding 5 ] <| text ""
        }


viewFile : File -> Element Msg
viewFile file =
    let
        link_src =
            { url = "/ipfs/" ++ file.cid
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
            ]
          <|
            text "cat_file"
        ]


viewRawDag : Data -> Element Msg
viewRawDag raw_dag =
    el
        [ Font.size 10
        , htmlAttribute <| Html.Attributes.style "overflow-wrap" "break-word"
        ]
    <|
        paragraph [ padding 5 ] [ text raw_dag ]


toggleIfTarget : Int -> Int -> Node -> Node
toggleIfTarget id index node =
    if id == index then
        { node | status = Selected }

    else
        { node | status = Completed }



-- navigation in breadcrumbs


viewPath : List Node -> Element Msg
viewPath list =
    row [ width fill ] <| List.map viewNodeinPath list


viewURL : List Node -> Element Msg
viewURL list =
    case list of
        x :: xs ->
            row [] <| [ text <| .cid x ] ++ List.map (\node -> text <| "/" ++ .name node) xs

        [] ->
            el [] <| text "NO PATH"


viewNodeinPath : Node -> Element Msg
viewNodeinPath node =
    Input.button
        [ padding 5
        , mouseOver <| [ Background.color <| darkGrey 1.0 ]
        ]
        { label = text (node.name ++ "(id: " ++ Debug.toString node.id ++ ")/")
        , onPress = Just <| GetNode node.cid
        }


getPath : Zipper Node -> List Node -> List Node
getPath zipper acc =
    let
        appendLabel =
            Zipper.label zipper :: acc
    in
    case Zipper.parent zipper of
        Just parent ->
            getPath parent appendLabel

        Nothing ->
            appendLabel


viewControls : Model -> Element Msg
viewControls model =
    let
        style =
            [ padding 5
            , Border.width 1
            , Border.color <| darkGrey 1.0
            , mouseOver <| [ Background.color <| lightGrey 1.0 ]
            ]
    in
    row
        [ spacing 5, width fill ]
        [ Input.text
            [ padding 5
            , Event.onLoseFocus <| GetNode model.root
            ]
            { onChange = UpdateQuery
            , text = model.root
            , placeholder = Just <| Input.placeholder [] <| text "Enter query hash here"
            , label = Input.labelLeft [ padding 5 ] <| text "Root Hash"
            }
        , Input.button
            style
            { onPress = Just NoOp

            -- Just <| ObjectPut <| objectEncoderPB model.data <| Zipper.children model.zipper
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
        [ Font.color <| darkGrey 1.0
        , Background.color <| white 1.0
        , Font.family fontFamily
        ]
        [ Input.multiline
            [ padding 5 ]
            { onChange = UpdateData
            , placeholder = Just <| Input.placeholder [] <| text "Data"
            , label = Input.labelLeft [] <| text "Data"
            , text = "text field"
            , spellcheck = False
            }
        ]



-- REQUESTS
-- IPFS OBJECT API REQUESTS TURNED INTO TASKS FOR CHAINING


getStats : Hash -> Task Http.Error Node
getStats hash =
    Http.task
        { method = "GET"
        , headers = []
        , url = ipfsApiUrl ++ "object/stat?arg=" ++ hash
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| expect objectStatDecoder
        , timeout = Nothing
        }
        |> Task.andThen
            (\stat ->
                Task.succeed
                    { initNode | size = stat.cumulativeSize, cid = stat.hash }
            )


type alias Url =
    String


type alias Method =
    String


ipfsRequest : Url -> Method -> Node -> Decode.Decoder a -> Task Http.Error a
ipfsRequest url method node decoder =
    Http.task
        { method = method
        , headers = []
        , body = Http.emptyBody
        , url = url
        , resolver = Http.stringResolver <| expect decoder
        , timeout = Nothing
        }


expect : Decode.Decoder a -> Http.Response String -> Result Http.Error a
expect decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata body ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ metadata body ->
            case Decode.decodeString decoder body of
                Ok value ->
                    Ok value

                Err err ->
                    Err (Http.BadBody (Decode.errorToString err))


getData : Node -> Task Http.Error Data
getData node =
    ipfsRequest (ipfsApiUrl ++ "object/get?arg=" ++ node.cid) "GET" node <| Decode.field "Data" Decode.string


getNode : Hash -> Cmd Msg
getNode cid =
    getStats cid
        |> Task.andThen
            (\node -> Tree.tree node [] |> Task.succeed)
        |> Task.andThen (getTree 2)
        |> Task.attempt UpdateZipper


addText : String -> Cmd Msg
addText text =
    let
        body =
            Http.multipartBody
                [ Http.stringPart "textpart" text
                ]
    in
    Http.post
        { url = ipfsApiUrl ++ "add"
        , body = Http.stringBody "text/plain" text
        , expect = Http.expectJson AddFile fileLinkDecoder
        }


getChildren : Node -> Task Http.Error (Tree Node)
getChildren node =
    let
        updateTitle =
            \x data -> Tree.tree { x | title = data } []

        objectGetRequest x =
            ipfsRequest (ipfsApiUrl ++ "object/get?arg=" ++ x.cid) "GET" x <| objectDecoder x
    in
    getStats node.cid
        |> Task.andThen objectGetRequest
        |> Task.andThen
            (\object ->
                object.links
                    |> List.map
                        (\link ->
                            getData link
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
    case depth == 0 of
        True ->
            Task.succeed tree

        False ->
            Tree.label tree
                |> getChildren
                |> Task.andThen
                    (\x ->
                        Tree.children x
                            |> List.map
                                (\child ->
                                    getChildren (Tree.label child)
                                        |> Task.andThen (getTree (depth - 1))
                                )
                            |> Task.sequence
                    )
                |> Task.andThen (\list -> Task.succeed <| Tree.replaceChildren list tree)


dataDecoder : Decode.Decoder String
dataDecoder =
    Decode.field "Data" Decode.string



-- DECODERS


mtypeDecoder : Decode.Decoder (Maybe MimeType)
mtypeDecoder =
    Decode.map Mime.parseMimeType (Decode.field "type" Decode.string)


fileLinkDecoder : Decode.Decoder Link
fileLinkDecoder =
    Decode.succeed Link
        |> required "Name" Decode.string
        |> required "Hash" Decode.string
        |> required "Size" sizeDecoder


nodeDecoder : Node -> Decode.Decoder Node
nodeDecoder node =
    Decode.succeed Node
        |> required "Name" Decode.string
        |> required "Hash" Decode.string
        |> required "Size" Decode.int
        |> optional "Title" Decode.string ""
        |> hardcoded Completed
        |> hardcoded 0


objectDecoder : Node -> Decode.Decoder Object
objectDecoder node =
    Decode.succeed Object
        |> required "Links" (Decode.list <| nodeDecoder node)
        |> required "Data" Decode.string


pbLinkDecoder : Decode.Decoder Link
pbLinkDecoder =
    Decode.succeed Link
        |> required "Name" Decode.string
        |> required "Hash" Decode.string
        |> required "Size" Decode.int


sizeDecoder : Decode.Decoder Int
sizeDecoder =
    Decode.oneOf [ Decode.int, DecodeExtra.parseInt ]


objectStatDecoder : Decode.Decoder ObjectStat
objectStatDecoder =
    Decode.succeed ObjectStat
        |> required "Hash" Decode.string
        |> required "NumLinks" Decode.int
        |> required "BlockSize" Decode.int
        |> required "LinksSize" Decode.int
        |> required "DataSize" Decode.int
        |> required "CumulativeSize" Decode.int


onlyHashDecoder : Decode.Decoder Hash
onlyHashDecoder =
    Decode.field "Hash" Decode.string


objectModifiedDecoder : Decode.Decoder ModifiedObject
objectModifiedDecoder =
    Decode.succeed ModifiedObject
        |> required "Hash" Decode.string
        |> required "Links" (Decode.list pbLinkDecoder)



-- ENCODERS


objectEncoderPB : Data -> List Node -> Value
objectEncoderPB data list =
    Encode.object
        [ ( "Data", Encode.string data )
        , ( "Links", Encode.list linkEncoderPB list )
        ]


dagNodePbEncoder : List Link -> List Value
dagNodePbEncoder list =
    List.map linkApiEncoderPB list


linkEncoderPB : Node -> Value
linkEncoderPB node =
    Encode.object
        [ ( "name", Encode.string node.name )
        , ( "size", Encode.int node.size )
        , ( "multihash", Encode.string node.cid )
        ]


objectApiEncoder : Data -> List Node -> Value
objectApiEncoder data list =
    Encode.object
        [ ( "Data", Encode.string data )
        , ( "Links", Encode.list linkEncoderPB list )
        ]


linkApiEncoderPB : Link -> Value
linkApiEncoderPB link =
    Encode.object
        [ ( "Name", Encode.string link.name )
        , ( "Size", Encode.int link.size )
        , ( "Hash", Encode.string link.cid )
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
            text (Debug.toString error)



-- COLOR PROPERTIES


white : Float -> Color
white alpha =
    E.rgba255 255 255 255 alpha


orange : Float -> Color
orange alpha =
    E.rgba255 255 122 0 alpha


yellow : Float -> Color
yellow alpha =
    E.rgba255 255 214 0 alpha


green : Float -> Color
green alpha =
    E.rgba255 152 237 0 alpha


cyan : Float -> Color
cyan alpha =
    E.rgba255 2 142 155 alpha


blue : Float -> Color
blue alpha =
    E.rgba255 62 19 175 alpha


violet : Float -> Color
violet alpha =
    E.rgba255 210 0 107 alpha


lightGrey : Float -> Color
lightGrey alpha =
    E.rgba255 211 211 211 alpha


darkGrey : Float -> Color
darkGrey alpha =
    E.rgba255 169 169 169 alpha



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



{- }
   addLink : Hash -> String -> Hash -> Task.Task Http.Error Hash
   addLink parent name cid =
       Http.get ( ipfsApiUrl
                       ++ "object/patch/add-link?arg=" ++ parent
                       ++ "&arg=" ++ name
                       ++ "&arg=" ++ cid )
                   (Decode.field "Hash" Decode.string)
                   |> Http.toTask

   patchObject : Node -> Hash -> Task.Task Http.Error Hash
   patchObject node new_cid =
       case node.parent of
           Just parent ->
               addLink parent.cid node.name new_cid
                   |> Task.andThen (\hash -> patchObject parent hash)
           Nothing ->
               Task.succeed node.cid

-}
-- BUNCH OF REQUESTS FOR ADDING FILES
-- [ add files, patch current node, patch path, patch root node ]
{- }

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



   patchPath : Path -> Path -> Hash -> Task.Task Http.Error Path
   patchPath acc path hash =
       case path of
           (childname, childhash) :: (parentname, parenthash) :: xs ->
               addLink parenthash
                   { name = childname, size = 0, cid = hash, obj_type = 2, status = Completed }
                   |> Task.andThen
                       (\hash ->
                           patchPath (acc ++ [(parentname, hash)]) ((parentname, parenthash) :: xs) hash )
           x :: [] ->
               Task.succeed acc
           [] ->
               Task.succeed acc
-}