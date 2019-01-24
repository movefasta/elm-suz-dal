module Main exposing (main)

-- import String.Extra
-- import Svg exposing (svg)

import Browser exposing (Document, document)
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Bytes
import Bytes.Encode
import Element as E exposing (..)
import Element.Lazy exposing (lazy, lazy2)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Download
import File.Select
import Filesize
import Html.Attributes
import Http
import Json.Decode as Decode
import Json.Decode.Extra as DecodeExtra exposing (parseInt)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode exposing (Value, object)
import List.Extra
import MimeType as Mime
import Result
import Task exposing (Task)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
import Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        zipper =
            Zipper.fromTree <| Tree.singleton initNode
    in
    ( Model initNode.cid zipper [] [ initNode ] False "" [] key url, getNode initNode.cid )



-- CONSTANTS


initNode : Node
initNode =
    { id = 0
    , status = Completed
    , name = "ROOT"
    , cid = "zdpuAuS47nYz9ZC4zvirXK5ze1P3RFwPQ4L6BR8BXH8HBEVSQ"
    , size = 0
    , description = "Корень дерева трактовочной сети"
    , mimetype = Nothing
    , color = 0
    }


ipfsApiUrl : String
ipfsApiUrl =
    "http://localhost:5001/api/v0/"


ipfsGatewayUrl : String
ipfsGatewayUrl =
    "http://localhost:8080/ipfs/"



-- TYPES


type alias Model =
    { root : Hash
    , zipper : Zipper Node
    , upload : List File -- fileReader API buffer
    , path : List Node -- dag nodes path
    , hover : Bool
    , addtext : String
    , content : List Node
    , key : Nav.Key
    , url : Url.Url
    }


type alias Node =
    { id : Id
    , status : Status
    , name : String
    , cid : Hash
    , size : Int
    , description : String
    , mimetype : Maybe Mime.MimeType
    , color : Int
    }


type alias IpfsNodeID =
    { id : String
    , publicKey : String
    , addresses : List String
    , agentVersion : String
    , protocolVersion : String
    }


type Status
    = Editing
      --    | Loading
    | Completed
    | Selected


type Action
    = Remove Node
    | Set Node
    | Move Node Int



-- OBJECT API TYPES FOR DECODE IPFS RESPONSES


type alias Link =
    { name : String
    , hash : String
    , size : Int
    }


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


type alias Hash =
    String


type alias Url =
    String


type alias Id =
    Int



{-
   type Page
       = Loading Session.Data
       | Krugozor Krugozor.Model
       | Level Levels.Model
       | Tensor Structure.Model
       | Profile Profile.Model
-}


type Msg
    = NoOp
    | UpdateQuery Hash
    | GetNode Hash
    | DagPut Value
    | UpdateZipper (Result.Result Http.Error (Tree Node))
    | GetRootHash (Result.Result Http.Error Hash)
    | GetContentHash (Result.Result Http.Error Hash)
    | GetNodeContent (Result.Result Http.Error (List Node))
    | AddText String
    | ChangeFocus Node
    | UpdateFocus Node
    | DownloadAsJson String
    | Pick
    | GotFiles File (List File)
    | Content (Result.Result Http.Error (List Node))
    | Perform Action
    | UpdateAddText String
    | DragEnter
    | DragLeave
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        label =
            Zipper.label model.zipper
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        Pick ->
            ( model, File.Select.files [ "*" ] GotFiles )

        DragEnter ->
            ( { model | hover = True }, Cmd.none )

        DragLeave ->
            ( { model | hover = False }, Cmd.none )

        GotFiles file files ->
            let
                upload body =
                    Http.task
                        { method = "POST"
                        , headers = []
                        , url = ipfsApiUrl ++ "add"
                        , body = Http.multipartBody [ Http.filePart "file" body ]
                        , resolver = Http.stringResolver <| expect <| Decode.andThen linkToNodeDecoder <| objLinkDecoder
                        , timeout = Nothing
                        }
                        |> Task.andThen
                            (\x ->
                                Task.succeed
                                    { x | mimetype = File.mime file |> Mime.parseMimeType }
                            )
            in
            ( { model | upload = [], hover = False }
            , List.map upload (file :: files)
                |> Task.sequence
                |> Task.attempt Content
            )

        Content result ->
            case result of
                Ok list ->
                    ( { model
                        | content = List.indexedMap (\i a -> { a | id = i }) (list ++ model.content)
                        , addtext =
                            if List.isEmpty list then
                                ""

                            else
                                model.addtext
                      }
                    , dagPut GetContentHash cidDecoder <| Encode.list contentEncoder (list ++ model.content)
                    )

                Err _ ->
                    ( model, Cmd.none )

        UpdateQuery hash ->
            ( { model | root = hash }, Cmd.none )

        GetRootHash result ->
            case result of
                Ok cid ->
                    ( { model | root = cid }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GetNodeContent result ->
            case result of
                Ok nodes ->
                    ( { model | content = List.indexedMap (\i a -> { a | id = i }) nodes }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GetContentHash result ->
            case result of
                Ok cid ->
                    let
                        updatedZipper =
                            Zipper.replaceLabel
                                { label | cid = cid, size = List.sum <| List.map .size model.content }
                                model.zipper
                    in
                    ( { model | zipper = updatedZipper }
                    , dagPut GetRootHash cidDecoder <| treeEncoder 0 <| Zipper.toTree <| updatedZipper
                    )

                Err _ ->
                    ( model, Cmd.none )

        DownloadAsJson str ->
            ( model, File.Download.string "136_structure.json" "application/json" str )

        AddText data ->
            ( model
            , if String.isEmpty data then
                Cmd.none

              else
                addText data
            )

        UpdateAddText data ->
            ( { model | addtext = data }, Cmd.none )

        UpdateFocus node ->
            ( { model | zipper = Zipper.replaceLabel node model.zipper }, Cmd.none )

        ChangeFocus node ->
            let
                completeFocusedLabel =
                    Zipper.replaceLabel { label | status = Completed } model.zipper

                newZipper =
                    case Zipper.findFromRoot (\x -> x.id == node.id) completeFocusedLabel of
                        Just zipper ->
                            Zipper.mapLabel (\_ -> node) zipper

                        Nothing ->
                            model.zipper
            in
            ( { model | zipper = newZipper, content = [] }
            , if node.status == Editing then
                Task.attempt
                    (\_ -> NoOp)
                <|
                    Dom.focus ("cell-" ++ String.fromInt node.id)

              else
                Http.get
                    { url = ipfsApiUrl ++ "dag/get?arg=" ++ node.cid
                    , expect = Http.expectJson GetNodeContent contentDecoder
                    }
            )

        GetNode cid ->
            ( { model | root = cid }, getNode cid )

        DagPut value ->
            ( model
            , dagPut GetRootHash cidDecoder value
            )

        UpdateZipper result ->
            let
                indexedTree tree =
                    Tree.indexedMap (\idx val -> { val | id = idx }) tree
            in
            case result of
                Ok x ->
                    ( { model | zipper = Zipper.fromTree <| indexedTree x }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        Perform action ->
            let
                links =
                    model.content
                        |> (case action of
                                Remove link ->
                                    List.Extra.removeAt link.id
                                        >> List.indexedMap (\i a -> { a | id = i })

                                Move link offset ->
                                    List.Extra.swapAt link.id (link.id + offset)
                                        >> List.indexedMap (\i a -> { a | id = i })

                                Set link ->
                                    List.map 
                                        (\x ->
                                            if x.id == link.id then
                                                link

                                            else
                                                { x | status = Completed }
                                        )
                           )
            in
            ( { model | content = links }
            , case action of
                Set link ->
                    case link.status of
                        Editing -> 
                            Task.attempt (\_ -> NoOp) <| Dom.focus ("file-id-" ++ String.fromInt link.id)
                        
                        _ ->
                            Cmd.none

                _ ->
                    dagPut GetContentHash cidDecoder <| Encode.list contentEncoder links
            )



--VIEW


view : Model -> Document Msg
view model =
    let
        label =
            Zipper.label model.zipper
    in
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
                [ Font.size 14
                , Font.family [ Font.typeface "Ubuntu-Regular", Font.sansSerif ]
                , spacing 10
                , padding 10
                , width fill
                ]
                [ viewControls model

                --, row [] [ viewPath model.path, E.text <| .cid label ]
                , row
                    [ width fill ]
                    [ lazy viewTable <| Zipper.toTree model.zipper
                    , lazy2 viewContent model.addtext model.content
                    ]
                ]
        ]
    }


viewContent : String -> List Node -> Element Msg
viewContent string nodes =
    let
        buttonStyle =
            [ padding 5
            , Border.width 1
            , Border.color <| darkGrey 1.0
            , mouseOver <| [ Background.color <| lightGrey 1.0 ]
            , width fill
            , height shrink
            ]
    in
    column
        [ height <| px 900
        , width (fill |> maximum 700)
        , padding 10
        , spacing 5
        , scrollbarY
        , alignTop
        ]
        [ row
            [ width fill
            , spacing 5
            ]
            [ Input.button
                buttonStyle
                { onPress = Just <| AddText string
                , label = text "Добавить текст"
                }
            , Input.button
                buttonStyle
                { onPress = Just Pick
                , label = text "Добавить файлы"
                }
            ]
        , Input.multiline
            [ width fill
            , height shrink
            ]
            { onChange = UpdateAddText
            , text = string
            , placeholder = Just <| Input.placeholder [] <| text string
            , label = Input.labelHidden "Data input"
            , spellcheck = True
            }
        , column [ width fill ] <| List.map viewNodeAsFile nodes
        ]


viewNodeActions : Node -> Element Msg
viewNodeActions node =
    let
        actionStyle =
            [ mouseOver <| [ Background.color <| lightGrey 1.0 ]
            , paddingXY 7 2
            , Font.size 12
            , Border.rounded 3
            ]
    in
    el
        [ width fill ]
    <|
        row
            [ if node.status /= Selected then
                htmlAttribute <| Html.Attributes.style "visibility" "hidden"

              else
                alignRight
            , spacing 5
            , padding 5
            ]
            [ newTabLink
                actionStyle
                { url = ipfsGatewayUrl ++ node.cid
                , label = text "открыть"
                }
            , Input.button actionStyle
                { onPress = Just <| Perform <| Set { node | status = Editing }
                , label = text "править"
                }
            , downloadAs
                actionStyle
                { label = text "загрузить"
                , filename = node.name
                , url = ipfsGatewayUrl ++ node.cid
                }
            , Input.button actionStyle
                { onPress = Just <| Perform <| Move node -1
                , label = text "˄"
                }
            , Input.button actionStyle
                { onPress = Just <| Perform <| Move node 1
                , label = text "˅"
                }
            , Input.button actionStyle
                { onPress = Just <| Perform <| Remove node
                , label = text "x"
                }
            ]


viewNodeAsFile : Node -> Element Msg
viewNodeAsFile node =
    let
        style =
            [ width fill
            , paddingXY 10 0
            ]

        selectStyle =
            [ width fill
            , Event.onClick <| Perform <| Set { node | status = Completed }
            , Border.width 1
            , Border.dashed
            , Border.color <| darkGrey 1.0
            ]

        anotherStyle =
            [ width fill
            , Event.onClick <|
                case node.status of
                    Editing ->
                        NoOp

                    _ ->
                        Perform <| Set { node | status = Selected }
            , Border.width 1
            , Border.dashed
            , Border.color <| white 1.0
            , mouseOver [ Background.color <| lightGrey 0.2 ]
            ]
    in
    column
        style
        [ viewNodeActions node
        , el
            (if node.status /= Selected then
                anotherStyle

             else
                selectStyle
            )
          <|
            case node.mimetype of
                Just (Mime.Image _) ->
                    image [ width fill ]
                        { src = ipfsGatewayUrl ++ node.cid
                        , description = node.name
                        }

                Just (Mime.Text Mime.PlainText) ->
                    let
                        font =
                            if node.size <= 140 then
                                [ Font.size 24
                                ]

                            else if node.size > 140 && node.size <= 500 then
                                [ Font.italic
                                , Font.size 16
                                ]

                            else if node.size > 500 && node.size <= 1800 then
                                [ Font.size 14 ]

                            else
                                []
                    in
                    if node.status == Editing then
                        Input.multiline
                            [ height shrink
                            , width fill
                            , htmlAttribute <| Html.Attributes.id <| String.concat [ "file-id-", String.fromInt node.id ]
                            , Event.onLoseFocus <| Perform <| Set { node | status = Selected }
                            ]
                            { onChange = \new -> Perform <| Set { node | description = new }
                            , text = node.description
                            , placeholder = Just <| Input.placeholder [] <| el [] none
                            , label = Input.labelHidden "Text data input"
                            , spellcheck = True
                            }

                    else
                        paragraph
                            ([ width fill, padding 5, clip ] ++ font)
                            [ text node.description ]

                _ ->
                    row
                        [ width fill
                        ]
                        [ el
                            [ padding 10
                            , width fill
                            , htmlAttribute <| Html.Attributes.style "overflow" "hidden"
                            , htmlAttribute <| Html.Attributes.style "white-space" "nowrap"
                            , htmlAttribute <| Html.Attributes.style "text-overflow" "ellipsis"
                            ]
                          <|
                            text node.name
                        , el
                            [ Font.color <| darkGrey 1.0
                            , padding 5
                            , alignRight
                            , centerY
                            ]
                          <|
                            text <|
                                "("
                                    ++ Filesize.format node.size
                                    ++ ")"
                        ]
        ]


viewControls : Model -> Element Msg
viewControls model =
    let
        style =
            [ padding 5
            , Border.width 1
            , Border.color <| darkGrey 1.0
            , mouseOver <| [ Background.color <| lightGrey 1.0 ]
            ]

        treeToJson =
            treeEncoder 0 <| Zipper.toTree <| model.zipper
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
            { onPress = Just <| DagPut treeToJson
            , label = text "dag put"
            }
        , Input.button
            style
            { onPress = Just <| DownloadAsJson <| Encode.encode 4 treeToJson
            , label = text "download json"
            }
        ]


viewTable : Tree Node -> Element Msg
viewTable tree =
    let
        alpha =
            0.7
    in
    column
        [ height <| px 900
        , width (fill |> minimum 500)
        , spacing 5
        , scrollbarY
        , alignTop
        ]
    <|
        List.map
            (\child ->
                viewSector child <|
                    colorCodeConverter (.color <| Tree.label child) alpha
            )
        <|
            Tree.children tree


viewSector : Tree Node -> Color -> Element Msg
viewSector tree color =
    let
        node =
            Tree.label tree

        childMap child =
            let
                child_node =
                    Tree.label child
            in
            case String.toInt <| .name child_node of
                Just int ->
                    row
                        [ spacing 5
                        , padding 5
                        , width fill
                        , height fill
                        , Background.color <| lightGrey 0.5
                        , htmlAttribute <| Html.Attributes.style "overflow-wrap" "inherit"
                        ]
                    <|
                        [ viewCell child_node <| white 1.0 ]
                            ++ (List.map
                                    (\x ->
                                        viewCell (Tree.label x) <|
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
        , height fill
        ]
        [ el
            [ Font.size 20
            , width fill
            , paddingXY 5 15
            , Border.width 0
            ]
          <|
            viewCell node color
        , column
            [ width fill
            , height fill
            , spacing 5
            ]
          <|
            List.map childMap <|
                Tree.children tree
        ]


viewCell : Node -> Color -> Element Msg
viewCell node color =
    case node.status of
        Editing ->
            Input.multiline
                [ height fill
                , width fill
                , padding 10
                , htmlAttribute <| Html.Attributes.id <| String.concat [ "cell-", String.fromInt node.id ]
                , Event.onLoseFocus <| UpdateFocus { node | status = Selected }
                , Font.center
                ]
                { onChange = \new -> UpdateFocus { node | description = new }
                , text = node.description
                , placeholder = Just <| Input.placeholder [] <| el [] none
                , label = Input.labelHidden "Data input"
                , spellcheck = True
                }

        _ ->
            textColumn
                [ if node.status == Selected then
                    Background.color <| lightGrey 1.0

                  else
                    Background.color color
                , Border.width 1
                , Border.color <| darkGrey 1.0
                , height fill
                , width fill
                , htmlAttribute <| Html.Attributes.id <| String.concat [ "cell-", String.fromInt node.id ]
                , Event.onClick <|
                    case node.status of
                        Selected ->
                            NoOp

                        _ ->
                            ChangeFocus { node | status = Selected }
                , Event.onDoubleClick <| ChangeFocus { node | status = Editing }
                ]
                [ paragraph
                    [ width fill
                    , centerY
                    , Font.center
                    , paddingEach { top = 10, right = 10, bottom = 1, left = 10 }
                    ]
                    [ text <| .description node ]
                , paragraph
                    [ alignRight
                    , alignBottom
                    , Font.size 10
                    , Font.color <| darkGrey 1.0
                    , padding 2
                    ]
                    [ text <| Filesize.format node.size
                    ]
                ]



-- REQUESTS
-- IPFS OBJECT API REQUESTS TURNED INTO TASKS FOR CHAINING


turnToBytesPart : String -> String -> Encode.Value -> Http.Part
turnToBytesPart message mime json =
    Encode.encode 0 json
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
        |> Http.bytesPart message mime


dagPut : (Result Http.Error a -> Msg) -> Decode.Decoder a -> Encode.Value -> Cmd Msg
dagPut msg decoder value =
    Http.request
        { method = "POST"
        , url = ipfsApiUrl ++ "dag/put"
        , headers = []
        , body =
            Http.multipartBody
                [ turnToBytesPart "whatever" "application/json" value ]
        , expect = Http.expectJson msg decoder
        , timeout = Nothing
        , tracker = Just "upload"
        }


addText : String -> Cmd Msg
addText string =
    let
        bytes =
            string
                |> Bytes.Encode.string
                |> Bytes.Encode.encode
    in
    Http.task
        { method = "POST"
        , headers = []
        , url = ipfsApiUrl ++ "add"
        , body = Http.multipartBody [ Http.bytesPart "whatever" "text/plain" bytes ]
        , resolver = Http.stringResolver <| expect <| Decode.andThen linkToNodeDecoder <| objLinkDecoder
        , timeout = Nothing
        }
        |> Task.andThen
            (\link ->
                Task.succeed
                    [ { link
                        | mimetype = Mime.parseMimeType "text/plain"
                        , size = Bytes.width bytes
                        , description =
                            if Bytes.width bytes < 5000 then
                                string

                            else
                                ""
                      }
                    ]
            )
        |> Task.attempt Content


getContent : Hash -> Task Http.Error (List Node)
getContent cid =
    Http.task
        { method = "GET"
        , headers = []
        , url = ipfsApiUrl ++ "dag/get?arg=" ++ cid
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| expect contentDecoder
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

        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Ok value ->
                    Ok value

                Err err ->
                    Err (Http.BadBody (Decode.errorToString err))


getNode : Hash -> Cmd Msg
getNode cid =
    Http.get
        { url = ipfsApiUrl ++ "dag/get?arg=" ++ cid
        , expect = Http.expectJson UpdateZipper treeDecoder
        }



-- DECODERS


treeDecoder : Decode.Decoder (Tree Node)
treeDecoder =
    Decode.map2 Tree.tree nodeDecoder linksDecoder


nodeDecoder : Decode.Decoder Node
nodeDecoder =
    Decode.succeed Node
        |> hardcoded 0
        |> hardcoded Completed
        |> required "name" Decode.string
        |> required "cid" Decode.string
        |> optional "size" sizeDecoder 0
        |> optional "description" Decode.string ""
        |> optional "mimetype" (Decode.map Mime.parseMimeType Decode.string) Nothing
        |> optional "color" Decode.int 0


linksDecoder : Decode.Decoder (List (Tree Node))
linksDecoder =
    Decode.field "links" (Decode.lazy (\_ -> Decode.list treeDecoder))


contentDecoder : Decode.Decoder (List Node)
contentDecoder =
    Decode.list nodeDecoder


sizeDecoder : Decode.Decoder Int
sizeDecoder =
    Decode.oneOf [ Decode.int, DecodeExtra.parseInt ]


linkToNodeDecoder : Link -> Decode.Decoder Node
linkToNodeDecoder link =
    Decode.succeed Node
        |> hardcoded 0
        |> hardcoded Completed
        |> hardcoded link.name
        |> hardcoded link.hash
        |> hardcoded link.size
        |> optional "description" Decode.string ""
        |> optional "mimetype" (Decode.map Mime.parseMimeType Decode.string) Nothing
        |> optional "color" Decode.int 0


objLinkDecoder : Decode.Decoder Link
objLinkDecoder =
    Decode.succeed Link
        |> required "Name" Decode.string
        |> required "Hash" Decode.string
        |> required "Size" sizeDecoder


ipfsNodeID : Decode.Decoder IpfsNodeID
ipfsNodeID =
    Decode.succeed IpfsNodeID
        |> required "ID" Decode.string
        |> required "PublicKey" Decode.string
        |> required "Addresses" (Decode.list Decode.string)
        |> required "AgentVersion" Decode.string
        |> required "ProtocolVersion" Decode.string


objectDecoder : Decode.Decoder Object
objectDecoder =
    Decode.succeed Object
        |> required "Links" (Decode.list nodeDecoder)
        |> required "Data" Decode.string



-- decode answer of dag put request


cidDecoder : Decode.Decoder Hash
cidDecoder =
    Decode.at [ "Cid", "/" ] Decode.string


colorCodeConverter : Int -> Float -> Color
colorCodeConverter i alpha =
    let
        color =
            case i of
                0 ->
                    yellow

                1 ->
                    orange

                2 ->
                    violet

                3 ->
                    blue

                4 ->
                    cyan

                5 ->
                    green

                _ ->
                    white
    in
    color alpha



-- ENCODERS


treeEncoder : Int -> Tree Node -> Value
treeEncoder i tree =
    let
        node =
            Tree.label tree
    in
    Encode.object <|
        [ ( "description", Encode.string node.description )
        , ( "size", Encode.int node.size )
        , ( "cid", Encode.string node.cid )
        , ( "name", Encode.string node.name )
        , ( "links", Encode.list (treeEncoder (i + 1)) <| Tree.children tree )
        ]
            ++ (if i == 1 then
                    [ ( "color", Encode.int <| Maybe.withDefault 9 <| String.toInt <| node.name ) ]

                else
                    []
               )


contentEncoder : Node -> Value
contentEncoder node =
    Encode.object
        [ ( "name", Encode.string node.name )
        , ( "size", Encode.int node.size )
        , ( "cid", Encode.string node.cid )
        , ( "mimetype", Encode.string <| Mime.toString <| Maybe.withDefault (Mime.OtherMimeType "") node.mimetype )
        , ( "description", Encode.string node.description )
        ]



-- HELPERS
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



{- }


   ipfsRequest : Url -> String -> Decode.Decoder a -> Task Http.Error a
   ipfsRequest url method decoder =
       Http.task
           { method = method
           , headers = []
           , body = Http.emptyBody
           , url = url
           , resolver = Http.stringResolver <| expect decoder
           , timeout = Nothing
           }


   -- navigation in breadcrumbs


   viewPath : List Node -> Element Msg
   viewPath list =
       row [ width fill ] <| List.map viewNodeinPath list



   {- }
      viewURL : List Node -> Element Msg
      viewURL list =
          case list of
              x :: xs ->
                  row [] <| [ text <| .cid x ] ++ List.map (\node -> text <| "/" ++ .name node) xs

              [] ->
                  el [] <| text "NO PATH"
   -}


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



   objectStatDecoder : Decode.Decoder ObjectStat
   objectStatDecoder =
       Decode.succeed ObjectStat
           |> required "Hash" Decode.string
           |> required "NumLinks" Decode.int
           |> required "BlockSize" Decode.int
           |> required "LinksSize" Decode.int
           |> required "DataSize" Decode.int
           |> required "CumulativeSize" Decode.int


   objectEncoder : String -> List Node -> Value
   objectEncoder data list =
       Encode.object
           [ ( "Data", Encode.string data )
           , ( "Links", Encode.list linkEncoder list )
           ]


   linkEncoder : Node -> Value
   linkEncoder node =
       Encode.object
           [ ( "Name", Encode.string node.name )
           , ( "Size", Encode.int node.size )
           , ( "Hash", Encode.string node.cid )
           ]


   getStats : Hash -> Task Http.Error Int
   getStats cid =
       Http.task
           { method = "GET"
           , headers = []
           , url = ipfsApiUrl ++ "object/stat?arg=" ++ cid
           , body = Http.emptyBody
           , resolver = Http.stringResolver <| expect objectStatDecoder
           , timeout = Nothing
           }
           |> Task.andThen
               (\stat ->
                   Task.succeed stat.cumulativeSize
               )


   getData : Node -> Task Http.Error String
   getData node =
       let
           url =
               ipfsApiUrl ++ "object/get?arg=" ++ node.cid
       in
       ipfsRequest url "GET" (Decode.field "Data" Decode.string)




   getChildren : Node -> Task Http.Error (Tree Node)
   getChildren node =
       let
           updatedescription =
               \x data -> Tree.tree { x | description = data } []

           objectGetRequest x =
               ipfsRequest (ipfsApiUrl ++ "object/get?arg=" ++ x.cid) "GET" objectDecoder
       in
       getStats node.cid
           |> Task.andThen
               (\size -> Task.succeed { initNode | size = size, cid = node.cid })
           |> Task.andThen objectGetRequest
           |> Task.andThen
               (\object ->
                   object.links
                       |> List.map
                           (\link ->
                               getData link
                                   |> Task.map2 updatedescription (Task.succeed link)
                           )
                       |> Task.sequence
                       |> Task.andThen
                           (\links_list ->
                               Task.succeed <| Tree.tree { node | description = object.data } links_list
                           )
               )


   getTree : Int -> Tree Node -> Task Http.Error (Tree Node)
   getTree depth tree =
       if depth == 0 then
           Task.succeed tree

       else
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

      treeDecoder : Decode.Decoder (Tree Node)
      treeDecoder =
              Decode.map2 Tree.tree
                  (Decode.field "Data" Decode.string)
                  (Decode.field "Links" (Decode.map ))
              Decode.map2 Node
                      (Decode.field "value" nodeDecoder)
                      (Decode.field "children" (Decode.lazy (\_ -> decoder nodeDecoder |> Decode.list)))
                  ]

      onlyHashDecoder : Decode.Decoder Hash
      onlyHashDecoder =
          Decode.field "Hash" Decode.string

      objectModifiedDecoder : Decode.Decoder ModifiedObject
      objectModifiedDecoder =
          Decode.succeed ModifiedObject
              |> required "Hash" Decode.string
              |> required "Links" (Decode.list linkDecoder)



         -- BUNCH OF REQUESTS FOR ADDING FILES
         -- [ add files, patch current node, patch path, patch root node ]


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
                         { name = childname, size = 0, cid = hash, title = 2, status = Completed }
                         |> Task.andThen
                             (\hash ->
                                 patchPath (acc ++ [(parentname, hash)]) ((parentname, parenthash) :: xs) hash )
                 x :: [] ->
                     Task.succeed acc
                 [] ->
                     Task.succeed acc
-}
