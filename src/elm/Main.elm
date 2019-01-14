module Main exposing (main)

-- import String.Extra
-- import Svg exposing (svg)

import Browser exposing (Document, document)
import Browser.Dom as Dom
import Bytes
import Bytes.Encode
import Element as E exposing (..)
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



-- CONSTANTS


initModel : Model
initModel =
    { root = initNode.cid
    , data = ""
    , zipper = Zipper.fromTree <| Tree.singleton initNode
    , raw_dag = ""
    , upload = []
    , path = [ initNode ]
    , hover = False
    , addtext = ""
    }


initNode : Node
initNode =
    { name = "ROOT"
    , cid = "QmPcEu59DUhZGz4woYgDL22bZ6byCVNt97m5ds2QwUMqUo"
    , size = 0
    , title = ""
    , status = Completed
    , id = 0
    , content = []
    }


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
    , upload : List File -- file list
    , path : List Node -- dag nodes path
    , hover : Bool
    , addtext : String
    }


type alias Node =
    { name : String
    , cid : String
    , size : Int
    , title : Data
    , status : Status
    , id : Id
    , content : List Link
    }


type alias Content a =
    { a
        | id : Id
        , cid : String
        , size : Int
        , title : String
        , status : Status
    }


type alias Link =
    { name : String
    , cid : String
    , size : Int
    , mimetype : Maybe Mime.MimeType
    , preview : String
    , id : Id
    , status : Status
    }


type Status
    = Editing
      --    | Loading
    | Completed
    | Selected


type Action
    = Remove Link
    | Set Link
    | Move Link Int



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


type alias Hash =
    String


type alias Data =
    String


type alias Url =
    String


type alias Method =
    String


type alias Id =
    Int


type Msg
    = NoOp
    | UpdateQuery Hash
    | UpdateData String
    | GetNode Hash
    | ObjectPut Value
    | UpdateZipper (Result Http.Error (Tree Node))
    | AddText String
    | ChangeFocus Node
    | UpdateFocus Node
    | GetNewHash (Result.Result Http.Error Hash)
    | DownloadAsJson String
    | Pick
    | GotFiles File (List File)
    | Uploading (Result Http.Error (List Link))
    | DragEnter
    | DragLeave
    | UpdateAddText String
    | Perform Action


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        label =
            Zipper.label model.zipper
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

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
                        , resolver = Http.stringResolver <| expect linkDecoder
                        , timeout = Nothing
                        }
                        |> Task.andThen
                            (\link ->
                                Task.succeed
                                    { link | mimetype = File.mime file |> Mime.parseMimeType }
                            )
            in
            ( { model | upload = [], hover = False }
            , List.map upload (file :: files) |> Task.sequence |> Task.attempt Uploading
            )

        Uploading result ->
            case result of
                Ok list ->
                    ( { model
                        | zipper =
                            Zipper.replaceLabel
                                { label
                                    | content = List.indexedMap (\i a -> { a | id = i }) (list ++ label.content)
                                }
                                model.zipper
                        , addtext =
                            if List.isEmpty list then
                                ""

                            else
                                model.addtext
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        UpdateQuery hash ->
            ( { model | root = hash }, Cmd.none )

        GetNewHash result ->
            case result of
                Ok cid ->
                    ( { model | root = cid }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        DownloadAsJson str ->
            ( model, File.Download.string "136_structure.json" "application/json" str )

        UpdateData text ->
            ( { model | data = text }, Cmd.none )

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
            ( { model
                | zipper = newZipper
                , data = node.title

                --, path = getPath newZipper []
              }
            , if node.status == Editing then
                Task.attempt (\_ -> NoOp) <| Dom.focus ("cell-" ++ String.fromInt node.id)

              else
                Cmd.none
            )

        GetNode cid ->
            ( { model | root = cid }, getNode cid )

        ObjectPut value ->
            ( model
            , Http.request
                { method = "POST"
                , url = ipfsApiUrl ++ "object/put"
                , headers = []
                , body =
                    Http.multipartBody
                        [ turnToBytesPart "whatever" "application/json" value ]
                , expect = Http.expectJson GetNewHash cidDecoder
                , timeout = Nothing
                , tracker = Just "upload"
                }
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
                    ( { model | raw_dag = "UPDATE ZIPPER TASK FAIL" }, Cmd.none )

        Perform action ->
            let
                links =
                    label.content
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
            ( { model | zipper = Zipper.replaceLabel { label | content = links } model.zipper }
            , case action of
                Set link ->
                    if link.status == Editing then
                        Task.attempt (\_ -> NoOp) <| Dom.focus ("link-" ++ String.fromInt link.id)

                    else
                        Cmd.none

                _ ->
                    Cmd.none
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
                    [ viewTable <| Zipper.toTree model.zipper
                    , viewContent model.addtext <| .content label
                    ]
                ]
        ]
    }


viewDataInput : Node -> Element Msg
viewDataInput node =
    Input.text
        [ padding 5
        , height shrink
        , Event.onLoseFocus <| UpdateFocus node
        ]
        { onChange = UpdateData
        , text = node.title
        , placeholder = Just <| Input.placeholder [] <| text "Введите данные сюда"
        , label = Input.labelHidden "Data input"
        }


viewContent : String -> List Link -> Element Msg
viewContent string links =
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
        , column [ width fill ] <| List.map viewLink links
        ]


viewLinkActions : Link -> Element Msg
viewLinkActions link =
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
            [ if link.status /= Selected then
                htmlAttribute <| Html.Attributes.style "visibility" "hidden"

              else
                alignRight
            , spacing 5
            , padding 5
            ]
            [ Input.button actionStyle
                { onPress = Just <| Perform <| Set { link | status = Editing }
                , label = text "править"
                }
            , downloadAs
                actionStyle
                { label = text "загрузить"
                , filename = link.name
                , url = ipfsGatewayUrl ++ link.cid
                }
            , Input.button actionStyle
                { onPress = Just <| Perform <| Move link -1
                , label = text "˄"
                }
            , Input.button actionStyle
                { onPress = Just <| Perform <| Move link 1
                , label = text "˅"
                }
            , Input.button actionStyle
                { onPress = Just <| Perform <| Remove link
                , label = text "x"
                }
            ]


viewLink : Link -> Element Msg
viewLink link =
    let
        style =
            [ width fill
            , paddingXY 10 0
            ]

        selectStyle =
            [ width fill
            , Event.onClick <| Perform <| Set { link | status = Completed }
            , Border.width 1
            , Border.dashed
            , Border.color <| darkGrey 1.0
            ]

        anotherStyle =
            [ width fill
            , Event.onClick <|
                case link.status of
                    Editing ->
                        NoOp

                    _ ->
                        Perform <| Set { link | status = Selected }
            , Border.width 1
            , Border.dashed
            , Border.color <| white 1.0
            , mouseOver [ Background.color <| lightGrey 0.2 ]
            ]
    in
    column
        style
        [ viewLinkActions link
        , el
            (if link.status /= Selected then
                anotherStyle

             else
                selectStyle
            )
          <|
            case link.mimetype of
                Just (Mime.Image _) ->
                    image [ width fill ]
                        { src = ipfsGatewayUrl ++ link.cid
                        , description = link.name
                        }

                Just (Mime.Text Mime.PlainText) ->
                    let
                        contentLength =
                            String.length link.preview

                        font =
                            if contentLength <= 140 then
                                [ Font.size 24
                                ]

                            else if contentLength > 140 && contentLength <= 500 then
                                [ Font.italic
                                , Font.size 16
                                ]

                            else if contentLength > 500 && contentLength <= 1800 then
                                [ Font.size 14 ]

                            else
                                []
                    in
                    if link.status == Editing then
                        Input.multiline
                            [ height shrink
                            , width fill
                            , htmlAttribute <| Html.Attributes.id <| String.concat [ "link-", String.fromInt link.id ]
                            , Event.onLoseFocus <| Perform <| Set { link | status = Selected }
                            ]
                            { onChange = \new -> Perform <| Set { link | preview = new }
                            , text = link.preview
                            , placeholder = Just <| Input.placeholder [] <| el [] none
                            , label = Input.labelHidden "Text data input"
                            , spellcheck = True
                            }

                    else
                        paragraph
                            ([ width fill, padding 5, clip ] ++ font)
                            [ text link.preview ]

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
                            text link.name
                        , el
                            [ Font.color <| darkGrey 1.0
                            , padding 5
                            , alignRight
                            , centerY
                            ]
                          <|
                            text <|
                                "("
                                    ++ Filesize.format link.size
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

        rootZipper =
            Zipper.root model.zipper

        data =
            rootZipper |> Zipper.label |> .title

        links =
            rootZipper |> Zipper.children |> List.map Tree.label
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
            { onPress = Just <| ObjectPut <| objectEncoder data links
            , label = text "obj put"
            }
        , Input.button
            style
            { onPress = Just <| DownloadAsJson <| Encode.encode 4 <| treeEncoder 0 <| Zipper.toTree <| model.zipper
            , label = text "download json"
            }
        ]


viewTable : Tree Node -> Element Msg
viewTable tree =
    let
        alpha =
            0.7

        color i =
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

        childMap child =
            case String.toInt <| .name <| Tree.label child of
                Just int ->
                    viewSector child <| color int <| alpha

                Nothing ->
                    none
    in
    column
        [ height <| px 900
        , width (fill |> minimum 500)
        , spacing 5
        , scrollbarY
        , alignTop
        ]
    <|
        List.map childMap <|
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
                ]
                { onChange = \new -> UpdateFocus { node | title = new }
                , text = node.title
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
                , padding 10
                , height fill
                , width fill
                , htmlAttribute <| Html.Attributes.id <| String.concat [ "cell-", String.fromInt node.id ]
                , Event.onClick <| ChangeFocus { node | status = Selected }
                , Event.onDoubleClick <| ChangeFocus { node | status = Editing }
                ]
                [ paragraph
                    [ width fill
                    , Font.center
                    ]
                    [ text <| .title node ]
                , paragraph
                    [ alignRight
                    , alignBottom
                    , Font.size 10
                    , Font.color <| darkGrey 1.0
                    , padding 3
                    ]
                    [ if List.isEmpty node.content then
                        none

                      else
                        List.map .size node.content
                            |> List.foldl (+) 0
                            |> Filesize.format
                            |> text
                    ]
                ]



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



-- REQUESTS
-- IPFS OBJECT API REQUESTS TURNED INTO TASKS FOR CHAINING


turnToBytesPart : String -> String -> Encode.Value -> Http.Part
turnToBytesPart message mime json =
    Encode.encode 0 json
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
        |> Http.bytesPart message mime


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
        , resolver = Http.stringResolver <| expect linkDecoder
        , timeout = Nothing
        }
        |> Task.andThen
            (\link ->
                Task.succeed
                    [ { link
                        | mimetype = Mime.parseMimeType "text/plain"
                        , size = Bytes.width bytes
                        , preview =
                            if Bytes.width bytes < 5000 then
                                string

                            else
                                ""
                      }
                    ]
            )
        |> Task.attempt Uploading


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


ipfsRequest : Url -> Method -> Decode.Decoder a -> Task Http.Error a
ipfsRequest url method decoder =
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

        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Ok value ->
                    Ok value

                Err err ->
                    Err (Http.BadBody (Decode.errorToString err))


getData : Node -> Task Http.Error Data
getData node =
    let
        url =
            ipfsApiUrl ++ "object/get?arg=" ++ node.cid
    in
    ipfsRequest url "GET" (Decode.field "Data" Decode.string)


getNode : Hash -> Cmd Msg
getNode cid =
    getStats cid
        |> Task.andThen
            (\size -> Tree.tree { initNode | size = size, cid = cid } [] |> Task.succeed)
        |> Task.andThen (getTree 2)
        |> Task.attempt UpdateZipper


getChildren : Node -> Task Http.Error (Tree Node)
getChildren node =
    let
        updateTitle =
            \x data -> Tree.tree { x | title = data } []

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



-- DECODERS


nodeDecoder : Decode.Decoder Node
nodeDecoder =
    Decode.succeed Node
        |> required "Name" Decode.string
        |> required "Hash" Decode.string
        |> required "Size" Decode.int
        |> optional "Title" Decode.string ""
        |> hardcoded Completed
        |> hardcoded 0
        |> hardcoded []


mtypeDecoder : Decode.Decoder (Maybe Mime.MimeType)
mtypeDecoder =
    Decode.map Mime.parseMimeType (Decode.field "type" Decode.string)


sizeDecoder : Decode.Decoder Int
sizeDecoder =
    Decode.oneOf [ Decode.int, DecodeExtra.parseInt ]


linkDecoder : Decode.Decoder Link
linkDecoder =
    Decode.succeed Link
        |> optional "Name" Decode.string ""
        |> required "Hash" Decode.string
        |> optional "Size" sizeDecoder 0
        |> optional "MimeType" mtypeDecoder Nothing
        |> optional "Preview" Decode.string ""
        |> hardcoded 0
        |> hardcoded Completed


objectDecoder : Decode.Decoder Object
objectDecoder =
    Decode.succeed Object
        |> required "Links" (Decode.list nodeDecoder)
        |> required "Data" Decode.string


objectStatDecoder : Decode.Decoder ObjectStat
objectStatDecoder =
    Decode.succeed ObjectStat
        |> required "Hash" Decode.string
        |> required "NumLinks" Decode.int
        |> required "BlockSize" Decode.int
        |> required "LinksSize" Decode.int
        |> required "DataSize" Decode.int
        |> required "CumulativeSize" Decode.int


cidDecoder : Decode.Decoder Hash
cidDecoder =
    Decode.at [ "Cid", "/" ] Decode.string



-- ENCODERS


treeEncoder : Int -> Tree Node -> Value
treeEncoder i tree =
    let
        node =
            Tree.label tree
    in
    Encode.object <|
        [ ( "title", Encode.string node.title )
        , ( "cid", Encode.string node.cid )
        , ( "name", Encode.string node.name )
        , ( "links", Encode.list (treeEncoder (i + 1)) <| Tree.children tree )
        , ( "content", Encode.list contentEncoder node.content )
        ]
            ++ (if i == 1 then
                    [ ( "color", Encode.int <| Maybe.withDefault 9 <| String.toInt <| node.name ) ]

                else
                    []
               )


contentEncoder : Link -> Value
contentEncoder link =
    Encode.object
        [ ( "Name", Encode.string link.name )
        , ( "Size", Encode.int link.size )
        , ( "Hash", Encode.string link.cid )
        , ( "MimeType", Encode.string <| Mime.toString <| Maybe.withDefault (Mime.OtherMimeType "") link.mimetype )
        , ( "Preview", Encode.string link.preview )
        ]


objectEncoder : Data -> List Node -> Value
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
