module Page.Tensor exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Hash)
import Api.Endpoint as Endpoint
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Bytes
import Bytes.Encode
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Element.Lazy exposing (lazy, lazy2)
import File exposing (File)
import File.Download
import File.Select
import Filesize
import Html.Attributes
import Http
import Json.Decode as Decode
import Json.Decode.Extra as DecodeExtra exposing (parseInt)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import List.Extra
import MimeType as Mime
import Page
import Page.Settings as Settings
import Result exposing (Result)
import Route exposing (Path, Route)
import Session exposing (Session)
import Task exposing (Task)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
import Url
import Url.Parser exposing ((</>), Parser, int, map, oneOf, s, string)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- INIT


pathlist : Path -> List Path
pathlist path =
    Page.processPath { path | location = List.reverse path.location } (\x -> x) []


pathlistWOcid : Path -> List Path
pathlistWOcid path =
    Page.processPath { path | location = List.reverse path.location } (\x -> x) []


init : Session -> Path -> ( Model, Cmd Msg )
init session path =
    let
        initpath =
            { path | location = [] }
    in
    ( { session = session
      , problems = []
      , root = path.cid
      , zipper = Zipper.fromTree <| initTree []
      , upload = []
      , path = path
      , hover = False
      , addtext = ""
      , content = []
      }
    , Cmd.batch
        [ Api.get (Endpoint.node initpath) (UpdateZipper (pathlist path)) linksDecoder
        , Api.storeSettings path
        ]
    )



-- CONSTANTS


initNode : List String -> Node
initNode location =
    { id = 0
    , status = Completed
    , name = "ROOT"
    , cid = ""
    , size = 0
    , description = "Кругозор"
    , mimetype = Nothing
    , color = 0
    , location = location
    }


initTree : List String -> Tree Node
initTree location =
    Tree.tree
        (initNode location)
        []



-- MODEL


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


type ValidatedField
    = Email
    | Password


type alias Model =
    { session : Session
    , problems : List Problem
    , root : Hash
    , zipper : Zipper Node
    , upload : List File
    , path : Route.Path
    , hover : Bool
    , addtext : String
    , content : List Node
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
    , location : List String
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
    | Loading
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


type alias Url =
    String


type alias Id =
    Int


type Msg
    = NoOp
    | UpdateQuery Hash
    | GetNode Hash
    | DagPut Encode.Value
    | UpdateZipper (List Path) (Result.Result Http.Error (List (Tree Node)))
    | GetRootHash (Result.Result Http.Error Hash)
    | GetContentHash (Result.Result Http.Error Hash)
    | GetNodeContent (Result.Result Http.Error (List Node))
    | AddText String
    | UpdateAddText String
    | ChangeFocus Node
    | UpdateFocus Node
    | DownloadAsJson String
    | Pick
    | GotFiles File (List File)
    | Content (Result.Result Http.Error (List Node))
    | Perform Action
    | DragEnter
    | DragLeave
    | GotSession Session.Session
    | ModifyZipper (Zipper Node)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        label =
            Zipper.label model.zipper

        currentPath =
            Session.path model.session
    in
    case msg of
        UpdateZipper paths result ->
            let
                indexed path =
                    List.indexedMap
                        (\idx val ->
                            let
                                node =
                                    Tree.label val
                            in
                            Tree.replaceLabel
                                { node | location = List.take (List.length path.location - 1) path.location ++ [ String.fromInt idx ] }
                                val
                        )

                newZipper path list =
                    Zipper.replaceTree (Tree.tree (Zipper.label model.zipper) <| indexed path list) model.zipper

                newFocus path list =
                    case Zipper.findFromRoot (\a -> path.location == a.location) (newZipper path list) of
                        Just zipper ->
                            zipper

                        Nothing ->
                            newZipper path list
            in
            case result of
                Ok list ->
                    case paths of
                        x :: xs ->
                            ( { model | zipper = newFocus x list }
                            , Api.get (Endpoint.node x) (UpdateZipper xs) linksDecoder
                            )

                        [] ->
                            let
                                getChildren path =
                                    List.indexedMap
                                        (\idx val ->
                                            let
                                                node =
                                                    Tree.label val
                                            in
                                            Tree.replaceLabel
                                                { node | location = path.location ++ [ String.fromInt idx ] }
                                                val
                                        )
                            in
                            ( { model | zipper = Zipper.replaceTree (Tree.tree (Zipper.label model.zipper) <| getChildren currentPath list) model.zipper }
                            , Api.get (Endpoint.content currentPath) GetNodeContent contentDecoder
                            )

                Err _ ->
                    ( model, Cmd.none )

        ChangeFocus node ->
            let
                newZipper =
                    case Zipper.findFromRoot (\x -> x.location == node.location) model.zipper of
                        Just zipper ->
                            zipper

                        Nothing ->
                            model.zipper

                newPath =
                    { cid = model.root, location = node.location }
            in
            ( { model
                | zipper = newZipper
                , content = []
                , session = Session.update model.session newPath
                , path = newPath
              }
            , Cmd.batch
                [ if node.status == Editing then
                    Task.attempt
                        (\_ -> NoOp)
                    <|
                        Dom.focus ("cell-" ++ String.fromInt node.id)

                  else
                    Api.get (Endpoint.content newPath) GetNodeContent contentDecoder
                , Api.storeSettings newPath
                , Api.get (Endpoint.node newPath) (UpdateZipper (pathlist newPath)) linksDecoder

                --, Api.get (Endpoint.node newPath) (UpdateZipper newPath) linksDecoder
                ]
            )

        -- just update zipper without any other actions
        ModifyZipper zipper ->
            ( { model | zipper = zipper }, Cmd.none )

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
                resolver =
                    Http.stringResolver <| expect <| Decode.andThen linkToNodeDecoder <| objLinkDecoder

                upload body =
                    Api.add (Http.multipartBody [ Http.filePart "file" body ]) resolver
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
                    , let
                        body =
                            Encode.list contentEncoder (list ++ model.content)
                      in
                      Api.put body GetContentHash
                    )

                Err _ ->
                    ( model, Cmd.none )

        UpdateQuery hash ->
            ( { model | root = hash }, Cmd.none )

        GetRootHash result ->
            case result of
                Ok cid ->
                    let
                        newPath =
                            { currentPath | cid = cid }
                    in
                    ( { model | root = cid, session = Session.update model.session newPath }
                    , Cmd.batch
                        [ Route.replaceUrl (Session.navKey model.session) (Route.Tensor <| Debug.log "newpath" newPath)
                        , Api.storeSettings newPath
                        ]
                    )

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
                    , let
                        body =
                            updatedZipper
                                |> Zipper.toTree
                                |> treeEncoder
                      in
                      Api.put body GetRootHash
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

        GetNode cid ->
            ( model
            , Api.get (Endpoint.node currentPath) (UpdateZipper [ currentPath ]) linksDecoder
            )

        DagPut value ->
            ( model
            , Api.put value GetRootHash
            )

        GotSession _ ->
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
                    Api.put (Encode.list contentEncoder links) GetContentHash
            )



--VIEW


view : Model -> { title : String, content : Element Msg }
view model =
    let
        label =
            Zipper.label model.zipper
    in
    { title = "Суз-Даль Сервис v0"
    , content =
        column
            [ Font.size 14
            , Font.family [ Font.typeface "Ubuntu-Regular", Font.sansSerif ]
            , spacing 10
            , padding 10
            , width fill
            , height fill
            ]
            [ viewControls model
            , row
                [ width fill
                , height fill
                ]
                [ viewZipper model.zipper model.root
                , viewContent model.addtext model.content <| label
                ]
            ]
    }


viewZipper : Zipper Node -> String -> Element Msg
viewZipper zipper root =
    let
        style =
            [ width fill
            , height fill
            ]

        viewLabel node =
            viewCell
                { cid = root, location = .location <| Zipper.label zipper }
                node
            <|
                colorCodeConverter node.color 0.7

        viewCrumbs zip acc =
            let
                label =
                    Zipper.label zip
            in
            case Zipper.parent zip of
                Just parent ->
                    viewCrumbs parent <|
                        (Zipper.children parent
                            |> List.map Tree.label
                            |> List.map (\node -> viewLabel node)
                            |> row style
                        )
                            :: acc

                Nothing ->
                    (el [ height shrink, padding 5, Font.size 24 ] <| text label.description) :: acc
    in
    column
        style
        [ column style <| viewCrumbs zipper []
        , row style <|
            List.map viewLabel <|
                List.map Tree.label <|
                    Zipper.children zipper
        ]


viewCell : Path -> Node -> Color -> Element Msg
viewCell path node color =
    case node.status of
        Editing ->
            Input.multiline
                [ height fill
                , width fill
                , spacing 7
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
                [ if List.member node.location (List.map .location <| pathlist path) then
                    Background.color color

                  else
                    Background.color <| lightGrey 1.0
                , Border.color <| white 1.0
                , Border.width 2
                , pointer
                , Font.color <| black 1.0
                , height fill
                , width fill
                , htmlAttribute <| Html.Attributes.id <| String.concat [ "cell-", String.fromInt node.id ]
                , Event.onClick <| ChangeFocus node
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
                    , Font.color <| black 1.0
                    , padding 2
                    ]
                    [ text <| Filesize.format node.size
                    , link
                        [ Border.color <| black 1.0
                        , Font.underline
                        , padding 2
                        ]
                        { url = Route.pathToUrl { cid = path.cid, location = node.location }
                        , label = text <| String.join "/" node.location
                        }
                    ]
                ]


viewNode : Node -> Element Msg
viewNode node =
    column
        [ width fill
        , spacing 2
        , Font.size 11
        ]
        [ Input.text
            [ width fill
            ]
            { onChange = \new -> UpdateFocus { node | description = new }
            , text = node.description
            , placeholder = Just <| Input.placeholder [] <| text "Описание ячейки"
            , label = Input.labelLeft [] <| text "Desc"
            }
        , Input.text
            [ width fill
            ]
            { onChange = \new -> UpdateFocus { node | cid = new }
            , text = node.cid
            , placeholder = Just <| Input.placeholder [] <| text "Идентификатор контента - хэш"
            , label = Input.labelLeft [] <| text "Cid"
            }
        , Input.text
            [ width fill
            ]
            { onChange = \new -> UpdateFocus { node | name = new }
            , text = node.name
            , placeholder = Just <| Input.placeholder [] <| text "Имя узла"
            , label = Input.labelLeft [] <| text "Name"
            }
        , Input.radioRow
            [ width fill
            ]
            { onChange = \new -> UpdateFocus { node | color = new }
            , selected = Just node.color
            , label = Input.labelLeft [] (text "Цвет")
            , options =
                let
                    option i x =
                        el
                            [ width <| px 20
                            , height <| px 20
                            , spacing 3
                            , Border.rounded 10
                            , Border.width 2
                            , Background.color <| colorCodeConverter i 1.0
                            , case x of
                                Input.Idle ->
                                    Border.color <| white 1.0

                                Input.Focused ->
                                    Border.color <| lightGrey 1.0

                                Input.Selected ->
                                    Border.color <| darkGrey 1.0
                            ]
                        <|
                            text ""
                in
                List.range 0 8
                    |> List.map
                        (\code ->
                            Input.optionWith code (option code)
                        )
            }
        ]


viewContent : String -> List Node -> Node -> Element Msg
viewContent string nodes node =
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
        [ height fill
        , width (fill |> maximum 700)
        , padding 10
        , spacing 5
        , scrollbarY
        , alignTop
        ]
        [ viewNode node
        , row
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
                { url = Endpoint.file node.cid
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
                , url = Endpoint.file node.cid
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
                        { src = Endpoint.file node.cid
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
            treeEncoder <| Zipper.toTree <| model.zipper
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
        , Input.button
            style
            { onPress = Just <| ModifyZipper <| addNode model.zipper
            , label = text "add node"
            }
        , Input.button
            style
            { onPress = Just <| ModifyZipper <| Maybe.withDefault model.zipper <| removeNode model.zipper
            , label = text "remove node"
            }
        ]


viewTable : Path -> Tree Node -> Element Msg
viewTable path tree =
    let
        alpha =
            1.0
    in
    column
        [ height fill
        , width (fill |> minimum 500)
        , spacing 5
        , alignTop
        ]
    <|
        List.map
            (\child ->
                viewSector path child <|
                    colorCodeConverter (.color <| Tree.label child) alpha
            )
        <|
            Tree.children tree


viewSector : Path -> Tree Node -> Color -> Element Msg
viewSector path tree color =
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
                        [ viewCell path child_node <| color ]
                            ++ (List.map
                                    (\x ->
                                        viewCell path (Tree.label x) <|
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
        [ spacing 3
        , width fill
        , height fill
        ]
        [ el
            [ Font.size 20
            , Font.color <| darkGrey 1.0
            , width fill
            , height fill
            ]
          <|
            viewCell path node color
        , column
            [ width fill
            , height fill
            ]
          <|
            List.map childMap <|
                Tree.children tree
        ]



-- REQUESTS


addText : String -> Cmd Msg
addText string =
    let
        bytes =
            string
                |> Bytes.Encode.string
                |> Bytes.Encode.encode

        body =
            Http.multipartBody [ Http.bytesPart "whatever" "text/plain" bytes ]

        resolver =
            Http.stringResolver <| expect <| Decode.andThen linkToNodeDecoder <| objLinkDecoder
    in
    Api.add body resolver
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



-- DECODERS


treeDecoder : Decode.Decoder (Tree Node)
treeDecoder =
    Decode.map2 Tree.tree nodeDecoder linksDecoder


nodeToTree : Decode.Decoder (Tree Node)
nodeToTree =
    Decode.map2 Tree.tree nodeDecoder (Decode.succeed [])


nodeDecoder : Decode.Decoder Node
nodeDecoder =
    Decode.succeed Node
        |> hardcoded 0
        |> hardcoded Completed
        |> required "name" Decode.string
        |> required "cid" (Decode.field "/" Decode.string)
        |> optional "size" sizeDecoder 0
        |> optional "description" Decode.string ""
        |> optional "mimetype" (Decode.map Mime.parseMimeType Decode.string) Nothing
        |> optional "color" Decode.int 0
        |> hardcoded []


lazyLinksDecoder : Decode.Decoder (List (Tree Node))
lazyLinksDecoder =
    Decode.field "links" (Decode.lazy (\_ -> Decode.list treeDecoder))


linksDecoder : Decode.Decoder (List (Tree Node))
linksDecoder =
    Decode.field "links" (Decode.list nodeToTree)


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
        |> hardcoded []


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

                6 ->
                    lightGrey

                7 ->
                    darkGrey

                8 ->
                    black

                _ ->
                    white
    in
    color alpha



-- ENCODERS


treeEncoder : Tree Node -> Encode.Value
treeEncoder tree =
    let
        node =
            Tree.label tree
    in
    Encode.object <|
        [ ( "description", Encode.string node.description )
        , ( "size", Encode.int node.size )
        , ( "cid"
          , Encode.object
                [ ( "/"
                  , Encode.string <|
                        if String.isEmpty node.cid then
                            "zdpuB2CQLxyv4vfsFPL17HYXtVrJjxRsAwXgP787w94hi7Gsk"

                        else
                            node.cid
                  )
                ]
          )
        , ( "name", Encode.string node.name )
        , ( "links", Encode.list treeEncoder <| Tree.children tree )
        , ( "color", Encode.int node.color )
        ]


contentEncoder : Node -> Encode.Value
contentEncoder node =
    Encode.object
        [ ( "name", Encode.string node.name )
        , ( "size", Encode.int node.size )
        , ( "cid", Encode.object [ ( "/", Encode.string node.cid ) ] )
        , ( "mimetype", Encode.string <| Mime.toString <| Maybe.withDefault (Mime.OtherMimeType "") node.mimetype )
        , ( "description", Encode.string node.description )
        ]



-- HELPERS


addNode : Zipper Node -> Zipper Node
addNode zipper =
    let
        label =
            Zipper.label zipper

        newName =
            [ String.fromInt <| List.length <| Zipper.children zipper ]
    in
    Zipper.append (initTree <| label.location ++ newName) zipper


removeNode : Zipper Node -> Maybe (Zipper Node)
removeNode zipper =
    Zipper.removeTree zipper


getPath : Zipper Node -> List Node -> List String
getPath zipper acc =
    let
        appendLabel =
            Zipper.label zipper :: acc
    in
    case Zipper.parent zipper of
        Just parent ->
            getPath parent appendLabel

        Nothing ->
            List.map .name acc


getCrumbs : Zipper Node -> List Node -> List Node
getCrumbs zipper acc =
    let
        appendLabel =
            Zipper.label zipper :: acc
    in
    case Zipper.parent zipper of
        Just parent ->
            getCrumbs parent appendLabel

        Nothing ->
            acc



{-
   getFocus : Zipper Node -> List String -> Zipper Node
   getFocus zipper list =
       case list of
           x :: xs ->
               case Zipper.findNext (\a -> a == (.name <| Zipper.label zipper)) zipper of
                   Just child ->
                       if (.name <| Zipper.label child) == Debug.log "step" x then
                           getFocus child xs

                       else
                           child

                   Nothing ->
                       let
                           label =
                               Zipper.label zipper
                       in
                       Zipper.replaceLabel { label | status = Completed } zipper

           [] ->
               zipper
-}
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


black : Float -> Color
black alpha =
    E.rgba255 0 0 0 alpha
