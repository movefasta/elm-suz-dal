module View exposing (..)

import Html exposing (Html)
import Html.Attributes as HtA exposing (width, height)
import Models exposing (..)
import Msgs exposing (Msg)
import RemoteData exposing (WebData)
import Json.Decode as Decode
import Color
import Element as E exposing (Attribute, Element)
import Element.Attributes exposing (..)
import Element.Events as Event
import Element.Input as Input
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Commands exposing (..)
import Material.Icons.Image as Icon exposing (edit)
import Svg exposing (svg)
import DropZone exposing (dropZoneEventHandlers, isHovering)
import FileReader exposing (Error(..), FileRef, NativeFile, readAsTextFile)


view : Model -> Html Msg
view model =
    E.layout stylesheet <|
        E.column None [ padding 20, spacing 10 ] <|
            [ E.el None [] <| viewControls model
            , E.el None [] <| viewPath <| List.reverse model.path
            , E.row None [ spacing 20 ]
                [ E.column None
                    [ spacing 5 ]
                    [ viewLinksByType 1 model.node -- Show Directories
                    ]
                , E.column None
                    [ spacing 5 ]
                    [ viewContent model.draft -- Show Files
                    , E.el None [] <| viewLinkProperties model.link
                    , E.el None [] <| maybeRemote viewRawDag model.raw_dag
                    , E.el None [] <| E.html <| renderDropZone model.dropZone
                    ]
                ]
            ]

viewContent : List Link -> Element Styles variation Msg
viewContent links =
    E.row None [ spacing 5, Element.Attributes.width fill ] <|
        List.foldl
            (\link list ->
                if (link.obj_type == 2) then
                    list ++ [ viewFile link ] 
                else list
                ) 
            [] links

viewFile : Link -> Element Styles variation Msg
viewFile link =
    let
        link_src =
            ("/ipfs/" ++ link.cid)
    in
    E.column None [] <| 
        [ E.newTab link_src <| E.image None [Element.Attributes.width (px 150)] 
            { src = link_src
            , caption = "it's not an image" 
            }
        , E.button None
            [ padding 2
            , Event.onClick <| Msgs.FileCat link.cid
            ]
          <| E.text "cat_file"
        ]

viewRawDag : Data -> Element Styles variation Msg
viewRawDag raw_dag =
    E.el RawData [] <| E.paragraph None [ padding 5 ] <| [ E.text raw_dag ]

viewLinksByType : Int -> List Link -> Element Styles variation Msg
viewLinksByType obj_type links =
    E.column None [ spacing 5 ] <|
        List.foldl
            (\link list ->
                if (link.obj_type == obj_type) then
                    list ++ [ viewLink link ] 
                else list
                ) 
            [] links

viewLinkProperties : Link -> Element Styles variation Msg
viewLinkProperties link =
    let
        div =
            case link.status of 
                Editing ->
                    Input.text None
                        [ padding 5
                        , id ("link-" ++ link.name)
                        , onEnter <| Msgs.UpdateLink link Completed
                        , Event.onBlur <| Msgs.UpdateLink link Completed
                        ]
                        { onChange = Msgs.UpdateData
                        , value = link.name
                        , label = 
                            Input.placeholder { label = Input.hiddenLabel <| "", text = "Текст" }
                        , options = []
                        }
                Completed -> 
                    E.el Cell [ padding 5 ] <| E.text link.name
    in
        E.column None 
            [ spacing 5 ]
            [ E.el Cell [ Event.onClick <| Msgs.UpdateLink link Editing ] <| div
            , E.el None [ padding 5 ] <| E.text link.cid
            ]


viewLink : Link -> Element Styles variation Msg
viewLink link =
    let
        style =
            case link.status of
                Editing -> CheckedCell
                _ -> case link.obj_type of
                    1 -> Red
                    2 -> Green
                    _ -> Cell
    in
        E.el style
                [ padding 10
                , minWidth (px 130)
                , Event.onClick <| Msgs.PreviewGet link
                , Event.onDoubleClick <| Msgs.AddNodeToPath (link.name, link.cid)
                ]
                <| E.paragraph None [] [ E.text link.name ]

viewPath : List Node -> Element Styles variation Msg
viewPath path =
    E.row None [] <|
        List.map
            (\(name, hash) ->
                E.button None 
                    [ Event.onClick <| Msgs.DagGet name hash ] 
                    <| E.text (name ++ " > ")
            )
            path


viewControls : Model -> Element Styles variation Msg
viewControls model =
    E.row Main
        [ spacing 5 ]
        [ Input.text None
            [ padding 5, onEnter <| Msgs.PathInit model.hash ]
            { onChange = Msgs.UpdateQuery
            , value = model.hash
            , label = 
                Input.placeholder
                    { label = Input.labelLeft (E.el None [ verticalCenter ] (E.text "Root hash"))
                    , text = "" }
                    , options = []
                    }
        , E.button Cell
            [ padding 5
            , Event.onClick <| Msgs.DagPut <| objectEncoder model.data model.node
            ]
          <| E.text "dag-cbor put"
        , E.button Cell
            [ padding 5
            , Event.onClick <| Msgs.DagGet "Home" model.hash
            ]
          <| E.text "dag get"
        , E.button Cell
            [ padding 5
            , Event.onClick <| Msgs.DagPutPB <| objectEncoder model.data model.node
            ]
          <| E.text "dag-pb put"
        , E.button Cell
            [ padding 5
            , Event.onClick <| Msgs.LsObjects model.hash
            ]
          <| E.text "ls"
        ]

editIcon : Link -> Element Styles variation Msg
editIcon link =
    E.el EditIcon
        [ padding 5
        , Event.onClick <| Msgs.UpdateLink link Editing
        ] 
        <| E.html 
        <| Svg.svg 
            [ HtA.width 18
            , HtA.height 18
            ]
            [ Icon.edit Color.black 18 ]


findLinkByName : Name -> WebData Object -> Maybe Link
findLinkByName link_name object =
    case RemoteData.toMaybe object of
        Just object ->
            object.links
                |> List.filter (\link -> link.name == link_name)
                |> List.head

        Nothing ->
            Nothing


onEnter : Msg -> Attribute variaton Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg
            else
                Decode.fail "not ENTER"
    in
        Event.on "keydown" (Decode.andThen isEnter Event.keyCode)


viewData : Data -> Element Styles variation Msg
viewData data =
    E.column Main
        [ spacing 5 ]
        [ Input.multiline None
            [ padding 5 ]
            { onChange = Msgs.UpdateData
            , value = data
            , label =
                Input.placeholder
                    { label = Input.labelLeft (E.el None [ verticalCenter ] (E.text "Data"))
                    , text = "Введите данные узла"
                    }
            , options = []
            }
        ]


maybeRemote : (a -> Element Styles variation Msg) -> WebData a -> Element Styles variation Msg
maybeRemote viewFunction response =
    case response of
        RemoteData.NotAsked ->
            E.text ""

        RemoteData.Loading ->
            E.text "Loading..."

        RemoteData.Success object ->
            viewFunction object

        RemoteData.Failure error ->
            E.text (toString error)


renderDropZone : DropZone.Model -> Html Msg
renderDropZone dropZoneModel =
    Html.map Msgs.DnD
        (Html.div (renderZoneAttributes dropZoneModel) [])


renderZoneAttributes : DropZone.Model -> List (Html.Attribute (DropZone.DropZoneMessage (List NativeFile)))
renderZoneAttributes dropZoneModel =
    (if DropZone.isHovering dropZoneModel then
        dropZoneHover
        -- style the dropzone differently depending on whether the user is hovering
     else
        dropZoneDefault
    )
        :: -- add the necessary DropZone event wiring
           dropZoneEventHandlers FileReader.parseDroppedFiles

-- STYLES

fontFamily : List Font
fontFamily =
    [ Font.font "Source Sans Pro" 
    , Font.font "Trebuchet MS"
    , Font.font "Lucida Grande"
    , Font.font "Bitstream Vera Sans"
    , Font.font "Helvetica Neue"
    , Font.font "sans-serif"
    ]


type Styles
    = None
    | Main
    | Navigation
    | Cell
    | Red
    | Green
    | CheckedCell
    | EditIcon
    | DragHover
    | RawData

stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style None []
        , style Main
            [ Font.size 12
            , Color.text Color.charcoal
            , Color.background Color.white
            , Font.typeface fontFamily      
            ]
        , style Cell
            [ cursor "pointer"
            , hover [ Color.background <| Color.grayscale 0.1 ]
            ]
        , style CheckedCell
            [ Border.right 2.0
            , Border.solid
            , Color.border Color.black
            ]
        , style Green
            [ Color.text Color.white
            , Color.background Color.green
            , hover 
                [ Color.background <| Color.black ] 
            ]
        , style Red 
            [ Color.text Color.white
            , Color.background Color.red 
            , hover 
                [ Color.background <| Color.black ]
            ]
        , style EditIcon
            [ cursor "pointer" ]
        , style DragHover
            [ Color.background <| Color.grayscale 0.1 ]
        , style RawData
            [ Font.size 10 ]
        ]


dropZoneDefault : Html.Attribute a
dropZoneDefault =
    HtA.style
        [ ( "height", "120px" )
        , ( "border-radius", "10px" )
        , ( "border", "3px dashed steelblue" )
        ]


dropZoneHover : Html.Attribute a
dropZoneHover =
    HtA.style
        [ ( "height", "120px" )
        , ( "border-radius", "10px" )
        , ( "border", "3px dashed red" )
        ]
