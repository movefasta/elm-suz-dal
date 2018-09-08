module View exposing (..)

import Commands exposing (..)

-- STYLISH ELEPHANTS
import Element as E exposing (..)
import Element.Events as Event
import Element.Input as Input
import Element.Border as Border
import Element.Font as Font exposing (Font)
import Element.Background as Background

-- EXTERNAL LIBRARIES
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes exposing (id, style, width, height)
import RemoteData exposing (WebData)
import Material.Icons.Image as Icon exposing (edit)
import Svg exposing (svg)
import DropZone exposing (dropZoneEventHandlers, isHovering)
import FileReader exposing (Error(..), FileRef, NativeFile, readAsTextFile)

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
            ]
            [ viewControls model
            , viewPath model.node []
            , row
                [ width fill ]
                [ column
                    [ width ( fill |> maximum 170 ) ] 
                    [ viewTable model.node ]
                , column
                    [ spacing 10 ]
                    [ renderDropZone model.dropZone
                    , viewContent model.content -- Show Files
                    , viewLinkProperties model model.link
                    , maybeRemote viewRawDag model.raw_dag
                    ]
                ]
            ]

viewContent : List Link -> Element Msg
viewContent links =
    row [ spacing 5 ] links

viewFile : Link -> Element Msg
viewFile link =
    let
        link_src =
            { url = ("/ipfs/" ++ link.cid)
            , label = text link.name
            }
    in
    column [] <| 
        [ image [ width (px 130) ]
            { src = link_src.url
            , description = "it's not an image" 
            }
        , el
            [ padding 2
            , Event.onClick <| Msgs.FileCat link.cid
            ]
          <| text "cat_file"
        ]

viewRawDag : Data -> Element Msg
viewRawDag raw_dag =
    el 
        [ Font.size 10
        , htmlAttribute Html.Attributes.style "overflow-wrap" "break-word" 
        ] <| 
        paragraph [ padding 5 ] [ text raw_dag ]

viewLinkProperties : Model -> Link -> Element Msg
viewLinkProperties model link =
    let
        div =
            case link.status of 
                Editing ->
                    Input.text
                        [ padding 5
                        , htmlAttribute <| Html.Attributes.id ("link-" ++ link.name)
                        , Event.onLoseFocus <| Msgs.UpdateLink link Completed
                        , Font.size 20
                        ]
                        { onChange = Just Msgs.UpdateData
                        , text = model.data
                        , placeholder = Just <| Input.placeholder [] <| text "Текст"
                        , label = Input.labelLeft [] <| text "Editing"
                        }
                Completed -> 
                    el [] <| text link.name
    in
        column 
            [ spacing 5 ]
            [ el [ Event.onClick <| Msgs.UpdateLink link Editing ] <| div
            , el [] <| text link.cid
            ]

viewCell : Color -> Node -> Element Msg
viewCell color node =
    paragraph
        [ Background.color color
        , centerX
        , centerY
        , padding 10
        , htmlAttribute Html.Attributes.style "overflow-wrap" "break-word"
        ] <|
        text node.title

viewSector : Node -> Element Msg
viewSector node ->
    let
        color i =
            case i of
                0 -> white
                1 -> orange
                2 -> yellow
                3 -> green
                4 -> cyan
                5 -> blue
                6 -> violet

        childMap child =
            case ( String.toInt child.name ) of
                Ok int ->
                    row [ Background.color Color.white ]
                        [ el [] <| text node.title
                        , row
                            [ spacing 5
                            , padding 5
                            ] <|
                            viewRow List.map (\x -> viewCell (color int) x) child.children
                        ]
                Err _ ->
                    none
    in
    column 
        [ Border.width 1
        , Border.color Color.darkGrey
        ] 
        [ el 
            [ Font.size 
            , centerX
            , centerY
            ] <|
            text node.title
        , column [] List.map childMap node.children
        ]

viewTable : Node -> Element Msg
viewTable node =
    let
        childMap child =
            case ( String.toInt child.name ) of
                Ok int ->
                    viewSector child
                Err _ ->
                    none
    in
    column [ spacing 15 ] <| List.map childMap node.children

viewCell : Link -> Element Msg
viewCell link =
    el
        [ width fill
        , pointer
        , colorFill
        , mouseOver [ Background.color <| Color.grayscale 0.2 ]
        , padding 10
        , Event.onClick <| Msgs.PreviewGet link
        , Event.onDoubleClick <| Msgs.UpdateQuery link.name
        ] <|
        paragraph [] [ text link.name ] 

-- navigation in breadcrumbs

viewPath : Node -> List (Element Msg) -> Element Msg
viewPath node acc =
    let
        div =
            Input.button 
                [ padding 5
                , mouseOver <| [ Background.color Color.lightGrey ]
                ]
                { label = text (node.title ++ " > ")
                , onPress = Just <| Msgs.GetNode node.cid
                }
    in
    case node.parent of
        Just parent ->
            viewPath parent ([ div ] :: acc)
        Nothing ->
            row [] acc

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
            , Event.onLoseFocus <| Msgs.PathInit model.hash
            ]
            { onChange = Just Msgs.UpdateQuery
            , text = model.hash
            , placeholder = Just <| Input.placeholder [] <| text "Enter query hash here"
            , label = Input.labelLeft [ padding 5 ] <| text "Root Hash"
            }
        , Input.button
            style
            { onPress = Just <| Msgs.DagPut <| objectEncoder model.data model.node 
            , label = text "dag-cbor put"
            }
        , Input.button
            style
            { onPress = Just <| Msgs.DagGet "Home" model.hash 
            , label = text "dag get"
            }
        , Input.button
            style
            { onPress = Just <| Msgs.DagPutPB <| dagNodePbEncoder model.node 
            , label = text "dag-pb put"
            }
        , Input.button
            style
            { onPress = Just <| Msgs.LsObjects model.hash 
            , label = text "ls"
            }
        ]

editIcon : Link -> Element Msg
editIcon link =
    el
        [ pointer
        , padding 5
        , Event.onClick <| Msgs.UpdateLink link Editing
        ] 
        <| html 
        <| Svg.svg 
            [ Html.Attributes.width 18
            , Html.Attributes.height 18
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

viewData : Data -> Element Msg
viewData data =
    column
        [ Font.color Color.charcoal
        , Background.color Color.white
        , Font.family fontFamily      
        ]
        [ Input.multiline
            [ padding 5 ]
            { onChange = Just Msgs.UpdateData
            , placeholder = Just <| Input.placeholder [] <| text "Data" 
            , label = Input.labelLeft [] <| text "Data" 
            , text = "text field"
            , spellcheck = False
            }
        ]

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


renderDropZone : DropZonModel -> Element Msg
renderDropZone dropZoneModel =
    map Msgs.DnD (el (renderZoneAttributes dropZoneModel) <| 
        el
            [ Font.size 20
            , centerX
            , centerY
            , Font.color Color.white
            , Font.bold ]
            <| text "DRAG HERE" )

renderZoneAttributes : DropZonModel -> 
    List (Attribute (DropZonDropZoneMessage (List NativeFile)))
renderZoneAttributes dropZoneModel =
    (if DropZonisHovering dropZoneModel then
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

alpha : Float
alpha = 1.0

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

viewPath : List Node -> Element Msg
viewPath path =
    row [] <|
        List.map
            (\(name, hash) ->
                Input.button 
                    [ padding 5
                    , mouseOver <| [ Background.color Color.lightGrey ]
                    ]
                    { label = text (name ++ " > ") 
                    , onPress = Just <| Msgs.DagGet name hash
                    }
            )
            path
renderStyleInFront : Float -> List (Attribute a)
renderStyleInFront transparency =
    [ width fill
    , height fill
    , alpha transparency
    , Background.color Color.black
    ]

onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decodsucceed msg
            else
                Decodfail "not ENTER"
    in
        Event.on "keydown" (DecodandThen isEnter Event.keyCode)

renderZoneAttributesInFront : DropZonModel -> 
    List (Attribute (DropZonDropZoneMessage (List NativeFile)))
renderZoneAttributesInFront dropZoneModel =
    (if DropZonisHovering dropZoneModel then
        renderStyleInFront 0.2
        -- style the dropzone differently depending on whether the user is hovering
    else
        renderStyleInFront 0
    )
        ++ -- add the necessary DropZone event wiring
           ( List.map htmlAttribute <| dropZoneEventHandlers FileReader.parseDroppedFiles )
-}