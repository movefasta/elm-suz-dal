module View exposing (..)

import Models exposing (..)
import Msgs exposing (Msg)
import Commands exposing (..)

-- STYLISH ELEPHANTS LIBRARY
import Element as E exposing (Element, Attribute, minimum, px, spacing, padding, width, height, fill, fillPortion)
import Element.Events as Event
import Element.Input as Input
import Element.Border as Border
import Element.Font as Font exposing (Font)
import Element.Background as Background

-- EXTERNAL LIBRARIES
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes as HtA exposing (width, height)
import RemoteData exposing (WebData)
import Material.Icons.Image as Icon exposing (edit)
import Svg exposing (svg)
import DropZone exposing (dropZoneEventHandlers, isHovering)
import FileReader exposing (Error(..), FileRef, NativeFile, readAsTextFile)

view : Model -> Html Msg
view model =
    E.layout [ E.inFront <| renderDropZone model.dropZone ] <|
        E.column [ Font.size 12, spacing 10, padding 10 ]
            [ viewControls model
            , viewPath <| List.reverse model.path
            , E.row [ E.width fill ]
                [ E.column [ E.width ( fill |> E.maximum 170 ) ] 
                    [ viewLinksByType 1 model.node
                    , viewLinksByType 2 model.node
                    ]
                , E.column []
                    [ viewContent model.draft -- Show Files
                    , viewLinkProperties model.link
                    , maybeRemote viewRawDag model.raw_dag
                    ]
                ]
            ]

viewContent : List Link -> Element Msg
viewContent links =
    E.row [ spacing 5 ] <|
        List.foldl
            (\link list ->
                if (link.obj_type == 2) then
                    list ++ [ viewFile link ] 
                else list
                ) 
            [] links

viewFile : Link -> Element Msg
viewFile link =
    let
        link_src =
            { url = ("/ipfs/" ++ link.cid)
            , label = E.text link.name
            }
    in
    E.column [] <| 
        [ E.image [E.width (E.px 130)] 
            { src = link_src.url
            , description = "it's not an image" 
            }
        , E.el
            [ padding 2
            , Event.onClick <| Msgs.FileCat link.cid
            ]
          <| E.text "cat_file"
        ]

viewRawDag : Data -> Element Msg
viewRawDag raw_dag =
    E.el [ Font.size 10 ] 
        <| E.paragraph [ padding 5 ] [ E.text raw_dag ]

viewLinksByType : Int -> List Link -> Element Msg
viewLinksByType obj_type links =
    E.column [] <|
        List.foldl
            (\link list ->
                if (link.obj_type == obj_type) then
                    list ++ [ viewLink link ] 
                else list
                ) 
            [] links

viewLinkProperties : Link -> Element Msg
viewLinkProperties link =
    let
        div =
            case link.status of 
                Editing ->
                    Input.text
                        [ padding 5
                        , E.htmlAttribute <| HtA.id ("link-" ++ link.name)
                        , Event.onLoseFocus <| Msgs.UpdateLink link Completed
                        ]
                        { onChange = Just Msgs.UpdateData
                        , text = ""
                        , placeholder = Just <| Input.placeholder [] <| E.text "Текст"
                        , label = Input.labelLeft [] <| E.text "Editing"
                        }
                Completed -> 
                    E.el [] <| E.text link.name
    in
        E.column 
            [ spacing 5 ]
            [ E.el [ Event.onClick <| Msgs.UpdateLink link Editing ] <| div
            , E.el [] <| E.text link.cid
            ]

viewLink : Link -> Element Msg
viewLink link =
    let
        colorFill =
            case link.status of
                Editing -> Background.color Color.lightGrey
                _ -> case link.obj_type of
                    1 -> Background.color <| orange 0.7
                    2 -> Background.color <| green 0.7
                    _ -> Background.color <| Color.white
    in
        E.el
            [ E.width E.fill
            , E.pointer
            , colorFill
            , E.mouseOver [ Background.color <| Color.grayscale 0.4 ]
            , padding 10
            , Event.onClick <| Msgs.PreviewGet link
            , Event.onDoubleClick <| Msgs.AddNodeToPath (link.name, link.cid)
            ]
            <| E.paragraph [] [ E.text link.name ] 

viewPath : List Node -> Element Msg
viewPath path =
    E.row [] <|
        List.map
            (\(name, hash) ->
                Input.button 
                    [ padding 5
                    , E.mouseOver <| [ Background.color Color.lightGrey ]
                    ]
                    { label = E.text (name ++ " > ") 
                    , onPress = Just <| Msgs.DagGet name hash
                    }
            )
            path

viewControls : Model -> Element Msg
viewControls model =
    let
        style  =
            [ padding 5
            , Border.width 1
            , Border.color Color.darkGrey
            , E.mouseOver <| [ Background.color Color.lightGrey ]
            ]
     in
        E.row
        [ spacing 5, E.width fill ]
        [ Input.text
            [ padding 5
            , Event.onLoseFocus <| Msgs.PathInit model.hash 
            ]
            { onChange = Just Msgs.UpdateQuery
            , text = model.hash
            , placeholder = Just <| Input.placeholder [] <| E.text "Enter query hash here"
            , label = Input.labelLeft [ padding 5 ] <| E.text "Root Hash"
            }
        , Input.button
            style
            { onPress = Just <| Msgs.DagPut <| objectEncoder model.data model.node 
            , label = E.text "dag-cbor put"
            }
        , Input.button
            style
            { onPress = Just <| Msgs.DagGet "Home" model.hash 
            , label = E.text "dag get"
            }
        , Input.button
            style
            { onPress = Just <| Msgs.DagPutPB <| objectEncoder model.data model.node 
            , label = E.text "dag-pb put"
            }
        , Input.button
            style
            { onPress = Just <| Msgs.LsObjects model.hash 
            , label = E.text "ls"
            }
        ]

editIcon : Link -> Element Msg
editIcon link =
    E.el
        [ E.pointer
        , padding 5
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




viewData : Data -> Element Msg
viewData data =
    E.column
        [ Font.color Color.charcoal
        , Background.color Color.white
        , Font.family fontFamily      
        ]
        [ Input.multiline
            [ padding 5 ]
            { onChange = Just Msgs.UpdateData
            , placeholder = Just <| Input.placeholder [] <| E.text "Data" 
            , label = Input.labelLeft [] <| E.text "Data" 
            , text = "text field"
            , spellcheck = False
            }
        ]


maybeRemote : (a -> Element Msg) -> WebData a -> Element Msg
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


renderDropZone : DropZone.Model -> Element Msg
renderDropZone dropZoneModel =
    E.map Msgs.DnD (E.el (renderZoneAttributesInFront dropZoneModel) <| 
        E.el
            [ Font.size 20
            , E.centerX
            , E.centerY
            , Font.color Color.white
            , Font.bold ]
            <| E.text "DRAG HERE" )


renderZoneAttributes : DropZone.Model -> 
    List (E.Attribute (DropZone.DropZoneMessage (List NativeFile)))
renderZoneAttributes dropZoneModel =
    (if DropZone.isHovering dropZoneModel then
        renderStyle Color.darkGrey
        -- style the dropzone differently depending on whether the user is hovering
    else
        renderStyle Color.lightGrey
    )
        ++ -- add the necessary DropZone event wiring
           ( List.map E.htmlAttribute <| dropZoneEventHandlers FileReader.parseDroppedFiles )

renderZoneAttributesInFront : DropZone.Model -> 
    List (E.Attribute (DropZone.DropZoneMessage (List NativeFile)))
renderZoneAttributesInFront dropZoneModel =
    (if DropZone.isHovering dropZoneModel then
        renderStyleInFront 0.2
        -- style the dropzone differently depending on whether the user is hovering
    else
        renderStyleInFront 0
    )
        ++ -- add the necessary DropZone event wiring
           ( List.map E.htmlAttribute <| dropZoneEventHandlers FileReader.parseDroppedFiles )


renderStyle : Color -> List (E.Attribute a)
renderStyle color =
    [ E.width fill
    , E.height fill
    , Border.width 1
    , Border.color color
    , Border.dashed
    ]

renderStyleInFront : Float -> List (E.Attribute a)
renderStyleInFront transparency =
    [ E.width fill
    , E.height fill
    , E.alpha transparency
    , Background.color Color.black
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
onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg
            else
                Decode.fail "not ENTER"
    in
        Event.on "keydown" (Decode.andThen isEnter Event.keyCode)
-}