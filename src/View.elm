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
import Commands exposing (objectEncoder)
import Material.Icons.Image as Icon exposing (edit)
import Svg exposing (svg)


view : Model -> Html Msg
view model =
    E.layout stylesheet <|
        E.column None [ padding 20, spacing 10 ] <|
            [ E.el None [] <| viewControls model
            , E.el None [] <| viewPath model.path
            , E.row None [ spacing 20 ]
                [ E.column None
                    [ spacing 5 ]
                    [ viewLinks model.node
                    ]
                , E.column None
                    [ spacing 5 ]
                    [ E.el None [] <| viewLinkProperties model.link
                    , E.el None [] <| maybeRemote viewRawDag model.raw_dag
                    ]
                ]
            ]

viewRawDag : Data -> Element Styles variation Msg
viewRawDag raw_dag =
    E.el None [] <| E.paragraph None [ padding 5 ] <| [ E.text raw_dag ]

viewLinks : List Link -> Element Styles variation Msg
viewLinks links =
    E.column None [ spacing 5 ] <|
        List.concat
            [ (List.map (\link -> viewLink link) links) ]

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
                        { onChange = Msgs.UpdateDescription
                        , value = link.description
                        , label = 
                            Input.placeholder { label = Input.hiddenLabel <| "", text = "Текст" }
                        , options = []
                        }
                Completed -> 
                    E.el Cell [ padding 5 ] <| E.text link.description
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
                _ -> case link.name of
                    "0" -> Red
                    "1" -> Green
                    _ -> Cell
    in
        E.row style [ spacing 5 ]
            [ E.el None
                [ padding 10
                , minWidth (px 130)
                , Event.onClick <| Msgs.PreviewGet link
                , Event.onDoubleClick <| Msgs.DagGet link.name link.cid
                ]
                <| E.paragraph None [] [ E.text link.description ]
            , editIcon link
            ]

viewPath : List Node -> Element Styles variation Msg
viewPath path =
    E.row None [] <|
        List.foldr
            (\( name, hash ) list ->
                [ E.button None 
                    [ Event.onClick <| Msgs.DagGet name hash ] 
                    <| E.text (name ++ " > ")
                ]
                    ++ list
            )
            []
            path


viewControls : Model -> Element Styles variation Msg
viewControls model =
    E.row Main
        [ spacing 5 ]
        [ Input.text None
            [ padding 5, onEnter <| Msgs.DagGet "Home" model.hash ]
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
          <| E.text "dag put"
        , E.button Cell
            [ padding 5
            , Event.onClick <| Msgs.DagGet "Home" model.hash
            ]
          <| E.text "dag get"
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
