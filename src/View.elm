module View exposing (..)

import Html exposing (Html)
import Html.Events exposing (..)
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


view : Model -> Html Msg
view model =
    E.layout stylesheet <|
        E.column None [ center, width (px 800) ] <|
            [ E.el None [ center, width (px 800) ] <| viewControls model
            , E.row Main [ spacing 5 ] <| 
                [ E.column None
                        [ spacing 20, width (px 400) ]
                        [ viewPath model.path
                        , maybeRemote viewObject model.object
                        ]
                , E.el DagJson
                        [ spacing 20, width (px 400) ]
                        <| maybeRemote viewRawDag model.raw_dag
                ]
            ]


viewObject : Object -> Element Styles variation Msg
viewObject object =
    E.column Main [ spacing 5 ] <| 
        ( List.map ( \link -> viewLink link ) object.links )


viewRawDag : Data -> Element Styles variation Msg
viewRawDag raw_dag =
    --E.paragraph DagJson [ padding 5 ] <| [ E.text raw_dag ]
    E.column Main [ spacing 20 ]
        [ Input.multiline None
            [ padding 5 ]
            { onChange = Msgs.UpdateData
            , value = raw_dag
            , label = Input.placeholder
                            { label = Input.labelLeft (E.el None [ verticalCenter ] (E.text "Data"))
                            , text = "Введите данные объекта"
                            }
            , options = []
            }
        ]



viewLink : Link -> Element Styles variation Msg
viewLink link =
    E.row Main [ spacing 5 ] 
        [ E.button None
            [ padding 5
            , Event.onClick <| Msgs.GetObjectRequest link.name link.hash
            ]
            <| E.text link.name
        , E.button None
            [ padding 5
            , Event.onClick <| Msgs.RemoveLink link
            ]
            <| E.text "Удалить"

        ]


viewPath : List Node -> Element Styles variation Msg
viewPath path =
    E.row Main [] <|
        List.foldr (\(name, hash) list -> 
            [ E.button Navigation [ Event.onClick <| Msgs.GetObjectRequest name hash ]
                <| E.text (name ++ " > ") ] ++ list) [] path


viewControls : Model -> Element Styles variation Msg
viewControls model =
    E.row Main [ spacing 20 ]
        [ Input.text None 
            [ padding 5 ]
            { onChange = Msgs.UpdateQuery
            , value = model.hash
            , label = Input.placeholder
                            { label = Input.labelLeft (E.el None [ verticalCenter ] (E.text "Hash"))
                            , text = "Введите адрес объекта"
                            }
            , options = []
            }
        , E.button Button
            [ padding 5
            , Event.onClick <| Msgs.GetObjectRequest "Home" model.hash
            ]
            <| E.text "get object"
        , E.button Button
            [ padding 5
            , Event.onClick <| Msgs.DagPut <| objectEncoder <| RemoteData.withDefault {data = "", links = []} model.object
            ]
            <| E.text "set data"
        , E.button Button
            [ padding 5
            , Event.onClick <| Msgs.DagGet model.hash
            ]
            <| E.text "dag get"

        ]



findLinkByName : Name -> WebData Object -> Maybe Link
findLinkByName link_name object =
    case RemoteData.toMaybe object of
        Just object ->
            object.links
                |> List.filter (\link -> link.name == link_name)
                |> List.head

        Nothing ->
            Nothing


maybeRemote : ( a -> Element Styles variation Msg ) -> WebData a -> Element Styles variation Msg
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


onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg
            else
                Decode.fail "not ENTER"
    in
        on "keydown" (Decode.andThen isEnter keyCode)


-- STYLES

sansSerif : List Font
sansSerif =
    [ Font.font "helvetica"
    , Font.font "arial"
    , Font.font "sans-serif"
    ]

stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style None [] -- It's handy to have a blank style
        , style Main
            [ Border.all 1 -- set all border widths to 1 px.
            , Color.text Color.darkCharcoal
            , Color.background Color.white
            , Color.border Color.lightGrey
            , Style.prop "font-family" "'Source Sans Pro', 'Trebuchet MS', 'Lucida Grande', 'Bitstream Vera Sans', 'Helvetica Neue', sans-serif"
            ]
        , style Button
            [ Border.rounded 5
            , Border.all 1
            , Border.solid
            , Color.border Color.blue
            , Color.background Color.lightBlue
            ]
        , style Navigation
            [ Color.background Color.white 
            ]
        , style DagJson
            [ Style.prop "white-space" "normal" ]
        ]

type Styles
    = None
    | Main
    | Button
    | Navigation
    | DagJson

{-
-- HELPER FUNCTIONS

listUnicodeCodes : String -> Html Msg
listUnicodeCodes data =
    E.layout stylesheet <| E.text <| List.foldr intToStringFoldFun "" <| toCodePoints data


intToStringFoldFun: Int -> String -> String
intToStringFoldFun a b =
    ( toString a ) ++ ", " ++ b


removeUTFControlChars: Int -> List Int -> List Int
removeUTFControlChars a b =
    case ( a > 31 ) && ( a < 65500 ) of
        True -> [ a ] ++ b
        False -> b
-}