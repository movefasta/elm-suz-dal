module View exposing (..)

import Html exposing (Html, div, tr, td, a, input, iframe, text)
import Html.Events exposing (..)
import Models exposing (..)
import Msgs exposing (Msg)
import RemoteData exposing (WebData)
import Json.Decode as Decode
import String.Extra exposing (toCodePoints, fromCodePoints)
--import Commands exposing (ipfsGatewayUrl, pathForUrl, pathUpdate)
import Color
import Element as E exposing (Attribute, Element)
import Element.Attributes exposing (..)
import Element.Events as Event
import Element.Input as Input
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
--import Style.Transition as Transition


view : Model -> Html Msg
view model =
    E.layout stylesheet <|
        E.column None
            []
            [ E.el None [ center, width (px 800) ] <|
                E.column Main
                    [ spacing 50, paddingTop 50, paddingBottom 50 ]
                    [ viewControls model
                    , E.html <| viewPath model.path
                    , maybeRemote viewObject model.object
                    ]
            ]


viewObject : Object -> Element Styles variation Msg
viewObject object =
    E.column Main [] <|
        List.concat 
        [ [ toCodePoints object.data
                    |> List.foldr removeUTFControlChars []
                    |> fromCodePoints
                    |> E.text ]
        , ( List.map ( \link -> viewLink link ) object.links )
        ]


viewPath : List Node -> Html Msg
viewPath path =
    div [] <| List.foldr (\(name, hash) list -> 
                [ a [ onClick 
                <| Msgs.GetObjectRequest name hash ] [ Html.text name ] ] ++ [ Html.text " > "] ++ list) [] path


viewControls : Model -> Element Styles variation Msg
viewControls model =
    E.row Main [ spacing 20 ]
        [ Input.text None 
            [ padding 10 ]
            { onChange = Msgs.UpdateQuery
            , value = model.hash
            , label = Input.placeholder
                            { label = Input.labelLeft (E.el None [ verticalCenter ] (E.text "Hash"))
                            , text = "Введите адрес объекта"
                            }
            , options = [ Input.errorBelow (E.el Main [] (E.text "This is an Error!")) ]
            }
        , Input.text None
            [ padding 10 ]
            { onChange = Msgs.UpdateData
            , value = model.hash
            , label = Input.placeholder
                            { label = Input.labelLeft (E.el None [ verticalCenter ] (E.text "Data"))
                            , text = "Введите данные объекта"
                            }
            , options = [ Input.errorBelow (E.el Main [] (E.text "This is an Error!")) ]
            }
        , E.button Button
            [ padding 10
            , Event.onClick <| Msgs.GetObjectRequest "Home" model.hash
            ]
            <| E.text "Get"
        , E.button Button
            [ padding 10
            , Event.onClick <| Msgs.SetDataRequest
            ]
            <| E.text "Set Data"
        ]

viewLink : Link -> Element Styles variation Msg
viewLink link =
    E.button None
            [ padding 10
            , Event.onClick <| Msgs.GetObjectRequest link.name link.hash
            ]
            <| E.text link.name
{-}
    a [ onClick <| Msgs.GetObjectRequest link.name link.hash ] [ text link.name ]
    text ( link.name ++ "  " ++ toString link.size ) ]
    a [ onClick <| Msgs.RemoveLink link ] [ text "rm-link" ]
-}

findLinkByName : Name -> WebData Object -> Maybe Link
findLinkByName link_name object =
    case RemoteData.toMaybe object of
        Just object ->
            object.links
                |> List.filter (\link -> link.name == link_name)
                |> List.head

        Nothing ->
            Nothing

{-|
viewPureData : String -> Html Msg
viewPureData data =
    iframe [ srcdoc data, style [("height", "100%")] ] [] 
-}

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
            , Font.typeface sansSerif
            , Font.size 16
            , Font.lineHeight 1.3 -- line height, given as a ratio of current font size.
            ]
        , style Button
            [ Border.rounded 5
            , Border.all 1
            , Border.solid
            , Color.border Color.blue
            , Color.background Color.lightBlue
            ]
        ]

type Styles
    = None
    | Main
    | Button


{-
viewText t =
    [ E.el Main [] (text "First, Some Text")
    , E.textLayout None
        [ spacingXY 25 25
        , padding 60
        ]
        [ E.el Main
            [ width (px 200)
            , height (px 300)
            , alignLeft
            ]
            (text t)
        ]
    ]
-}