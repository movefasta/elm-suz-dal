module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (..)
import Msgs exposing (Msg)
import RemoteData exposing (WebData)
import Json.Decode as Decode
import String.Extra exposing (toCodePoints, fromCodePoints)


view : Model -> Html Msg
view model =
    case model.currentRoute of
        HashRoute hash -> 
            div [] [ viewControls model ]

        LinkRoute hash name ->
            case findLinkByName name model.object of
                Just link ->
                    div [] [ maybeRemote viewObject model.object ]

                Nothing ->
                    notFoundView

        NotFoundRoute ->
            notFoundView


findLinkByName : Name -> WebData Object -> Maybe Link
findLinkByName link_name object =
    case RemoteData.toMaybe object of
        Just object ->
            object.links
                |> List.filter (\link -> link.name == link_name)
                |> List.head

        Nothing ->
            Nothing


notFoundView : Html msg
notFoundView =
    h3 [] [ text "Oops! The page you requested was not found!" ]


viewControls : Model -> Html Msg
viewControls model =
    div []
        [ input [ onInput Msgs.UpdateQuery, value model.hash ] []
        , button [ onClick <| Msgs.GetObjectRequest model.hash ] [ text "Get Object Data" ]
        , input [ onInput Msgs.UpdateData, value model.data, onEnter Msgs.SetDataRequest ] []
        , button [ onClick Msgs.SetDataRequest ] [ text "Set Object Data" ]
        ]


viewPureData : String -> Html Msg
viewPureData data =
    iframe [ srcdoc data, style [("height", "100%")] ] [] 


viewObject : Object -> Html Msg
viewObject object =
    div []
        [ div [] [ toCodePoints object.data
                    |> List.foldr removeUTFControlChars []
                    |> fromCodePoints
                    |> text ]
        , div [] ( List.map ( \link -> viewLink link ) object.links )
        ]

removeUTFControlChars: Int -> List Int -> List Int
removeUTFControlChars a b =
    case ( a > 31 ) && ( a < 65500 ) of
        True -> [ a ] ++ b
        False -> b

viewLink : Link -> Html Msg
viewLink link =
    tr []
        [ td []
            [ a [ onClick <| Msgs.GetObjectRequest link.hash ] [ text link.name ] ]
        , td []
            [ text ( link.name ++ "  " ++ toString link.size ) ]
        , td []
            [ a [ onClick <| Msgs.RemoveLink link ] [ text "rm-link" ] ]
        , td []
            [ text <| link.hash ]
        ]

{-
viewLink : Link -> Html Msg
viewLink link =
    div [] [ a [ onClick <| Msgs.GetObjectRequest link.hash ] 
            [ text ( link.name ++ "  " ++ toString link.size ) ],
            Html.button [ onClick <| Msgs.RemoveLink link ] [ text "del" ] ]
-}

maybeRemote : ( a -> Html Msg ) -> WebData a -> Html Msg
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
    div [] [ text <| List.foldr intToStringFoldFun "" <| toCodePoints data ]


intToStringFoldFun: Int -> String -> String
intToStringFoldFun a b =
    ( toString a ) ++ ", " ++ b
