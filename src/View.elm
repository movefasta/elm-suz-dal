module View exposing (..)

import Html exposing (program, text, div, input, button, p, h3, a, Html)
import Html.Attributes exposing (href, value, attribute, width, style, downloadAs)
import Html.Events exposing (..)
import Models exposing (Model, Hash, Object, Link)
import Msgs exposing (Msg)
import RemoteData exposing (WebData)
import Json.Decode as Decode
import String.Extra exposing (toCodePoints, fromCodePoints)

view : Model -> Html Msg
view model =
  div []
    [ input [ onInput Msgs.UpdateQuery, value model.hash ] []
    , button [ onClick <| Msgs.GetObjectRequest model.hash ] [ text "Get Object Data" ]
    , input [ onInput Msgs.UpdateData, value model.data, onEnter Msgs.SetDataRequest ] []
    , button [ onClick Msgs.SetDataRequest ] [ text "Set Object Data" ]
    , div [] [ maybeRemote text model.headers ]
    , div [] [ maybeRemote viewObject model.object ]
    ]

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
    div [] [ a [ onClick <| Msgs.GetObjectRequest link.hash ] 
            [ text ( link.name ++ "  " ++ toString link.size ) ] ]

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



{-

view : Model -> Html Msg
view model =
    div []
        [ page model ]


page : Model -> Html Msg
page model =
    case model.route of
        Models.PlayersRoute ->
            Players.List.view model.players

        Models.PlayerRoute id ->
            playerEditPage model id

        Models.NotFoundRoute ->
            notFoundView


playerEditPage : Model -> PlayerId -> Html Msg
playerEditPage model playerId =
    case model.players of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            text "Loading ..."

        RemoteData.Success players ->
            let
                maybePlayer =
                    players
                        |> List.filter (\player -> player.id == playerId)
                        |> List.head
            in
                case maybePlayer of
                    Just player ->
                        Players.Edit.view player

                    Nothing ->
                        notFoundView

        RemoteData.Failure err ->
            text (toString err)


notFoundView : Html msg
notFoundView =
    div []
        [ text "Not found"
        ]
-}