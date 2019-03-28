module Page exposing (Page(..), processPath, view)

import Api
import Avatar
import Browser exposing (Document)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Element.Lazy exposing (lazy, lazy2)
import Html exposing (Html)
import Json.Decode as Decode
import Route exposing (Path, Route)
import Session exposing (Session)
import Username exposing (Username)


type Page
    = Other
    | Home
    | Tensor
    | Settings



-- VIEW


view : Path -> Page -> { title : String, content : Element msg } -> Browser.Document msg
view path page { title, content } =
    { title = title ++ " - Трактовочная сеть"
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
            row [ width fill, height fill ] (viewMenu path page :: [ content ])
        ]
    }


viewMenu : Path -> Page -> Element msg
viewMenu path page =
    let
        linkTo =
            navbarLink page
    in
    column
        [ height fill
        , width <| px 30
        , padding 5
        , spacing 7
        , Font.size 14
        , Background.color <| E.rgba255 211 211 211 1.0
        ]
        [ linkTo Route.Home
        , linkTo <| Route.Tensor path
        , column [] <| viewPath { path | location = List.reverse path.location } []
        , linkTo Route.Settings
        ]


navbarLink : Page -> Route -> Element msg
navbarLink page route =
    link
        [ width <| px 20
        , height <| px 20
        , spacing 3
        , Border.rounded 10
        , Border.width 2
        , Border.color <|
            if isActive page route then
                E.rgba255 211 211 211 1.0

            else
                E.rgba255 255 255 255 1.0
        ]
        { url = Route.routeToString route
        , label =
            case route of
                Route.Home ->
                    text "H"

                Route.Tensor path ->
                    text "T"

                --column [] <|
                -- List.map (\x -> text x) path.location
                Route.Settings ->
                    text "S"
        }


isActive : Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( Home, Route.Home ) ->
            True

        ( Tensor, Route.Tensor _ ) ->
            True

        ( Settings, Route.Settings ) ->
            True

        _ ->
            False


viewPath : Path -> List (Element msg) -> List (Element msg)
viewPath path acc =
    let
        crumb p =
            link
                [ width <| px 20
                , height <| px 20
                , spacing 3
                , Border.rounded 10
                ]
                { url = Route.pathToUrl p
                , label = text <| String.join "/" p.location
                }
    in
    processPath path crumb []


processPath : Path -> (Path -> a) -> List a -> List a
processPath path fun acc =
    case path.location of
        x :: xs ->
            processPath
                { path | location = xs }
                fun
                ([ fun { path | location = List.reverse (x :: xs) } ] ++ acc)

        [] ->
            acc



{-
   Render dismissable errors. We use this all over the place!
   viewErrors : msg -> List String -> Html msg
   viewErrors dismissErrors errors =
   if List.isEmpty errors then
   Html.text ""

       else
           div
               [ class "error-messages"
               , style "position" "fixed"
               , style "top" "0"
               , style "background" "rgb(250, 250, 250)"
               , style "padding" "20px"
               , style "border" "1px solid"
               ]
           <|
               List.map (\error -> p [] [ text error ]) errors
                   ++ [ button [ onClick dismissErrors ] [ text "Ok" ] ]

-}
