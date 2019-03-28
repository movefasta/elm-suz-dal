module Main exposing (main)

{-
   Сделать к выпуску версии 0.1:
       - загрузка и выгрузка данных из локального хранилища
            + корневой хэш хранилища
            - закладки
            + текущая страница
       - разработать шаблон кругозора и модель данных для него
       - разработать шаблон строений и модель данных для него
       - добавить кпопку "Сохранить" для обновления корневого хэша; кнопка показывает обновление данных
       + добавить возможность замены хэша для произвольной структуры дерева (временная замена слиянию)
       - разработать шаблон домашней страницы
       + почистить приложение от авторизации

-}

import Api
import Avatar exposing (Avatar)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Element.Lazy exposing (lazy, lazy2)
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Page exposing (Page)
import Page.Blank as Blank
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Settings as Settings
import Page.Tensor as Tensor
import Route exposing (Route)
import Session exposing (Session)
import Task
import Time
import Url exposing (Url)
import Username exposing (Username)


type Model
    = Redirect Session
    | NotFound Session
    | Home Home.Model
    | Settings Settings.Model
    | Tensor Tensor.Model



-- MODEL


init : Route.Path -> Url -> Nav.Key -> ( Model, Cmd Msg )
init path url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.fromPath navKey path))



-- VIEW


view : Model -> Document Msg
view model =
    let
        viewPage page toMsg doc =
            let
                { title, body } =
                    Page.view (Session.path (toSession model)) page doc
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _ ->
            viewPage Page.Other (\_ -> Ignored) Blank.view

        NotFound _ ->
            viewPage Page.Other (\_ -> Ignored) NotFound.view

        Settings settings ->
            viewPage Page.Other GotSettingsMsg (Settings.view settings)

        Home home ->
            viewPage Page.Home GotHomeMsg (Home.view home)

        Tensor tensor ->
            viewPage Page.Other GotTensorMsg (Tensor.view tensor)



-- UPDATE


type Msg
    = Ignored
    | ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHomeMsg Home.Msg
    | GotSettingsMsg Settings.Msg
    | GotTensorMsg Tensor.Msg
    | GotSession Session


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        Home home ->
            Home.toSession home

        Settings settings ->
            Settings.toSession settings

        Tensor tensor ->
            Tensor.toSession tensor


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Settings ->
            Settings.init session
                |> updateWith Settings GotSettingsMsg model

        Just Route.Home ->
            Home.init session
                |> updateWith Home GotHomeMsg model

        Just (Route.Tensor path) ->
            Tensor.init (Session.update session path) path
                |> updateWith Tensor GotTensorMsg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Ignored, _ ) ->
            ( model, Cmd.none )

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            -- If we got a link that didn't include a fragment,
                            -- it's from one of those (href "") attributes that
                            -- we have to include to make the RealWorld CSS work.
                            --
                            -- In an application doing path routing instead of
                            -- fragment-based routing, this entire
                            -- `case url.fragment of` expression this comment
                            -- is inside would be unnecessary.
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            changeRouteTo route model

        ( GotSettingsMsg subMsg, Settings settings ) ->
            Settings.update subMsg settings
                |> updateWith Settings GotSettingsMsg model

        ( GotTensorMsg subMsg, Tensor tensor ) ->
            Tensor.update subMsg tensor
                |> updateWith Tensor GotTensorMsg model

        ( GotHomeMsg subMsg, Home home ) ->
            Home.update subMsg home
                |> updateWith Home GotHomeMsg model

        ( GotSession session, Redirect _ ) ->
            ( Redirect session
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _ ->
            Sub.none

        Redirect _ ->
            Session.changes GotSession (Session.navKey (toSession model))

        Settings settings ->
            Sub.map GotSettingsMsg (Settings.subscriptions settings)

        Home home ->
            Sub.map GotHomeMsg (Home.subscriptions home)

        Tensor tensor ->
            Sub.map GotTensorMsg (Tensor.subscriptions tensor)



-- MAIN


main : Program Value Model Msg
main =
    Api.application Session.pathDecoder
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
