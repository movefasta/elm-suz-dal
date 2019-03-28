module Page.Settings exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Hash)
import Api.Endpoint as Endpoint
import Avatar
import Browser.Navigation as Nav
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Email exposing (Email)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, list, string)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode
import Loading
import Log
import Route
import Session exposing (Session)
import Task
import Username as Username exposing (Username)



-- MODEL


type alias Model =
    { session : Session
    , problems : List Problem
    , status : Status Form
    , root : String
    , config : Status String
    }


type alias Form =
    { avatar : String
    , bio : String
    , email : String
    , username : String
    , password : String
    }


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , problems = []
      , status = Loading
      , root = .cid <| Session.path session
      , config = Loading
      }
    , Cmd.batch
        [ Api.get Endpoint.config GotConfig peerIdDecoder ]
    )


formDecoder : Decoder Form
formDecoder =
    Decode.succeed Form
        |> required "image" (Decode.map (Maybe.withDefault "") (Decode.nullable Decode.string))
        |> required "bio" (Decode.map (Maybe.withDefault "") (Decode.nullable Decode.string))
        |> required "email" Decode.string
        |> required "username" Decode.string
        |> hardcoded ""


peerIdDecoder : Decoder String
peerIdDecoder =
    Decode.at [ "Identity", "PeerID" ] Decode.string


{-| A form that has been validated. Only the `edit` function uses this. Its
purpose is to prevent us from forgetting to validate the form before passing
it to `edit`.

This doesn't create any guarantees that the form was actually validated. If
we wanted to do that, we'd need to move the form data into a separate module!

-}
type ValidForm
    = Valid Form



-- VIEW


view : Model -> { title : String, content : Element Msg }
view model =
    { title = "Settings"
    , content =
        column [ width fill, Font.size 12 ] [ viewControls model, viewConfig model.config ]
    }


viewControls : Model -> Element Msg
viewControls model =
    row
        [ spacing 5, width fill ]
        [ Input.text
            [ padding 5
            , Event.onLoseFocus <| UpdateLocalStorage model.root
            ]
            { onChange = UpdateRoot
            , text = model.root
            , placeholder = Just <| Input.placeholder [] <| text "Enter query hash here"
            , label = Input.labelLeft [ padding 5 ] <| text "Root Hash"
            }
        ]


viewConfig : Status String -> Element Msg
viewConfig status =
    case status of
        Loading ->
            text "Loading..."

        LoadingSlowly ->
            text "Loading slowly ..."

        Loaded config ->
            paragraph [ width fill ] <| [ text "Ваш идентификатор IPFS:", text config ]

        Failed ->
            text "Config loading failed"



-- UPDATE


type Msg
    = SubmittedForm Form
    | EnteredEmail String
    | EnteredUsername String
    | EnteredPassword String
    | EnteredBio String
    | EnteredAvatar String
    | CompletedFormLoad (Result Http.Error Form)
    | CompletedSave (Result Http.Error Hash)
    | GotSession Session
    | PassedSlowLoadThreshold
    | UpdateLocalStorage String
    | UpdateRoot String
    | GotConfig (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotConfig config ->
            case config of
                Ok text ->
                    ( { model | config = Loaded text }, Cmd.none )

                Err problems ->
                    ( { model | config = Failed }, Cmd.none )

        UpdateRoot hash ->
            ( { model | root = hash }, Cmd.none )

        UpdateLocalStorage hash ->
            ( model, Api.storeSettings { cid = hash, location = [] } )

        CompletedFormLoad (Ok form) ->
            ( { model | status = Loaded form }
            , Cmd.none
            )

        CompletedFormLoad (Err _) ->
            ( { model | status = Failed }
            , Cmd.none
            )

        SubmittedForm form ->
            case validate form of
                Ok validForm ->
                    ( { model | status = Loaded form }
                    , Api.put (body validForm) CompletedSave
                    )

                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredUsername username ->
            updateForm (\form -> { form | username = username }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        EnteredBio bio ->
            updateForm (\form -> { form | bio = bio }) model

        EnteredAvatar avatar ->
            updateForm (\form -> { form | avatar = avatar }) model

        CompletedSave (Err error) ->
            let
                serverErrors =
                    Api.decodeErrors error
                        |> List.map ServerError
            in
            ( { model | problems = List.append model.problems serverErrors }
            , Cmd.none
            )

        CompletedSave (Ok hash) ->
            ( model
            , Api.storeSettings { cid = hash, location = [] }
            )

        GotSession session ->
            ( { model | session = session }
            , Cmd.none
              -- Route.replaceUrl (Session.navKey session) Route.Home
            )

        PassedSlowLoadThreshold ->
            case model.status of
                Loading ->
                    ( { model | status = LoadingSlowly }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


{-| Helper function for `update`. Updates the form and returns Cmd.none.
Useful for recording form fields!
-}
updateForm : (Form -> Form) -> Model -> ( Model, Cmd msg )
updateForm transform model =
    case model.status of
        Loaded form ->
            ( { model | status = Loaded (transform form) }, Cmd.none )

        _ ->
            ( model, Log.error )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



-- FORM


{-| Marks that we've trimmed the form's fields, so we don't accidentally send
it to the server without having trimmed it!
-}
type TrimmedForm
    = Trimmed Form


{-| When adding a variant here, add it to `fieldsToValidate` too!

NOTE: there are no ImageUrl or Bio variants here, because they aren't validated!

-}
type ValidatedField
    = Username
    | Email
    | Password


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Username
    , Email
    , Password
    ]


{-| Trim the form and validate its fields. If there are problems, report them!
-}
validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm

        problems ->
            Err problems


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Username ->
                if String.isEmpty form.username then
                    [ "username can't be blank." ]

                else
                    []

            Email ->
                if String.isEmpty form.email then
                    [ "email can't be blank." ]

                else
                    []

            Password ->
                let
                    passwordLength =
                        String.length form.password
                in
                if passwordLength > 0 then
                    [ "password must be at least " ++ " characters long." ]

                else
                    []


{-| Don't trim while the user is typing! That would be super annoying.
Instead, trim only on submit.
-}
trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { avatar = String.trim form.avatar
        , bio = String.trim form.bio
        , email = String.trim form.email
        , username = String.trim form.username
        , password = String.trim form.password
        }



-- HTTP


{-| This takes a Valid Form as a reminder that it needs to have been validated
first.
-}
body : TrimmedForm -> Encode.Value
body (Trimmed form) =
    let
        encodedAvatar =
            case form.avatar of
                "" ->
                    Encode.null

                avatar ->
                    Encode.string avatar

        updates =
            [ ( "username", Encode.string form.username )
            , ( "email", Encode.string form.email )
            , ( "bio", Encode.string form.bio )
            , ( "image", encodedAvatar )
            ]

        encodedUser =
            Encode.object <|
                case form.password of
                    "" ->
                        updates

                    password ->
                        ( "password", Encode.string password ) :: updates
    in
    Encode.object [ ( "user", encodedUser ) ]


nothingIfEmpty : String -> Maybe String
nothingIfEmpty str =
    if String.isEmpty str then
        Nothing

    else
        Just str
