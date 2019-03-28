module Session exposing (Session, changes, fromPath, navKey, path, pathDecoder, update)

import Api
import Avatar exposing (Avatar)
import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required, requiredAt)
import Json.Encode as Encode exposing (Value)
import Route exposing (Path)
import Time



-- TYPES


type Session
    = Session Nav.Key Path



-- INFO


update : Session -> Path -> Session
update (Session key p) newpath =
    Session key newpath


path : Session -> Path
path (Session _ val) =
    val


navKey : Session -> Nav.Key
navKey (Session key _) =
    key



-- SERIALIZATION


pathDecoder : Decoder Path
pathDecoder =
    Decode.succeed Path
        |> requiredAt [ "path", "cid" ] Decode.string
        |> requiredAt [ "path", "location" ] (Decode.list Decode.string)



-- CHANGES


changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    Api.viewerChanges (\x -> toMsg (fromPath key x)) pathDecoder


fromPath : Nav.Key -> Path -> Session
fromPath key p =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Session key p
