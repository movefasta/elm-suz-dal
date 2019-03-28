module Page.NotFound exposing (view)

import Asset
import Element as E exposing (..)



-- VIEW


view : { title : String, content : Element msg }
view =
    { title = "Not Found"
    , content =
        text "Not Found"
    }
