module Page.Blank exposing (view)

import Element as E exposing (..)


view : { title : String, content : Element msg }
view =
    { title = ""
    , content = text ""
    }
