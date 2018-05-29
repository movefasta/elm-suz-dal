module Models exposing (..)

import RemoteData exposing (WebData)


type alias Model =
    { hash : Hash
    , data : Data
    , node : Object
    , raw_dag : WebData String
    , path : List Node
    , multihash : MultihashFromJS
    }

type alias Node =
    ( Name, Hash )

-- модель объекта IPFS, приходит в ответ на запрос object get 

type alias Object =
    { data : Data
    , links : List Link
    }

-- модель ссылки IPFS

type alias Link =
    { name : Name
    , hash : Hash
    , size : Int
    }

type alias Header =
    { hash : Hash }

type LinkType 
    = Folder
    | File


-- Alias of basic types

type alias Hash =
    String

type alias Name =
    String

type alias Data =
    String

type alias Level =
    Int
{-}

type alias MultihashFromJS =
    { typ : String
    , data : List Int
    }

-- ModifiedObject приходит в ответ на запросы object put/patch(все)/new/links

type alias ModifiedObject =
    { hash : String
    , links : List Link

type alias JsSetData =
    { multihash : Hash
    , data : Data
    }
    }