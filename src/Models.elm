module Models exposing (..)

import RemoteData exposing (WebData)


type alias Model =
    { object : WebData Object
    , hash : Hash
    , data : String
    , headers : WebData Hash
    , pure_data : WebData String
    , path : List Node
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
    , data : Data
    }

type alias Header =
    { hash : Hash }

type LinkType 
    = Folder
    | File

-- ModifiedObject приходит в ответ на запросы object put/patch(все)/new/links

type alias ModifiedObject =
    { hash : String
    , links : List Link
    }

-- Alias of basic types

type alias Hash =
    String

type alias Name =
    String

type alias Data =
    String

type alias Level =
    Int