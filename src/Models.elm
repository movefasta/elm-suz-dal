module Models exposing (..)

import RemoteData exposing (WebData)


initModel : Route -> Model
initModel route =
    { object = RemoteData.Loading
    , hash = "QmUuGJ2gVET4JycdoJTGVk8VYHiRpf3B8WRhqWwiFG2idZ"
    , data = ""
    , headers = RemoteData.NotAsked
    , pure_data = RemoteData.Loading
    , currentRoute = route
    }

type alias Model =
  { object : WebData Object
  , hash : Hash
  , data : String
  , headers : WebData Hash
  , pure_data : WebData String
  , currentRoute : Route
  }

type Route
    = HashRoute Hash
    | LinkRoute Hash Name
    | NotFoundRoute

type alias Hash =
    String

type alias Data =
    String

type alias File =
    String

type alias Header =
    { hash : Hash }

type alias Name =
    String

-- модель ссылки IPFS

type alias Link =
    { name : Name
    , hash : Hash
    , size : Int
    }

-- модель объекта IPFS, приходит в ответ на запрос object get 

type alias Object =
    { data : Data
    , links : List Link
    }

-- ModifiedObject приходит в ответ на запросы object put/patch(все)/new/links

type alias ModifiedObject =
    { hash : String
    , links : List Link
    }


{-

type alias Model =
    { players : WebData (List Player)
    , route : Route
    }


initialModel : Route -> Model
initialModel route =
    { players = RemoteData.Loading
    , route = route
    }
-}