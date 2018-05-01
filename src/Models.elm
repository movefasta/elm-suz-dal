module Models exposing (..)

import RemoteData exposing (WebData)

type alias Hash =
    String

type alias Data =
    String

type alias File =
    String

type alias Header =
    { hash : Hash }

-- модель ссылки IPFS

type alias Link =
    { name : String
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

type alias Model =
  { object : WebData Object
  , hash : Hash
  , data : String
  , headers : WebData Hash
  , pure_data : WebData String
  }

initModel : Model
initModel =
  Model
    RemoteData.Loading
    "QmUuGJ2gVET4JycdoJTGVk8VYHiRpf3B8WRhqWwiFG2idZ"
    ""
    RemoteData.Loading
    RemoteData.Loading
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


type Route
    = PlayersRoute
    | PlayerRoute PlayerId
    | NotFoundRoute

-}