module Api.Endpoint exposing (Endpoint, add, config, content, dagGet, dagPut, file, getContent, links, node, request, task)

import CommentId exposing (CommentId)
import Http
import Route
import Task
import Url.Builder exposing (QueryParameter)
import Username exposing (Username)



-- ENDPOINTS


getContent : String -> Endpoint
getContent cid =
    url gateway [ cid ] []


file : String -> String
file cid =
    unwrap (getContent cid)


dagGet : String -> Endpoint
dagGet cid =
    url endpoint [ "dag", "get" ] [ Url.Builder.string "arg" cid ]



--deepDagGet : Route.Path -> Endpoint
--deepDagGet { cid, location } =
--    let
--        path =
--            Url.Builder.relative (cid :: location) []
--    in
--    url endpoint [ "dag", "get" ] [ Url.Builder.string "arg" path ]
--dagGetContent : Route.Path -> Endpoint
--dagGetContent { cid, location } =
--    url endpoint [ "dag", "get" ] [ Url.Builder.string "arg" path ]


dagPut : Endpoint
dagPut =
    url endpoint [ "dag", "put" ] []


content : Route.Path -> Endpoint
content path =
    "http://localhost:5001/api/v0/dag/get?arg="
        ++ path.cid
        ++ "/"
        ++ (String.join "/" <| List.map (\s -> "links/" ++ s) path.location)
        ++ "/cid"
        |> Endpoint


node : Route.Path -> Endpoint
node path =
    "http://localhost:5001/api/v0/dag/get?arg="
        ++ path.cid
        ++ "/"
        ++ (String.join "/" <| List.map (\s -> "links/" ++ s) path.location)
        |> Endpoint


links : Route.Path -> Endpoint
links path =
    "http://localhost:5001/api/v0/dag/get?arg="
        ++ path.cid
        ++ "/"
        ++ (String.join "/" <| List.map (\s -> "links/" ++ s) path.location)
        ++ "/links"
        |> Endpoint


config : Endpoint
config =
    url endpoint [ "config", "show" ] []


add : Endpoint
add =
    url endpoint [ "add" ] []



-- TYPES


type Endpoint
    = Endpoint String


type alias Host =
    { protocol : String
    , hostname : String
    , portnum : Int
    , path : String
    }


endpoint : Host
endpoint =
    Host "http" "localhost" 5001 "/api/v0"


gateway : Host
gateway =
    Host "http" "localhost" 8080 "/ipfs"


hostToString : Host -> String
hostToString host =
    host.protocol
        ++ "://"
        ++ host.hostname
        ++ ":"
        ++ String.fromInt host.portnum
        ++ host.path


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


url : Host -> List String -> List QueryParameter -> Endpoint
url host paths queryParams =
    Url.Builder.crossOrigin
        (hostToString host)
        paths
        queryParams
        |> Endpoint


{-| Http.request, except it takes an Endpoint instead of a Url.
-}
request :
    { method : String
    , headers : List Http.Header
    , url : Endpoint
    , body : Http.Body
    , expect : Http.Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }
    -> Cmd msg
request r =
    Http.request
        { method = r.method
        , headers = r.headers
        , url = unwrap r.url
        , body = r.body
        , expect = r.expect
        , timeout = r.timeout
        , tracker = r.tracker
        }


task :
    { method : String
    , headers : List Http.Header
    , url : Endpoint
    , body : Http.Body
    , resolver : Http.Resolver x a
    , timeout : Maybe Float
    }
    -> Task.Task x a
task r =
    Http.task
        { method = r.method
        , headers = r.headers
        , url = unwrap r.url
        , body = r.body
        , resolver = r.resolver
        , timeout = r.timeout
        }
