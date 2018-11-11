module Main exposing (..)

import View exposing (..)
import Html exposing (program)
import Ports
import DropZone
import Task exposing (attempt)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


init : ( Model, Cmd Msg )
init =
    ( initModel
    , Cmd.batch 
        [ Task.attempt UpdateZipper 
            <| getTree 2 initTree ]
    )

initModel : Model
initModel =
    { root = initNode.cid
    , data = "" 
    , zipper = Zipper.fromTree <| initTree
    , raw_dag = ""
    , dropZone =
        DropZone.init
    , files_to_add = []
    , content = []
--    , tree = initTree
    , path = [ initNode ]
--    , links = []
--    , draft = []
    }

initNode : Node
initNode =
    { name = "ROOT"
    , cid = "QmQ12hfokJyGXFFMiFaL4TK5eNMPTq2aSy6ng5s9V6rLvb"
    , size = 0
    , title = ""
    , parent = Nothing
    , status = Completed
--    , children = Children []
--    , content = []
    , id = 0
    }

initTree : Tree Node
initTree =
    Tree.tree initNode
        [ Tree.tree { initNode | name = "CHILD tree" }
            [ Tree.singleton { initNode | name = "grand CHILD1" }
            , Tree.tree { initNode | name = "grand CHILD2" }
                [ Tree.singleton { initNode | name = "grand grand CHILD tree" }
                ]
            ]
        , Tree.singleton { initNode | name = "CHILD node" }
        ]

subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveData UpdateQuery

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }