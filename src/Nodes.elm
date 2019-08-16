module Nodes exposing (Nodes, active, clearActive, find, get, list, new, setActive, swap)

import Dict exposing (Dict)
import List.Extra
import Node exposing (Node)


type alias NodeDict =
    Dict Node.ID Node


type Nodes
    = Nodes
        { dict : NodeDict
        , active : Maybe Node.ID
        }


new : List Node -> Nodes
new nodes =
    Nodes
        { dict =
            nodes
                |> List.map (\node -> ( Node.id node, node ))
                |> Dict.fromList
        , active = Nothing
        }


list : Nodes -> List Node
list (Nodes nodes) =
    Dict.values nodes.dict


active : Nodes -> Maybe Node
active (Nodes nodes) =
    nodes.active
        |> Maybe.map (\id -> get1 id nodes.dict)



-- Dict.get (nodes.active) dict


find : (Node -> Bool) -> Nodes -> Maybe Node.ID
find predicate (Nodes nodes) =
    Dict.values nodes.dict
        |> List.Extra.find predicate
        |> Maybe.map Node.id


get : Node.ID -> Nodes -> Node
get nodeID (Nodes nodes) =
    get1 nodeID nodes.dict


get1 : Node.ID -> NodeDict -> Node
get1 nodeID dict =
    Dict.get nodeID dict
        |> Maybe.withDefault Node.impossible


update : Node -> (Node -> Node) -> NodeDict -> NodeDict
update node updater =
    Dict.insert (Node.id node) (updater node)


swap : Node.ID -> Node.ID -> Nodes -> Nodes
swap i j (Nodes nodes) =
    let
        a =
            get1 i nodes.dict

        b =
            get1 j nodes.dict
    in
    nodes.dict
        |> update a (Node.withCoordinates <| Node.coordinates b)
        |> update b (Node.withCoordinates <| Node.coordinates a)
        |> (\dict -> Nodes { nodes | dict = dict })


clearActive : Nodes -> Nodes
clearActive (Nodes nodes) =
    Nodes { nodes | active = Nothing }


setActive : Node.ID -> Nodes -> Nodes
setActive id (Nodes nodes) =
    Nodes { nodes | active = Just id }
