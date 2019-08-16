module Link exposing (Link, areAdjacent, endpoints, fromTuple, isClear, new, setClear)

import Node


type Link
    = Link
        { endpoints : ( Node.ID, Node.ID )
        , clear : Bool
        }


new : Node.ID -> Node.ID -> Link
new i j =
    Link
        { endpoints = ( i, j )
        , clear = True
        }


fromTuple : ( Node.ID, Node.ID ) -> Link
fromTuple ( x, y ) =
    new x y


endpoints : Link -> ( Node.ID, Node.ID )
endpoints (Link link) =
    link.endpoints


isClear : Link -> Bool
isClear (Link link) =
    link.clear


setClear : Link -> Bool -> Link
setClear (Link link) value =
    Link { link | clear = value }


areAdjacent : Link -> Link -> Bool
areAdjacent (Link a) (Link b) =
    let
        ( v11, v12 ) =
            a.endpoints

        ( v21, v22 ) =
            b.endpoints
    in
    v11 == v21 || v11 == v22 || v12 == v21 || v12 == v22
