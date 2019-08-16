module Node exposing (ID, Node, coordinates, id, impossible, isActive, new, withCoordinates)

import Point2d exposing (Point2d)


type alias ID =
    Int


type Node
    = Node
        { id : ID
        , coordinates : Point2d
        , active : Bool
        }


new : ID -> Point2d -> Node
new nodeID coords =
    Node
        { id = nodeID
        , coordinates = coords
        , active = False
        }


impossible : Node
impossible =
    new -1 Point2d.origin


isActive : Node -> Bool
isActive (Node { active }) =
    active


id : Node -> ID
id (Node node) =
    node.id


coordinates : Node -> Point2d
coordinates (Node node) =
    node.coordinates


withCoordinates : Point2d -> Node -> Node
withCoordinates coords (Node node) =
    Node { node | coordinates = coords }
