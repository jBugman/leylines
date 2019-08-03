module Main exposing (main)

import Browser
import Canvas exposing (Point, Renderable)
import Canvas.Settings
import Canvas.Settings.Line
import Color exposing (Color)
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events.Extra.Mouse exposing (onClick)
import LineSegment2d exposing (fromEndpoints, intersectionPoint)
import List.Extra exposing (find, groupsOf, groupsOfWithStep)
import Maybe.Extra exposing (isJust, unpack)
import Point2d exposing (Point2d, coordinates)
import Random
import Random.List


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }



-- MODEL


type alias NodeID =
    Int


type alias Link =
    { vertices : ( NodeID, NodeID )
    , clear : Bool
    }


newLink : ( NodeID, NodeID ) -> Link
newLink nodes =
    Link nodes True


setClear : Link -> Bool -> Link
setClear link clear =
    { link | clear = clear }


type alias Node =
    { id : NodeID
    , point : Point2d
    , clear : Bool
    , active : Bool
    }


newNode : NodeID -> Point2d -> Node
newNode id point =
    Node id point True False


type alias Nodes =
    Dict NodeID Node


type alias Links =
    List Link


type alias Model =
    { nodes : Nodes
    , links : Links
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { nodes = Dict.empty
      , links = []
      }
    , Random.generate Init randomState
    )


clearColor : Color
clearColor =
    Color.lightBlue


crossedColor : Color
crossedColor =
    Color.lightRed


activeColor : Color
activeColor =
    Color.lightGreen


randomCoord : Float -> Random.Generator Float
randomCoord range =
    let
        offset =
            toFloat <| min width height // 2 - 30
    in
    Random.float (range - offset) (range + offset)


randomPoint : Random.Generator Point2d
randomPoint =
    let
        randomCoordInside size =
            size // 2 |> toFloat |> randomCoord
    in
    Random.pair (randomCoordInside width) (randomCoordInside height)
        |> Random.map Point2d.fromCoordinates


randomPoints : Random.Generator (List Point2d)
randomPoints =
    Random.list nodeCount randomPoint


randomTriangles : Random.Generator (List (List NodeID))
randomTriangles =
    List.range 0 (nodeCount - 1)
        |> Random.List.shuffle
        |> Random.map (groupsOf 3)


linkTriangle : List NodeID -> List Link
linkTriangle ids =
    (ids ++ List.take 1 ids)
        |> groupsOfWithStep 2 1
        |> List.map (toTuple >> newLink)


randomLinks : Random.Generator Links
randomLinks =
    randomTriangles
        |> Random.map (List.map linkTriangle >> List.concat)


randomState : Random.Generator InitialState
randomState =
    Random.pair randomPoints randomLinks


maybeTuple : List a -> Maybe ( a, a )
maybeTuple list =
    case list of
        [ x, y ] ->
            Just ( x, y )

        _ ->
            Nothing


toTuple : List NodeID -> ( NodeID, NodeID )
toTuple =
    maybeTuple >> unwrap "Attempting to construct tuple not from a [,]" ( -1, -1 )


unwrap : String -> a -> Maybe a -> a
unwrap err default =
    unpack
        (\_ -> log err default)
        identity


radius : Float
radius =
    10


width : Int
width =
    640


height : Int
height =
    480


triangleCount : Int
triangleCount =
    3


nodeCount : Int
nodeCount =
    3 * triangleCount


nodePair : ( NodeID, NodeID ) -> Nodes -> ( Node, Node )
nodePair ( i, j ) nodes =
    let
        nodeAt id =
            Dict.get id nodes

        maybePair =
            Maybe.map2 Tuple.pair (nodeAt i) (nodeAt j)

        neverNode =
            newNode -1 Point2d.origin
    in
    maybePair |> unwrap "invalid node IDs provided" ( neverNode, neverNode )



-- UPDATE


type alias InitialState =
    ( List Point2d, Links )


type Msg
    = Init InitialState
    | Click Point


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init ( points, links ) ->
            ( updateModel model links (fromPoints points)
            , Cmd.none
            )

        Click point ->
            ( point
                |> Point2d.fromCoordinates
                |> nodeInRadius model
                |> handleClick model
                |> updateModel model model.links
            , Cmd.none
            )


updateModel : Model -> Links -> Nodes -> Model
updateModel model links nodes =
    { model
        | nodes = nodes
        , links = calculateCrossing nodes links
    }


handleClick : Model -> Maybe NodeID -> Nodes
handleClick { nodes } maybeID =
    case maybeID of
        Just id ->
            handleNodeClick nodes id

        Nothing ->
            unmarkActive nodes


handleNodeClick : Nodes -> NodeID -> Nodes
handleNodeClick nodes id =
    case activeNode nodes of
        Nothing ->
            markActive nodes id

        Just activeID ->
            nodes
                |> swapNodes id activeID
                |> unmarkActive


unmarkActive : Nodes -> Nodes
unmarkActive =
    Dict.map (\_ v -> { v | active = False })


markActive : Nodes -> NodeID -> Nodes
markActive nodes id =
    let
        updater =
            Maybe.map (\node -> { node | active = True })
    in
    Dict.update id updater nodes


swapNodes : NodeID -> NodeID -> Nodes -> Nodes
swapNodes i j nodes =
    let
        ( a, b ) =
            nodePair ( i, j ) nodes
    in
    nodes
        |> Dict.insert a.id { a | point = b.point }
        |> Dict.insert b.id { b | point = a.point }


calculateCrossing : Nodes -> Links -> Links
calculateCrossing nodeDict links =
    let
        shareVertex l1 l2 =
            let
                ( v11, v12 ) =
                    l1.vertices

                ( v21, v22 ) =
                    l2.vertices
            in
            v11 == v21 || v11 == v22 || v12 == v21 || v12 == v22

        otherLinks link =
            List.filter (shareVertex link >> not) links

        segment =
            fromEndpoints << linkPoints nodeDict

        crosses l1 l2 =
            isJust <| intersectionPoint (segment l1) (segment l2)
    in
    links
        |> List.map
            (\link ->
                List.any (crosses link) (otherLinks link)
                    |> not
                    |> setClear link
            )


fromPoints : List Point2d -> Nodes
fromPoints =
    Dict.fromList << List.indexedMap (\id point -> ( id, newNode id point ))


nodeInRadius : Model -> Point2d -> Maybe NodeID
nodeInRadius { nodes } target =
    let
        inRadius a b =
            Point2d.distanceFrom a b <= radius
    in
    nodes |> findNodeID (.point >> inRadius target)


activeNode : Nodes -> Maybe NodeID
activeNode =
    findNodeID .active


findNodeID : (Node -> Bool) -> Nodes -> Maybe NodeID
findNodeID predicate =
    Dict.values >> find predicate >> Maybe.map .id



-- VIEW


dot : Point2d -> Canvas.Shape
dot point =
    Canvas.circle (coordinates point) radius


line : ( Point2d, Point2d ) -> Canvas.Shape
line ( p1, p2 ) =
    Canvas.path
        (coordinates p1)
        [ Canvas.lineTo (coordinates p2) ]


renderNodeType : List Node -> Color -> Renderable
renderNodeType nodes color =
    nodes
        |> List.map (.point >> dot)
        |> Canvas.shapes [ Canvas.Settings.fill color ]


renderNodes : Nodes -> List Renderable
renderNodes nodes =
    nodes
        |> Dict.values
        |> List.partition .active
        |> (\( active, rest ) ->
                rest
                    |> List.partition .clear
                    |> (\( clear, crossed ) ->
                            [ renderNodeType active activeColor
                            , renderNodeType clear clearColor
                            , renderNodeType crossed crossedColor
                            ]
                       )
           )


linkPoints : Nodes -> Link -> ( Point2d, Point2d )
linkPoints nodes { vertices } =
    nodePair vertices nodes
        |> Tuple.mapBoth .point .point


renderLinkType : Color -> Nodes -> Links -> Renderable
renderLinkType color nodes links =
    links
        |> List.map (linkPoints nodes >> line)
        |> Canvas.shapes
            [ Canvas.Settings.stroke color
            , Canvas.Settings.Line.lineWidth 3
            ]


renderLinks : Nodes -> Links -> List Renderable
renderLinks nodes links =
    links
        |> List.partition .clear
        |> (\( clear, rest ) ->
                [ renderLinkType clearColor nodes clear
                , renderLinkType crossedColor nodes rest
                ]
           )


background : Renderable
background =
    Canvas.shapes
        [ Canvas.Settings.fill <| Color.rgb255 0xF5 0xF5 0xF5 ]
        [ Canvas.rect ( 0, 0 ) (toFloat width) (toFloat height) ]


canvas : List Renderable -> Html Msg
canvas =
    (::) background
        >> Canvas.toHtml ( width, height )
            [ class "canvas"
            , onClick (.offsetPos >> Click)
            ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ canvas <|
            List.concat
                [ renderLinks model.nodes model.links
                , renderNodes model.nodes
                ]
        ]
