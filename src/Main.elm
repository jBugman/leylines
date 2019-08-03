module Main exposing (main)

import Browser
import Canvas exposing (Point, Renderable)
import Canvas.Settings
import Color exposing (Color)
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events.Extra.Mouse exposing (onClick)
import Random


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }



-- MODEL


type alias NodeID =
    Int


type alias Link =
    ( NodeID, NodeID )


type alias Node =
    { id : NodeID
    , point : Point
    , clear : Bool
    , active : Bool
    }


newNode : NodeID -> Point -> Node
newNode id point =
    Node id point True False


type alias Nodes =
    Dict NodeID Node


type alias Model =
    { nodes : Nodes
    , links : List Link
    }


setNodes : Model -> Nodes -> Model
setNodes model nodes =
    { model | nodes = nodes }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { nodes = Dict.empty
      , links = []
      }
    , Random.generate Init randomPoints
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


randomPoint : Random.Generator Point
randomPoint =
    let
        randomCoordInside size =
            size // 2 |> toFloat |> randomCoord
    in
    Random.pair (randomCoordInside width) (randomCoordInside height)


randomPoints : Random.Generator (List Point)
randomPoints =
    Random.list nodeCount randomPoint


radius : Float
radius =
    10


width : Int
width =
    640


height : Int
height =
    480


nodeCount : Int
nodeCount =
    5



-- UPDATE


type Msg
    = Init (List Point)
    | Click Point


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init points ->
            ( points
                |> fromPoints
                |> setNodes model
            , Cmd.none
            )

        Click point ->
            ( point
                |> nodeInRadius model
                |> log "click"
                |> handleClick model
                |> setNodes model
            , Cmd.none
            )


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
        nodeAt id =
            Dict.get id nodes
    in
    case Maybe.map2 Tuple.pair (nodeAt i) (nodeAt j) of
        -- This should't be the case but follback to doint nothing in this case
        Nothing ->
            nodes

        Just ( a, b ) ->
            nodes
                |> Dict.insert a.id { a | point = b.point }
                |> Dict.insert b.id { b | point = a.point }



-- Dict.update


fromPoints : List Point -> Nodes
fromPoints =
    Dict.fromList << List.indexedMap (\id point -> ( id, newNode id point ))


nodeInRadius : Model -> Point -> Maybe NodeID
nodeInRadius { nodes } target =
    nodes |> findNode1 "nearby nodes" (.point >> inRadius target)


activeNode : Nodes -> Maybe NodeID
activeNode =
    findNode1 "active nodes" .active


{-| Same as findNode but with debug print
-}
findNode1 : String -> (Node -> Bool) -> Nodes -> Maybe NodeID
findNode1 msg predicate =
    Dict.values >> List.filter predicate >> log msg >> List.head >> Maybe.map .id


findNode : (Node -> Bool) -> Nodes -> Maybe NodeID
findNode predicate =
    Dict.values >> List.filter predicate >> List.head >> Maybe.map .id


inRadius : Point -> Point -> Bool
inRadius a b =
    distance a b <= radius


distance : Point -> Point -> Float
distance ( x1, y1 ) ( x2, y2 ) =
    sqrt <| (x2 - x1) ^ 2 + (y2 - y1) ^ 2



-- VIEW


type alias Dot =
    Canvas.Shape


dot : Point -> Dot
dot point =
    Canvas.circle point radius


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


canvas : List Renderable -> Html Msg
canvas =
    Canvas.toHtml ( width, height )
        [ class "canvas"
        , onClick (.offsetPos >> Click)
        ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ canvas <|
            List.concat
                [ renderNodes model.nodes
                ]
        ]
