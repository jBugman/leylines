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
    ( Int, Int )


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
    Dict Int Node


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
            ( setNodes model <| fromPoints points
            , Cmd.none
            )

        Click point ->
            ( nodeAt model.nodes point
                |> log "click"
                |> handleNodeClick model.nodes
                |> setNodes model
            , Cmd.none
            )


handleNodeClick : Nodes -> Maybe NodeID -> Nodes
handleNodeClick nodes maybeID =
    case maybeID of
        Just id ->
            markActive nodes id

        Nothing ->
            unmarkActive nodes


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


fromPoints : List Point -> Nodes
fromPoints points =
    points
        |> List.indexedMap (\id point -> ( id, newNode id point ))
        |> Dict.fromList


nodeAt : Nodes -> Point -> Maybe NodeID
nodeAt nodes target =
    nodes
        |> Dict.values
        |> List.filter (.point >> inRadius target)
        |> log "nearby nodes"
        -- this picks one at random it there are multiple at this point
        |> List.head
        |> Maybe.map .id


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
