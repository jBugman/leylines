module Main exposing (main)

import Browser
import Canvas
import Canvas.Settings
import Canvas.Settings.Line
import Color exposing (Color)
import Css
import Css.Global
import Html.Events.Extra.Mouse exposing (onClick)
import Html.Styled exposing (Html, div, fromUnstyled, toUnstyled)
import Html.Styled.Attributes exposing (css)
import LineSegment2d
import Link exposing (Link)
import List exposing (concat, filter, map, take)
import List.Extra
import Maybe.Extra
import Node exposing (Node)
import Nodes exposing (Nodes)
import Point2d exposing (Point2d)
import Random
import Random.List


main : Program Flags Model Msg
main =
    Browser.document { init = init, update = update, view = document, subscriptions = \_ -> Sub.none }



-- MODEL


type alias Links =
    List Link


type alias Model =
    { nodes : Nodes
    , links : Links
    , solved : Bool
    }


type alias Flags =
    Maybe String


init : Flags -> ( Model, Cmd Msg )
init triangleCountFlag =
    ( { nodes = Nodes.new []
      , links = []
      , solved = False
      }
    , triangleCount triangleCountFlag
        |> randomState
        |> Random.generate Init
    )


triangleCount : Maybe String -> Int
triangleCount =
    Maybe.andThen String.toInt
        >> Maybe.withDefault 8
        >> max 4


randomPoint : Random.Generator Point2d
randomPoint =
    let
        padding =
            30

        randomCoordInside dimension =
            Random.float padding (toFloat dimension - padding)
    in
    Random.pair (randomCoordInside width) (randomCoordInside height)
        |> Random.map Point2d.fromCoordinates


randomPoints : Int -> Random.Generator (List Point2d)
randomPoints nodeCount =
    Random.list nodeCount randomPoint


randomTriangles : Int -> Random.Generator (List (List Node.ID))
randomTriangles nodeCount =
    List.range 0 (nodeCount - 1)
        |> Random.List.shuffle
        |> Random.map (List.Extra.groupsOf 3)


linkTriangle : List Node.ID -> List Link
linkTriangle ids =
    (ids ++ take 1 ids)
        |> List.Extra.groupsOfWithStep 2 1
        |> List.filterMap (toTuple >> Maybe.map Link.fromTuple)


toTuple : List Node.ID -> Maybe ( Node.ID, Node.ID )
toTuple list =
    case list of
        [ x, y ] ->
            Just ( x, y )

        _ ->
            Nothing


randomLinks : Int -> Random.Generator Links
randomLinks nodeCount =
    randomTriangles nodeCount
        |> Random.map (map linkTriangle >> concat)


randomState : Int -> Random.Generator InitialState
randomState shapeCount =
    let
        nodeCount =
            3 * shapeCount
    in
    Random.pair (randomPoints nodeCount) (randomLinks nodeCount)


nodeCoordinates : Nodes -> Node.ID -> Point2d
nodeCoordinates nodes nodeID =
    Nodes.get nodeID nodes
        |> Node.coordinates


radius : Float
radius =
    8


width : Int
width =
    640


height : Int
height =
    480



-- UPDATE


type alias InitialState =
    ( List Point2d, Links )


type Msg
    = Init InitialState
    | Click ( Float, Float )


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
updateModel model ls nodes =
    let
        links =
            calculateCrossing nodes ls
    in
    { model
        | nodes = nodes
        , links = links
        , solved = List.all Link.isClear links
    }


handleClick : Model -> Maybe Node.ID -> Nodes
handleClick { nodes } maybeID =
    case maybeID of
        Just id ->
            handleNodeClick nodes id

        Nothing ->
            Nodes.clearActive nodes


handleNodeClick : Nodes -> Node.ID -> Nodes
handleNodeClick nodes id =
    case Nodes.active nodes of
        Nothing ->
            Nodes.setActive id nodes

        Just active ->
            nodes
                |> Nodes.swap id (Node.id active)
                |> Nodes.clearActive


calculateCrossing : Nodes -> Links -> Links
calculateCrossing nodeDict links =
    let
        disjointLinks link =
            filter (not << Link.areAdjacent link) links

        segment =
            LineSegment2d.fromEndpoints << linkPoints nodeDict

        crosses l1 l2 =
            Maybe.Extra.isJust <| LineSegment2d.intersectionPoint (segment l1) (segment l2)
    in
    map
        (\link ->
            List.any (crosses link) (disjointLinks link)
                |> not
                |> Link.setClear link
        )
        links


fromPoints : List Point2d -> Nodes
fromPoints =
    List.indexedMap (\id point -> Node.new id point)
        >> Nodes.new


nodeInRadius : Model -> Point2d -> Maybe Node.ID
nodeInRadius { nodes } target =
    nodes
        |> Nodes.find
            (Node.coordinates
                >> Point2d.distanceFrom target
                >> (\distance -> distance <= radius)
            )



-- VIEW


color : Css.Color -> Color
color { red, green, blue } =
    Color.rgb255 red green blue


dot : Point2d -> Canvas.Shape
dot point =
    Canvas.circle (Point2d.coordinates point) radius


line : ( Point2d, Point2d ) -> Canvas.Shape
line ( p1, p2 ) =
    Canvas.path
        (Point2d.coordinates p1)
        [ Canvas.lineTo (Point2d.coordinates p2) ]


renderNodeType : Color -> List Node -> Canvas.Renderable
renderNodeType c nodes =
    nodes
        |> map (Node.coordinates >> dot)
        |> Canvas.shapes [ Canvas.Settings.fill c ]


renderNodes : Nodes -> List Canvas.Renderable
renderNodes nodes =
    [ nodes
        |> Nodes.list
        |> renderNodeType (color theme.blue)
        |> Just
    , nodes
        |> Nodes.active
        |> Maybe.map
            (List.singleton
                >> renderNodeType (color theme.green)
            )
    ]
        |> List.filterMap identity


linkPoints : Nodes -> Link -> ( Point2d, Point2d )
linkPoints nodes link =
    let
        coords nodeID =
            nodeCoordinates nodes nodeID
    in
    Link.endpoints link
        |> Tuple.mapBoth coords coords


renderLinkType : Color -> Nodes -> Links -> Canvas.Renderable
renderLinkType c nodes links =
    links
        |> map (linkPoints nodes >> line)
        |> Canvas.shapes
            [ Canvas.Settings.stroke c
            , Canvas.Settings.Line.lineWidth 3
            ]


renderLinks : Nodes -> Links -> List Canvas.Renderable
renderLinks nodes links =
    links
        |> List.partition Link.isClear
        |> (\( clear, rest ) ->
                [ renderLinkType (color theme.blue) nodes clear
                , renderLinkType (color theme.red) nodes rest
                ]
           )


field : Canvas.Renderable
field =
    Canvas.shapes
        [ Canvas.Settings.fill <| color theme.field ]
        [ Canvas.rect ( 0, 0 ) (toFloat width) (toFloat height) ]


canvas : List (List Canvas.Renderable) -> Html Msg
canvas renderables =
    fromUnstyled <|
        Canvas.toHtml
            ( width, height )
            [ onClick (.offsetPos >> Click)
            ]
            (field :: concat renderables)


theme : { background : Css.Color, field : Css.Color, red : Css.Color, green : Css.Color, blue : Css.Color }
theme =
    { background = Css.hex "272424"
    , field = Css.hex "FFFDFD"
    , green = Css.hex "87CF3E"
    , red = Css.hex "DB4C2E"
    , blue = Css.hex "56ADF4"
    }


border : Bool -> Css.Style
border solved =
    Css.border3 (Css.px 4) Css.solid <|
        if solved then
            theme.green

        else
            theme.red


view : Model -> Html Msg
view { nodes, links, solved } =
    div
        [ css
            [ Css.height <| Css.vh 100
            , Css.displayFlex
            , Css.justifyContent Css.center
            , Css.alignItems Css.center
            ]
        ]
        [ div
            [ css
                [ border solved
                , Css.boxSizing Css.borderBox
                , Css.backgroundColor theme.field
                ]
            ]
            [ canvas
                [ renderLinks nodes links
                , renderNodes nodes
                ]
            ]
        ]


document : Model -> Browser.Document Msg
document model =
    Browser.Document "Leylines" <|
        map toUnstyled
            [ Css.Global.global
                [ Css.Global.body
                    [ Css.backgroundColor theme.background ]
                ]
            , view model
            ]
