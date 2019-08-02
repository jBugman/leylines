module Main exposing (main)

import Browser
import Canvas exposing (Point)
import Canvas.Settings
import Color exposing (Color)
import Debug exposing (log)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Link =
    ( Int, Int )


type alias Node =
    { point : Point
    , clear : Bool
    }


node : Point -> Node
node point =
    Node point True


type alias Model =
    { nodes : List Node
    , links : List Link
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { nodes = []
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


randomCoord : Float -> Random.Generator Float
randomCoord range =
    Random.float (range / 2 - toFloat maxOffset) (range / 2 + toFloat maxOffset)


randomPoint : Random.Generator Point
randomPoint =
    Random.pair (randomCoord <| toFloat width) (randomCoord <| toFloat height)


randomPoints : Random.Generator (List Point)
randomPoints =
    Random.list 16 randomPoint


width : Int
width =
    640


height : Int
height =
    480


maxOffset : Int
maxOffset =
    min width height // 2 - 20



-- UPDATE


type Msg
    = Init (List Point)
    | Click


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init points ->
            ( { model | nodes = List.map node points }
            , Cmd.none
            )

        Click ->
            ( model, log "click" Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


type alias Dot =
    Canvas.Shape


dot : Point -> Dot
dot point =
    Canvas.circle point 10


clearNodes : List Node -> Canvas.Renderable
clearNodes nodes =
    Canvas.shapes
        [ Canvas.Settings.fill clearColor
        ]
        (nodes
            |> List.filter .clear
            |> List.map (.point >> dot)
        )


view : Model -> Html Msg
view model =
    div
        [ class "container"
        ]
        [ Canvas.toHtml ( width, height )
            [ class "canvas"
            , onClick Click
            ]
            [ clearNodes model.nodes
            ]
        ]
