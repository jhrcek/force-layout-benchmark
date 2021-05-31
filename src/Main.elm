module Main exposing (main)

{-| This module aims to test performance of Visualization.Force
using randomly generated Trees.
-}

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events
import Force
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html exposing (Html, button, div, input, span)
import Html.Attributes as HA exposing (value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Random
import Random.Graph
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import Svg.Events as SE
import Task


type Msg
    = DragStart NodeId Position
    | DragAt Position
    | DragEnd Position
    | Zoom Float
    | Tick
    | GenerateNewTree
    | AddRandomTree (Graph () ())
    | ChangeGeneratorParam String
    | GotWindowSize WindowSize


type alias Model =
    { drag : Maybe Drag
    , graph : ForceDirectedGraph
    , simulation : Force.State NodeId
    , nodeCountToGenerate : Int
    , windowSize : WindowSize
    }


type alias WindowSize =
    { width : Float
    , height : Float
    }


type alias Position =
    { x : Int, y : Int }


mousePosition : Decoder Position
mousePosition =
    Decode.map2 Position
        (Decode.field "pageX" Decode.int)
        (Decode.field "pageY" Decode.int)


mouseWheelDecoder : Decoder Float
mouseWheelDecoder =
    Decode.field "deltaY" Decode.float


type alias Drag =
    { start : Position
    , current : Position
    , index : NodeId
    }


type alias Entity =
    Force.Entity NodeId {}


entity : Int -> Entity
entity index =
    let
        radius =
            sqrt (0.5 + toFloat index) * initialRadius

        angle =
            toFloat index * initialAngle
    in
    { x = radius * cos angle
    , y = radius * sin angle
    , vx = 0.0
    , vy = 0.0
    , id = index
    }


initialRadius : Float
initialRadius =
    10


initialAngle : Float
initialAngle =
    pi * (3 - sqrt 5)


type alias ForceDirectedGraph =
    Graph Entity ()


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Task.perform
        (\{ viewport } ->
            GotWindowSize
                { width = viewport.width
                , height = viewport.height
                }
        )
        getViewport
    )


initialModel : Model
initialModel =
    { drag = Nothing
    , graph = Graph.empty
    , simulation = Force.simulation []
    , nodeCountToGenerate = 10
    , windowSize = { width = 1024, height = 768 }
    }


setGraph : Graph () () -> Model -> Model
setGraph graph model =
    let
        newGraph =
            Graph.mapContexts
                (\({ node } as ctx) ->
                    { node = { id = node.id, label = entity node.id }
                    , incoming = ctx.incoming
                    , outgoing = ctx.outgoing
                    }
                )
                graph

        link { from, to } =
            ( from, to )

        forces =
            [ Force.links <| List.map link <| Graph.edges graph
            , Force.manyBody <| List.map .id <| Graph.nodes graph
            , Force.center (model.windowSize.width / 2) (model.windowSize.height / 2)
            ]
    in
    { model
        | drag = Nothing
        , graph = newGraph
        , simulation = Force.simulation forces
    }


updateNode : Position -> NodeContext Entity () -> NodeContext Entity ()
updateNode pos nodeCtx =
    let
        nodeValue =
            nodeCtx.node.label
    in
    updateContextWithValue nodeCtx
        { nodeValue
            | x = toFloat pos.x
            , y = toFloat pos.y
        }


updateContextWithValue : NodeContext Entity () -> Entity -> NodeContext Entity ()
updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = value } }


updateGraphWithList : ForceDirectedGraph -> List Entity -> ForceDirectedGraph
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


updateModel : Msg -> Model -> Model
updateModel msg ({ drag, graph, simulation, nodeCountToGenerate, windowSize } as model) =
    case msg of
        Tick ->
            let
                ( newState, list ) =
                    Force.tick simulation <| List.map .label <| Graph.nodes graph

                newGraph =
                    updateGraphWithList graph list
            in
            case drag of
                Nothing ->
                    { model
                        | graph = newGraph
                        , simulation = newState
                    }

                Just { current, index } ->
                    { model
                        | graph = newGraph |> Graph.update index (Maybe.map (updateNode current))
                        , simulation = newState
                    }

        DragStart index xy ->
            { model | drag = Just (Drag xy xy index) }

        DragAt xy ->
            case drag of
                Just { start, index } ->
                    Model (Just (Drag start xy index))
                        (Graph.update index (Maybe.map (updateNode xy)) graph)
                        (Force.reheat simulation)
                        nodeCountToGenerate
                        windowSize

                Nothing ->
                    model

        DragEnd xy ->
            case drag of
                Just { index } ->
                    Model Nothing (Graph.update index (Maybe.map (updateNode xy)) graph) simulation nodeCountToGenerate windowSize

                Nothing ->
                    model

        GenerateNewTree ->
            model

        ChangeGeneratorParam nodeCountStr ->
            let
                newNodeCountToGenerate =
                    String.toInt nodeCountStr |> Maybe.withDefault 0 |> Basics.clamp 0 1000
            in
            { model | nodeCountToGenerate = newNodeCountToGenerate }

        AddRandomTree gr ->
            setGraph gr model

        GotWindowSize newSize ->
            { model | windowSize = newSize }

        Zoom scaleFactor ->
            { model | graph = scaleGraph scaleFactor model.windowSize model.graph }


scaleGraph : Float -> WindowSize -> ForceDirectedGraph -> ForceDirectedGraph
scaleGraph scaleFactor windowSize graph =
    let
        cx =
            windowSize.width / 2

        cy =
            windowSize.height / 2
    in
    Graph.mapNodes
        (\ent ->
            { ent
                | x = scaleFactor * (ent.x - cx) + cx
                , y = scaleFactor * (ent.y - cy) + cy
            }
        )
        graph


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateNewTree ->
            let
                generationCommand =
                    Random.generate AddRandomTree (Random.Graph.randomTree model.nodeCountToGenerate)
            in
            ( model, generationCommand )

        otherMsg ->
            ( updateModel otherMsg model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        dynamicSubscriptions =
            case model.drag of
                Nothing ->
                    -- This allows us to save resources, as if the simulation is done, there is no point in subscribing to the rAF.
                    if Force.isCompleted model.simulation then
                        Sub.none

                    else
                        Browser.Events.onAnimationFrame (always Tick)

                Just _ ->
                    Sub.batch
                        [ Browser.Events.onAnimationFrame (always Tick)
                        , Browser.Events.onMouseMove (Decode.map DragAt mousePosition)
                        , Browser.Events.onMouseUp (Decode.map DragEnd mousePosition)
                        ]
    in
    Sub.batch
        [ Browser.Events.onResize (\w h -> GotWindowSize { width = toFloat w, height = toFloat h })
        , dynamicSubscriptions
        ]


linkElement : ForceDirectedGraph -> Edge () -> Svg msg
linkElement graph edge =
    let
        source =
            lookupEntity edge.from graph

        target =
            lookupEntity edge.to graph
    in
    line
        [ strokeWidth "1"
        , stroke "#aaa"
        , x1 (String.fromFloat source.x)
        , y1 (String.fromFloat source.y)
        , x2 (String.fromFloat target.x)
        , y2 (String.fromFloat target.y)
        ]
        []


lookupEntity : NodeId -> ForceDirectedGraph -> Entity
lookupEntity nodeId =
    Graph.get nodeId
        >> Maybe.map (.node >> .label)
        >> Maybe.withDefault (entity 0)


nodeElement : Node Entity -> Svg Msg
nodeElement node =
    circle
        [ r "3"
        , fill "#000"
        , SE.on "mousedown" (Decode.map (DragStart node.id) mousePosition)
        , cx (String.fromFloat node.label.x)
        , cy (String.fromFloat node.label.y)
        ]
        [ Svg.title [] [ text <| String.fromInt node.id ] ]


view : Model -> Html Msg
view model =
    div []
        [ viewControls model.nodeCountToGenerate
        , viewGraph model
        ]


viewControls : Int -> Html Msg
viewControls nodeCountToGenerate =
    div []
        [ span [] [ text "Generate random tree with " ]
        , input
            [ type_ "number"
            , HA.min "0"
            , HA.max "1000"
            , value <| String.fromInt nodeCountToGenerate
            , HA.style "width" "3em"
            , onInput ChangeGeneratorParam
            ]
            []
        , span [] [ text " nodes " ]
        , button [ onClick GenerateNewTree ] [ text "Go!" ]
        ]


viewGraph : Model -> Svg Msg
viewGraph model =
    svg
        [ width "100vw"
        , height "100vh"
        , SA.style "position:absolute;top:0;z-index:-1;width:100vw"
        , SE.on "wheel" <|
            Decode.map
                (\deltaY ->
                    Zoom <|
                        if deltaY < 0 then
                            0.8

                        else
                            1.2
                )
                mouseWheelDecoder
        ]
        [ g [ class "links" ] <| List.map (linkElement model.graph) <| Graph.edges model.graph
        , g [ class "nodes" ] <| List.map nodeElement <| Graph.nodes model.graph
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
