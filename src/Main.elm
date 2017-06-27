module Main exposing (main)

{-| This module aims to test performance of Visualization.Force
using randomly generated Trees.
-}

import AnimationFrame
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html exposing (Html, button, div, input, span)
import Html.Attributes as HA exposing (size, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
import Mouse exposing (Position)
import Random
import Random.Graph
import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
import Time exposing (Time)
import Visualization.Force as Force exposing (State)


screenWidth : Float
screenWidth =
    990


screenHeight : Float
screenHeight =
    504


type Msg
    = DragStart NodeId Position
    | DragAt Position
    | DragEnd Position
    | Tick Time
    | GenerateNewTree
    | AddRandomTree (Graph () ())
    | ChangeGeneratorParam String


type alias Model =
    { drag : Maybe Drag
    , graph : ForceDirectedGraph
    , simulation : Force.State NodeId
    , nodeCountToGenerate : Int
    }


type alias Drag =
    { start : Position
    , current : Position
    , index : NodeId
    }


type alias Entity =
    Force.Entity NodeId { value : String }


type alias ForceDirectedGraph =
    Graph Entity ()


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    Model Nothing Graph.empty (Force.simulation []) 10


setGraph : Graph () () -> Model -> Model
setGraph graph model =
    let
        newGraph =
            graph
                |> initNodeLabels
                |> Graph.mapContexts
                    (\({ node } as ctx) ->
                        { ctx | node = { label = Force.entity node.id node.label, id = node.id } }
                    )

        link { from, to } =
            ( from, to )

        forces =
            [ Force.links <| List.map link <| Graph.edges graph
            , Force.manyBody <| List.map .id <| Graph.nodes graph
            , Force.center (screenWidth / 2) (screenHeight / 2)
            ]
    in
        { model | drag = Nothing, graph = newGraph, simulation = Force.simulation forces }



{- Set node Ids as node labels -}


initNodeLabels : Graph () () -> Graph String ()
initNodeLabels inputGraph =
    Graph.mapContexts
        (\({ node } as ctx) ->
            { ctx | node = { node | label = toString node.id } }
        )
        inputGraph


updateNode : Position -> NodeContext Entity () -> NodeContext Entity ()
updateNode pos nodeCtx =
    let
        nodeValue =
            nodeCtx.node.label
    in
        updateContextWithValue nodeCtx { nodeValue | x = toFloat pos.x, y = toFloat pos.y }


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
updateModel msg ({ drag, graph, simulation, nodeCountToGenerate } as model) =
    case msg of
        Tick t ->
            let
                ( newState, list ) =
                    Force.tick simulation <| List.map .label <| Graph.nodes graph
            in
                case drag of
                    Nothing ->
                        Model drag (updateGraphWithList graph list) newState nodeCountToGenerate

                    Just { current, index } ->
                        Model drag (Graph.update index (Maybe.map (updateNode current)) (updateGraphWithList graph list)) newState nodeCountToGenerate

        DragStart index xy ->
            Model (Just (Drag xy xy index)) graph simulation nodeCountToGenerate

        DragAt xy ->
            case drag of
                Just { start, index } ->
                    Model (Just (Drag start xy index))
                        (Graph.update index (Maybe.map (updateNode xy)) graph)
                        (Force.reheat simulation)
                        nodeCountToGenerate

                Nothing ->
                    Model Nothing graph simulation nodeCountToGenerate

        DragEnd xy ->
            case drag of
                Just { start, index } ->
                    Model Nothing (Graph.update index (Maybe.map (updateNode xy)) graph) simulation nodeCountToGenerate

                Nothing ->
                    Model Nothing graph simulation nodeCountToGenerate

        GenerateNewTree ->
            model

        ChangeGeneratorParam nodeCountStr ->
            let
                newNodeCountToGenerate =
                    String.toInt nodeCountStr |> Result.withDefault 0
            in
                { model | nodeCountToGenerate = newNodeCountToGenerate }

        AddRandomTree graph ->
            setGraph graph model


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
    case model.drag of
        Nothing ->
            -- This allows us to save resources, as if the simulation is done, there is no point in subscribing
            -- to the rAF.
            if Force.isCompleted model.simulation then
                Sub.none
            else
                AnimationFrame.times Tick

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd, AnimationFrame.times Tick ]


onMouseDown : NodeId -> Attribute Msg
onMouseDown index =
    on "mousedown" (Decode.map (DragStart index) Mouse.position)


linkElement : ForceDirectedGraph -> Edge () -> Svg msg
linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    in
        line
            [ strokeWidth "1"
            , stroke "#aaa"
            , x1 (toString source.x)
            , y1 (toString source.y)
            , x2 (toString target.x)
            , y2 (toString target.y)
            ]
            []


nodeElement : Node Entity -> Svg Msg
nodeElement node =
    circle
        [ r "2.5"
        , fill "#000"
        , onMouseDown node.id
        , cx (toString node.label.x)
        , cy (toString node.label.y)
        ]
        [ Svg.title [] [ text node.label.value ] ]


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
            , value <| toString nodeCountToGenerate
            , HA.style [ ( "width", "3em" ) ]
            , onInput ChangeGeneratorParam
            ]
            []
        , span [] [ text " nodes " ]
        , button [ onClick GenerateNewTree ] [ text "Go!" ]
        ]


viewGraph : Model -> Svg Msg
viewGraph model =
    svg [ width (toString screenWidth ++ "px"), height (toString screenHeight ++ "px") ]
        [ g [ class "links" ] <| List.map (linkElement model.graph) <| Graph.edges model.graph
        , g [ class "nodes" ] <| List.map nodeElement <| Graph.nodes model.graph
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
