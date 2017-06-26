module Main exposing (main)

{-| This demonstrates laying out the characters in Les Miserables
based on their co-occurence in a scene. Try dragging the nodes!
-}

import AnimationFrame
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html
import Html.Events exposing (on)
import Json.Decode as Decode
import Mouse exposing (Position)
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


type alias Model =
    { drag : Maybe Drag
    , graph : Graph Entity ()
    , simulation : Force.State NodeId
    }


type alias Drag =
    { start : Position
    , current : Position
    , index : NodeId
    }


type alias Entity =
    Force.Entity NodeId { value : String }


init : ( Model, Cmd Msg )
init =
    let
        graph =
            Graph.mapContexts
                (\({ node } as ctx) ->
                    { ctx | node = { label = Force.entity node.id node.label, id = node.id } }
                )
                miserablesGraph

        link { from, to } =
            ( from, to )

        forces =
            [ Force.links <| List.map link <| Graph.edges graph
            , Force.manyBody <| List.map .id <| Graph.nodes graph
            , Force.center (screenWidth / 2) (screenHeight / 2)
            ]
    in
        ( Model Nothing graph (Force.simulation forces), Cmd.none )


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


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
        List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


update : Msg -> Model -> Model
update msg ({ drag, graph, simulation } as model) =
    case msg of
        Tick t ->
            let
                ( newState, list ) =
                    Force.tick simulation <| List.map .label <| Graph.nodes graph
            in
                case drag of
                    Nothing ->
                        Model drag (updateGraphWithList graph list) newState

                    Just { current, index } ->
                        Model drag (Graph.update index (Maybe.map (updateNode current)) (updateGraphWithList graph list)) newState

        DragStart index xy ->
            Model (Just (Drag xy xy index)) graph simulation

        DragAt xy ->
            case drag of
                Just { start, index } ->
                    Model (Just (Drag start xy index))
                        (Graph.update index (Maybe.map (updateNode xy)) graph)
                        (Force.reheat simulation)

                Nothing ->
                    Model Nothing graph simulation

        DragEnd xy ->
            case drag of
                Just { start, index } ->
                    Model Nothing (Graph.update index (Maybe.map (updateNode xy)) graph) simulation

                Nothing ->
                    Model Nothing graph simulation


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


nodeElement node =
    circle
        [ r "2.5"
        , fill "#000"
        , onMouseDown node.id
        , cx (toString node.label.x)
        , cy (toString node.label.y)
        ]
        [ Svg.title [] [ text node.label.value ] ]


view : Model -> Svg Msg
view model =
    svg [ width (toString screenWidth ++ "px"), height (toString screenHeight ++ "px") ]
        [ g [ class "links" ] <| List.map (linkElement model.graph) <| Graph.edges model.graph
        , g [ class "nodes" ] <| List.map nodeElement <| Graph.nodes model.graph
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }


miserablesGraph =
    Graph.fromNodeLabelsAndEdgePairs
        [ "Myriel"
        , "Napoleon"
        , "Mlle.Baptistine"
        , "Mme.Magloire"
        , "CountessdeLo"
        , "Geborand"
        , "Champtercier"
        , "Cravatte"
        , "Count"
        , "OldMan"
        , "Labarre"
        , "Valjean"
        , "Marguerite"
        , "Mme.deR"
        , "Isabeau"
        , "Gervais"
        , "Tholomyes"
        , "Listolier"
        , "Fameuil"
        , "Blacheville"
        , "Favourite"
        , "Dahlia"
        , "Zephine"
        , "Fantine"
        , "Mme.Thenardier"
        , "Thenardier"
        , "Cosette"
        , "Javert"
        , "Fauchelevent"
        , "Bamatabois"
        , "Perpetue"
        , "Simplice"
        , "Scaufflaire"
        , "Woman1"
        , "Judge"
        , "Champmathieu"
        , "Brevet"
        , "Chenildieu"
        , "Cochepaille"
        , "Pontmercy"
        , "Boulatruelle"
        , "Eponine"
        , "Anzelma"
        , "Woman2"
        , "MotherInnocent"
        , "Gribier"
        , "Jondrette"
        , "Mme.Burgon"
        , "Gavroche"
        , "Gillenormand"
        , "Magnon"
        , "Mlle.Gillenormand"
        , "Mme.Pontmercy"
        , "Mlle.Vaubois"
        , "Lt.Gillenormand"
        , "Marius"
        , "BaronessT"
        , "Mabeuf"
        , "Enjolras"
        , "Combeferre"
        , "Prouvaire"
        , "Feuilly"
        , "Courfeyrac"
        , "Bahorel"
        , "Bossuet"
        , "Joly"
        , "Grantaire"
        , "MotherPlutarch"
        , "Gueulemer"
        , "Babet"
        , "Claquesous"
        , "Montparnasse"
        , "Toussaint"
        , "Child1"
        , "Child2"
        , "Brujon"
        , "Mme.Hucheloup"
        ]
        [ ( 1, 0 )
        , ( 2, 0 )
        , ( 3, 0 )
        , ( 3, 2 )
        , ( 4, 0 )
        , ( 5, 0 )
        , ( 6, 0 )
        , ( 7, 0 )
        , ( 8, 0 )
        , ( 9, 0 )
        , ( 11, 10 )
        , ( 11, 3 )
        , ( 11, 2 )
        , ( 11, 0 )
        , ( 12, 11 )
        , ( 13, 11 )
        , ( 14, 11 )
        , ( 15, 11 )
        , ( 17, 16 )
        , ( 18, 16 )
        , ( 18, 17 )
        , ( 19, 16 )
        , ( 19, 17 )
        , ( 19, 18 )
        , ( 20, 16 )
        , ( 20, 17 )
        , ( 20, 18 )
        , ( 20, 19 )
        , ( 21, 16 )
        , ( 21, 17 )
        , ( 21, 18 )
        , ( 21, 19 )
        , ( 21, 20 )
        , ( 22, 16 )
        , ( 22, 17 )
        , ( 22, 18 )
        , ( 22, 19 )
        , ( 22, 20 )
        , ( 22, 21 )
        , ( 23, 16 )
        , ( 23, 17 )
        , ( 23, 18 )
        , ( 23, 19 )
        , ( 23, 20 )
        , ( 23, 21 )
        , ( 23, 22 )
        , ( 23, 12 )
        , ( 23, 11 )
        , ( 24, 23 )
        , ( 24, 11 )
        , ( 25, 24 )
        , ( 25, 23 )
        , ( 25, 11 )
        , ( 26, 24 )
        , ( 26, 11 )
        , ( 26, 16 )
        , ( 26, 25 )
        , ( 27, 11 )
        , ( 27, 23 )
        , ( 27, 25 )
        , ( 27, 24 )
        , ( 27, 26 )
        , ( 28, 11 )
        , ( 28, 27 )
        , ( 29, 23 )
        , ( 29, 27 )
        , ( 29, 11 )
        , ( 30, 23 )
        , ( 31, 30 )
        , ( 31, 11 )
        , ( 31, 23 )
        , ( 31, 27 )
        , ( 32, 11 )
        , ( 33, 11 )
        , ( 33, 27 )
        , ( 34, 11 )
        , ( 34, 29 )
        , ( 35, 11 )
        , ( 35, 34 )
        , ( 35, 29 )
        , ( 36, 34 )
        , ( 36, 35 )
        , ( 36, 11 )
        , ( 36, 29 )
        , ( 37, 34 )
        , ( 37, 35 )
        , ( 37, 36 )
        , ( 37, 11 )
        , ( 37, 29 )
        , ( 38, 34 )
        , ( 38, 35 )
        , ( 38, 36 )
        , ( 38, 37 )
        , ( 38, 11 )
        , ( 38, 29 )
        , ( 39, 25 )
        , ( 40, 25 )
        , ( 41, 24 )
        , ( 41, 25 )
        , ( 42, 41 )
        , ( 42, 25 )
        , ( 42, 24 )
        , ( 43, 11 )
        , ( 43, 26 )
        , ( 43, 27 )
        , ( 44, 28 )
        , ( 44, 11 )
        , ( 45, 28 )
        , ( 47, 46 )
        , ( 48, 47 )
        , ( 48, 25 )
        , ( 48, 27 )
        , ( 48, 11 )
        , ( 49, 26 )
        , ( 49, 11 )
        , ( 50, 49 )
        , ( 50, 24 )
        , ( 51, 49 )
        , ( 51, 26 )
        , ( 51, 11 )
        , ( 52, 51 )
        , ( 52, 39 )
        , ( 53, 51 )
        , ( 54, 51 )
        , ( 54, 49 )
        , ( 54, 26 )
        , ( 55, 51 )
        , ( 55, 49 )
        , ( 55, 39 )
        , ( 55, 54 )
        , ( 55, 26 )
        , ( 55, 11 )
        , ( 55, 16 )
        , ( 55, 25 )
        , ( 55, 41 )
        , ( 55, 48 )
        , ( 56, 49 )
        , ( 56, 55 )
        , ( 57, 55 )
        , ( 57, 41 )
        , ( 57, 48 )
        , ( 58, 55 )
        , ( 58, 48 )
        , ( 58, 27 )
        , ( 58, 57 )
        , ( 58, 11 )
        , ( 59, 58 )
        , ( 59, 55 )
        , ( 59, 48 )
        , ( 59, 57 )
        , ( 60, 48 )
        , ( 60, 58 )
        , ( 60, 59 )
        , ( 61, 48 )
        , ( 61, 58 )
        , ( 61, 60 )
        , ( 61, 59 )
        , ( 61, 57 )
        , ( 61, 55 )
        , ( 62, 55 )
        , ( 62, 58 )
        , ( 62, 59 )
        , ( 62, 48 )
        , ( 62, 57 )
        , ( 62, 41 )
        , ( 62, 61 )
        , ( 62, 60 )
        , ( 63, 59 )
        , ( 63, 48 )
        , ( 63, 62 )
        , ( 63, 57 )
        , ( 63, 58 )
        , ( 63, 61 )
        , ( 63, 60 )
        , ( 63, 55 )
        , ( 64, 55 )
        , ( 64, 62 )
        , ( 64, 48 )
        , ( 64, 63 )
        , ( 64, 58 )
        , ( 64, 61 )
        , ( 64, 60 )
        , ( 64, 59 )
        , ( 64, 57 )
        , ( 64, 11 )
        , ( 65, 63 )
        , ( 65, 64 )
        , ( 65, 48 )
        , ( 65, 62 )
        , ( 65, 58 )
        , ( 65, 61 )
        , ( 65, 60 )
        , ( 65, 59 )
        , ( 65, 57 )
        , ( 65, 55 )
        , ( 66, 64 )
        , ( 66, 58 )
        , ( 66, 59 )
        , ( 66, 62 )
        , ( 66, 65 )
        , ( 66, 48 )
        , ( 66, 63 )
        , ( 66, 61 )
        , ( 66, 60 )
        , ( 67, 57 )
        , ( 68, 25 )
        , ( 68, 11 )
        , ( 68, 24 )
        , ( 68, 27 )
        , ( 68, 48 )
        , ( 68, 41 )
        , ( 69, 25 )
        , ( 69, 68 )
        , ( 69, 11 )
        , ( 69, 24 )
        , ( 69, 27 )
        , ( 69, 48 )
        , ( 69, 41 )
        , ( 70, 25 )
        , ( 70, 69 )
        , ( 70, 68 )
        , ( 70, 11 )
        , ( 70, 24 )
        , ( 70, 27 )
        , ( 70, 41 )
        , ( 70, 58 )
        , ( 71, 27 )
        , ( 71, 69 )
        , ( 71, 68 )
        , ( 71, 70 )
        , ( 71, 11 )
        , ( 71, 48 )
        , ( 71, 41 )
        , ( 71, 25 )
        , ( 72, 26 )
        , ( 72, 27 )
        , ( 72, 11 )
        , ( 73, 48 )
        , ( 74, 48 )
        , ( 74, 73 )
        , ( 75, 69 )
        , ( 75, 68 )
        , ( 75, 25 )
        , ( 75, 48 )
        , ( 75, 41 )
        , ( 75, 70 )
        , ( 75, 71 )
        , ( 76, 64 )
        , ( 76, 65 )
        , ( 76, 66 )
        , ( 76, 63 )
        , ( 76, 62 )
        , ( 76, 48 )
        , ( 76, 58 )
        ]
