module Random.Graph exposing (randomTree)

import Graph exposing (Edge, Graph, Node)
import Random exposing (Generator)
import Random.List exposing (choose, shuffle)
import Random.Extra
import Random


{-| Based on "On Generating Random Network Structures: Trees", ICCS 2003, LNCS 2658, pp. 879-887, 2003. (<http://dx.doi.org/10.1007/3-540-44862-4_95>)
-}
randomTree : Int -> Generator (Graph () ())
randomTree nodeCount =
    let
        nodes =
            List.map (\id -> Node id ()) <| List.range 0 (nodeCount - 1)
    in
        edgeListGenerator nodeCount
            |> Random.map (\edges -> Graph.fromNodesAndEdges nodes edges)



{-
   dst := random permutation of all nodes;
   src.push(dst.pop()); % picks the root
   while (!dst.empty()) {
     a := random element from src;
     b := dst.pop();
     add the edge (a, b)
     src.push(b);
   }
-}


edgeListGenerator : Int -> Generator (List (Edge ()))
edgeListGenerator nodeCount =
    let
        shuffledIds : Generator (List Graph.NodeId)
        shuffledIds =
            List.range 0 (nodeCount - 1)
                |> shuffle

        step : Graph.NodeId -> ( List Graph.NodeId, List (Edge ()) ) -> Generator ( List Graph.NodeId, List (Edge ()) )
        step targetNode ( sourceNodes, edges ) =
            let
                randomSourceGen =
                    choose sourceNodes
                        |> Random.map
                            (\( maybeSource, _ ) ->
                                case maybeSource of
                                    Nothing ->
                                        Debug.crash "edgeListGenerator: nodeCount wasn't positive"

                                    Just source ->
                                        source
                            )
            in
                randomSourceGen |> Random.map (\source -> ( targetNode :: sourceNodes, Edge source targetNode () :: edges ))

        sourceNodesAndEdgesGenerator : Generator ( List Graph.NodeId, List (Edge ()) )
        sourceNodesAndEdgesGenerator =
            shuffledIds
                |> Random.andThen
                    (\ids ->
                        case ids of
                            [] ->
                                Debug.crash "edgeListGenerator: nodeCount wasn't positive"

                            firstId :: remainingIds ->
                                foldM step ( [ firstId ], [] ) remainingIds
                    )
    in
        sourceNodesAndEdgesGenerator |> Random.map (\( sourceNodes, edges ) -> edges)


foldM : (b -> a -> Generator a) -> a -> List b -> Generator a
foldM f seed list =
    case list of
        [] ->
            Random.Extra.constant seed

        b :: bs ->
            f b seed |> Random.andThen (\newSeed -> foldM f newSeed bs)
