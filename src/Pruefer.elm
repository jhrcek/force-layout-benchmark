module Pruefer exposing (..)

import Graph exposing (Graph, NodeContext, NodeId)
import IntDict


{-| Naive implementation of <https://en.wikipedia.org/wiki/Pr%C3%BCfer_sequence>

Assumes the graph is tree

-}
prueferSequence : Graph n e -> List NodeId
prueferSequence g =
    case getLeafWithMinimalId g of
        Nothing ->
            []

        Just leaf ->
            case IntDict.keys leaf.incoming of
                [ parentKey ] ->
                    parentKey :: prueferSequence (Graph.remove leaf.node.id g)

                _ ->
                    []


getLeafWithMinimalId : Graph n e -> Maybe (NodeContext n e)
getLeafWithMinimalId =
    Graph.fold
        (\ctx acc ->
            if IntDict.size ctx.outgoing == 0 then
                case acc of
                    Nothing ->
                        Just ctx

                    Just oldCtx ->
                        if ctx.node.id < oldCtx.node.id then
                            Just ctx

                        else
                            acc

            else
                acc
        )
        Nothing
