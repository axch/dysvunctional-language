{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.Feedback where

import FOL.Language.Common

import Data.List
import Data.Maybe
import Data.Ord

import Data.Map (Map)
import qualified Data.Map as Map

data Node a = Node { outEdges :: Map a Int
                   , inEdges  :: Map a Int
                   , size     :: Int
                   }

multiplicity :: Ord a => a -> Map a Int -> Int
multiplicity name = fromMaybe 0 . Map.lookup name

outDegree, inDegree :: Ord a => a -> Node a -> Int
outDegree name = multiplicity name . outEdges
inDegree  name = multiplicity name . inEdges

totalDegree :: Map a Int -> Int
totalDegree = Map.fold (+) 0

totalOutDegree, totalInDegree :: Node a -> Int
totalOutDegree = totalDegree . outEdges
totalInDegree  = totalDegree . inEdges

type Graph a = [(a, (Int, [a]))]
type NodeMap a = Map a (Node a)

addOutEdge :: Ord a => a -> a -> NodeMap a -> NodeMap a
addOutEdge v1 v2 = Map.adjust addOutEdge' v1
    where
      addOutEdge' node
          = node {outEdges = Map.insertWith (+) v2 1 (outEdges node)}

addInEdge  :: Ord a => a -> a -> NodeMap a -> NodeMap a
addInEdge  v1 v2 = Map.adjust addInEdge'  v1
    where
      addInEdge'  node
          = node {inEdges  = Map.insertWith (+) v2 1 (inEdges  node)}

attachEdge :: Ord a => a -> a -> NodeMap a -> NodeMap a
attachEdge v1 v2 = addInEdge v2 v1 . addOutEdge v1 v2

inlineNode' :: Ord a => a -> Node a -> NodeMap a -> NodeMap a
inlineNode' scrutinee_name (Node o i s) node_map
    = compose transformations (Map.delete scrutinee_name node_map)
    where
      update_out_neighbor  = Map.adjustWithKey update_out_neighbor'
      update_out_neighbor' neighbor_name neighbor_node
          = neighbor_node {
              inEdges = Map.unionWith (+) new_in_edges old_in_edges
            }
          where
            d = multiplicity neighbor_name o
            old_in_edges = Map.delete scrutinee_name (inEdges neighbor_node)
            new_in_edges = Map.map (* d) i

      update_in_neighbor  = Map.adjustWithKey update_in_neighbor'
      update_in_neighbor' neighbor_name neighbor_node
          = neighbor_node {
              outEdges = Map.unionWith (+) new_out_edges old_out_edges
            , size = size neighbor_node + d * s
            }
          where
            d = multiplicity neighbor_name i
            old_out_edges = Map.delete scrutinee_name (outEdges neighbor_node)
            new_out_edges = Map.map (* d) o

      transformations = map update_out_neighbor (Map.keys o)
                        ++ map update_in_neighbor  (Map.keys i)

inlineNode :: Ord a => a -> NodeMap a -> NodeMap a
inlineNode name node_map
    | Just node <- Map.lookup name node_map
    = inlineNode' name node node_map
    | otherwise
    = node_map

mkNodeMap :: Ord a => Graph a -> NodeMap a
mkNodeMap graph = compose (map insert_vertex graph) initial_node_map
    where
      initial_node_map
          = Map.fromList [(name, Node Map.empty Map.empty size)
                              | (name, (size, _)) <- graph]
      insert_vertex (name, (_, neighbors))
          = compose (map (attachEdge name) neighbors)

inlineCost :: Node a -> Int
inlineCost node = size node * totalInDegree node

acceptableInlinees :: Ord a => Int -> Graph a -> [a]
acceptableInlinees threshold = prune [] 0 . mkNodeMap
    where
      prune is old_total_cost node_map
          | Map.null inlinable
          = is
          | new_total_cost <= threshold
          = prune (candidate_name:is) new_total_cost (inlineNode candidate_name node_map)
          | otherwise
          = is
          where
            isInlinable name node = outDegree name node == 0
            inlinable = Map.filterWithKey isInlinable node_map
            (candidate_name, candidate_cost) = findMinWith inlineCost inlinable
            new_total_cost = old_total_cost + candidate_cost

findMinWith :: Ord b => (a -> b) -> Map k a -> (k, b)
findMinWith cost m = minimumBy (comparing snd) [(k, cost v) | (k, v) <- Map.toList m]
