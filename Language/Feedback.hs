{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.Feedback where

import FOL.Language.Common

import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

data Node    a = Node { outNeighbors :: [a]
                      , inNeighbors  :: [a]
                      , outDegree    :: Int
                      , inDegree     :: Int
                      }
type Graph   a = [(a, [a])]
type NodeMap a = Map a (Node a)

-- Add v2 to the list of outgoing neighbors of v1.  To avoid computing
-- the length of that list, the length is stored in the node record
-- and is updated on every modification.
addOutNeighbor :: Ord a => a -> a -> NodeMap a -> NodeMap a
addOutNeighbor v1 v2 = Map.adjust addOutNeighbor' v1
    where
      addOutNeighbor' node
          = node { outNeighbors = v2 : outNeighbors node
                 , outDegree    = outDegree node + 1
                 }

-- Add v2 to the list of incoming neighbors of v1.
addInNeighbor  :: Ord a => a -> a -> NodeMap a -> NodeMap a
addInNeighbor  v1 v2 = Map.adjust addInNeighbor'  v1
    where
      addInNeighbor'  node
          = node { inNeighbors  = v2 : inNeighbors  node
                 , inDegree     = inDegree  node + 1
                 }

-- Attache an edge from v1 to v2.
attachEdge :: Ord a => a -> a -> NodeMap a -> NodeMap a
attachEdge v1 v2 = addInNeighbor v2 v1 . addOutNeighbor v1 v2

-- Remove a vertex v from the graph.  In particular, remove v from the
-- lists of outgoing and incoming neighbors of the other vertices.  It
-- there is no vertex called v in the graph, return it unchanged.
deleteNode :: Ord a => a -> NodeMap a -> NodeMap a
deleteNode v node_map
    | Just (Node o i _ _) <- Map.lookup v node_map
    = compose (map delInNeighbor o
                       ++ map delOutNeighbor i) (Map.delete v node_map)
    | otherwise
    = node_map
    where
      delOutNeighbor = Map.adjust delOutNeighbor'
          where
            delOutNeighbor' node
                = node { outNeighbors = delete v (outNeighbors node)
                       , outDegree    = outDegree node - 1
                       }
      delInNeighbor  = Map.adjust delInNeighbor'
          where
            delInNeighbor'  node
                = node { inNeighbors  = delete v (inNeighbors  node)
                       , inDegree     = inDegree  node - 1
                       }

-- Build a node map out of adjacency list representation of a graph.
mkNodeMap :: Ord a => Graph a -> NodeMap a
mkNodeMap graph = compose (map insertVertex graph) initialNodeMap
    where
      initialNodeMap
          = Map.fromList [(name, Node [] [] 0 0) | (name, _) <- graph]
      insertVertex (name, neighbors)
          = compose (map (attachEdge name) neighbors)

-- Compute a feedback vertex set of a given graph.
feedbackVertexSet :: Ord a => Graph a -> [a]
feedbackVertexSet graph = prune (mkNodeMap graph, [])
    where
      prune = pass2 . pass1
      pass1 (node_map, feedback)
          = Map.foldrWithKey sweep (node_map, feedback, False) node_map
      -- 'progress' is a flag indicating whether we have made progress.
      sweep name node state@(node_map, feedback, progress)
          | outDegree node == 0 || inDegree node == 0
          = (deleteNode name node_map,        feedback, True)
          | name `elem` outNeighbors node
          = (deleteNode name node_map, name : feedback, True)
          | otherwise
          = state
      pass2 (node_map, feedback, progress)
          | Map.null node_map
          = feedback
          -- If the first pass made no progress, heuristically remove a
          -- vertex and put it into the feedback set.
          | not progress
          , let name = select node_map
          = prune (deleteNode name node_map, name : feedback)
          | otherwise
          = prune (node_map, feedback)

-- Heuristic used to pick a node when the feedback set algorithm stalls.
select :: NodeMap a -> a
select = fst . maximumBy nodeMax . Map.toList
    where
      nodeMax (name1, Node _ _ o1 i1) (name2, Node _ _ o2 i2)
          | i1 * o1 > i2 * o2 = GT
          | otherwise         = LT
