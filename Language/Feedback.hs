{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.Feedback where

import FOL.Language.Common

import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe

data Node = Node { outNeighbors :: [Name]
                 , inNeighbors  :: [Name]
                 , outDegree    :: Int
                 , inDegree     :: Int
                 }

type Graph = Map Name Node

addOutNeighbor :: Name -> Name -> Graph -> Graph
addOutNeighbor name1 name2 graph = Map.adjust addOutNeighbor' name1 graph
    where
      addOutNeighbor' node
          = node { outNeighbors = name2 : outNeighbors node
                 , outDegree    = outDegree node + 1
                 }

addInNeighbor  :: Name -> Name -> Graph -> Graph
addInNeighbor  name1 name2 graph = Map.adjust addInNeighbor'  name1 graph
    where
      addInNeighbor'  node
          = node { inNeighbors  = name2 : inNeighbors  node
                 , inDegree     = inDegree  node + 1
                 }

attachEdge :: Name -> Name -> Graph -> Graph
attachEdge name1 name2
    = addInNeighbor name2 name1 . addOutNeighbor name1 name2

deleteNode :: Name -> Graph -> Graph
deleteNode name graph
    | Just (Node o i _ _) <- Map.lookup name graph
    = compose (map delInNeighbor o
                       ++ map delOutNeighbor i) (Map.delete name graph)
    | otherwise
    = graph
    where
      delOutNeighbor = Map.adjust delOutNeighbor'
          where
            delOutNeighbor' node
                = node { outNeighbors = delete name (outNeighbors node)
                       , outDegree    = outDegree node - 1
                       }
      delInNeighbor  = Map.adjust delInNeighbor'
          where
            delInNeighbor'  node
                = node { inNeighbors  = delete name (inNeighbors  node)
                       , inDegree     = inDegree  node - 1
                       }

compose :: [a -> a] -> a -> a
compose = foldr (.) id

mkGraph :: [(Name,[Name])] -> Graph
mkGraph vertices = compose (map insertVertex vertices) initialGraph
    where
      initialGraph
          = Map.fromList [(name, Node [] [] 0 0) | (name, _) <- vertices]
      insertVertex (name, neighbors)
          = compose (map (attachEdge name) neighbors)

feedbackVertexSet :: [(Name,[Name])] -> [Name]
feedbackVertexSet vertices = prune (mkGraph vertices, [])
    where
      prune = pass2 . pass1
      pass1 (graph, feedback)
          = Map.foldrWithKey sweep (graph, feedback, False) graph
      sweep name node state@(graph, feedback, progress)
          | outDegree node == 0 || inDegree node == 0
          = (deleteNode name graph, feedback, True)
          | name `elem` (outNeighbors node)
          = (deleteNode name graph, name : feedback, True)
          | otherwise
          = state
      pass2 (graph, feedback, progress)
          | Map.null graph
          = feedback
          | not progress
          , let name = select graph
          = prune (deleteNode name graph, name : feedback)
          | otherwise
          = prune (graph, feedback)

select :: Graph -> Name
select = fst . maximumBy nodeMax . Map.toList
    where
      nodeMax (name1, Node _ _ o1 i1) (name2, Node _ _ o2 i2)
          | i1 * o1 > i2 * o2 = GT
          | otherwise         = LT
