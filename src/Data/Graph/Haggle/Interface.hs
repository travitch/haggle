{-# LANGUAGE KindSignatures #-}
module Data.Graph.Haggle.Interface (
  Vertex(..),
  Edge(..),
  vertexId,
  edgeId,
  edgeSource,
  edgeDest,
  MGraph(..),
  MBidirectional(..),
  Graph(..)
  ) where

import Control.Monad.Primitive

newtype Vertex = V Int
  deriving (Eq, Ord, Show)
data Edge = E {-# UNPACK #-}!Int {-# UNPACK #-}!Int {-# UNPACK #-}!Int
  deriving (Eq, Ord, Show)

vertexId :: Vertex -> Int
vertexId (V vid) = vid
{-# INLINE vertexId #-}

edgeId :: Edge -> Int
edgeId (E eid _ _) = eid
{-# INLINE edgeId #-}

edgeSource :: Edge -> Vertex
edgeSource (E _ s _) = V s
{-# INLINE edgeSource #-}

edgeDest :: Edge -> Vertex
edgeDest (E _ _ d) = V d
{-# INLINE edgeDest #-}

-- FIXME: Use associated types here to define the Graph for each MGraph

class MGraph (g :: (* -> *) -> *) where
  new :: (PrimMonad m) => m (g m)
  newSized :: (PrimMonad m) => Int -> Int -> m (g m)
  addVertex :: (PrimMonad m) => g m -> m Vertex
  addEdge :: (PrimMonad m) => g m -> Vertex -> Vertex -> m (Maybe Edge)
  getSuccessors :: (PrimMonad m) => g m -> Vertex -> m [Vertex]
  getOutEdges :: (PrimMonad m) => g m -> Vertex -> m [Edge]
  countVertices :: (PrimMonad m) => g m -> m Int
  countEdges :: (PrimMonad m) => g m -> m Int
  -- freeze :: (PrimMonad m, Graph g') => g m -> m g'

class (MGraph g) => MBidirectional g where
  getPredecessors :: (PrimMonad m) => g m -> Vertex -> m [Vertex]
  getInEdges :: (PrimMonad m) => g m -> Vertex -> m [Edge]

class Graph g where
  vertices :: g -> [Vertex]
  edges :: g -> [Edge]
--  thaw :: (PrimMonad m, MGraph g') => g -> m (g' m)
