-- | This module defines the most basic types in the library.  Their
-- representations are required in several modules, but external
-- clients should probably not rely on them.
--
-- Stability not guaranteed.
module Data.Graph.Haggle.Internal.Basic (
  Vertex(..),
  Edge(..),
  vertexId,
  edgeId,
  edgeSource,
  edgeDest
  ) where

-- | An abstract representation of a vertex.
--
-- Note that the representation is currently exposed.  Do not rely on
-- this, as it is subject to change.
newtype Vertex = V Int
  deriving (Eq, Ord, Show)

-- | An edge between two vertices.
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


