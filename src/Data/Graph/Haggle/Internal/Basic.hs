{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}

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

import Control.DeepSeq
import Data.Hashable
import Data.Vector.Unboxed.Deriving (derivingUnbox)

-- | An abstract representation of a vertex.
--
-- Note that the representation is currently exposed.  Do not rely on
-- this, as it is subject to change.
newtype Vertex = V Int
  deriving (Eq, Ord, Show)

$(derivingUnbox "Vertex"
  [t| Vertex -> Int |]
  [| \(V i) -> i |]
  [| V |])

instance Hashable Vertex where
  hashWithSalt = hashVertex

instance NFData Vertex where
  rnf (V i) = i `seq` ()

hashVertex :: Int -> Vertex -> Int
hashVertex s (V i) = hashWithSalt s i
{-# INLINE hashVertex #-}

-- | An edge between two vertices.
data Edge = E {-# UNPACK #-}!Int {-# UNPACK #-}!Int {-# UNPACK #-}!Int
  deriving (Eq, Ord, Show)

$(derivingUnbox "Edge"
  [t| Edge -> (Int, Int, Int) |]
  [| \(E eid src dst) -> (eid, src, dst) |]
  [| \(eid, src, dst) -> E eid src dst |])

instance Hashable Edge where
  hashWithSalt = hashEdge

instance NFData Edge where
  rnf e = e `seq` ()

hashEdge :: Int -> Edge -> Int
hashEdge s (E eid src dst) = s `hashWithSalt` eid `hashWithSalt` src `hashWithSalt` dst
{-# INLINE hashEdge #-}

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


{- Note [Edge Format]

Edges track (in order)

1) The edge unique identifier
2) The edge source
3) The edge destination

-}
