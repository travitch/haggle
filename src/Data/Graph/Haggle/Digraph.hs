module Data.Graph.Haggle.Digraph (
  Vertex,
  MGraph,
  new,
  newSized,
  addVertex,
  ) where

import Control.Monad.Primitive
import Data.PrimRef

import qualified Data.Vector.Unboxed.Mutable as MUV
import qualified Data.Vector.Unboxed as UV

-- The edge roots vector is indexed by vertex id.  A -1 in the
-- vector indicates that there are no edges leaving the vertex.
-- Any other value is an index into BOTH the graphEdgeTarget and
-- graphEdgeNext vectors.
--
-- The graphEdgeTarget vector contains the vertex id of an edge
-- target.
--
-- The graphEdgeNext vector contains, at the same index, the index
-- of the next edge in the edge list (again into Target and Next).
-- A -1 indicates no more edges.
data MGraph m = MGraph { graphVertexCount :: PrimRef m Int
                       , graphEdgeRoots :: PrimRef m (MUV.MVector (PrimState m) Int)
                       , graphEdgeCount :: PrimRef m Int
                       , graphEdgeTarget :: PrimRef m (MUV.MVector (PrimState m) Int)
                       , graphEdgeNext :: PrimRef m (MUV.MVector (PrimState m) Int)
                       }

type Vertex = Int

defaultSize :: Int
defaultSize = 128

new :: (PrimMonad m) => m (MGraph m)
new = newSized defaultSize

newSized :: (PrimMonad m) => Int -> m (MGraph m)
newSized sz = do
  nn <- newPrimRef 0
  en <- newPrimRef 0
  nVec <- MUV.new sz
  nVecRef <- newPrimRef nVec
  eTarget <- MUV.new sz
  eTargetRef <- newPrimRef eTarget
  eNext <- MUV.new sz
  eNextRef <- newPrimRef eNext
  return $! MGraph { graphVertexCount = nn
                   , graphEdgeRoots = nVecRef
                   , graphEdgeCount = en
                   , graphEdgeTarget = eTargetRef
                   , graphEdgeNext = eNextRef
                   }

-- | Add a new 'Vertex' to an 'MGraph', returning the new 'Vertex' id.
-- The new vertex is initialized to have no outgoing edges.
addVertex :: (PrimMonad m) => MGraph m -> m Vertex
addVertex g = do
  ensureSpace g
  vid <- readPrimRef r
  modifyPrimRef' r (+1)
  vec <- readPrimRef (graphEdgeRoots g)
  MUV.write vec vid (-1)
  return vid
  where
    r = graphVertexCount g

-- Helpers

-- | Given a graph, ensure that there is space in the vertex vector
-- for a new vertex.  If there is not, double the capacity.
ensureSpace :: (PrimMonad m) => MGraph m -> m ()
ensureSpace g = do
  vec <- readPrimRef (graphEdgeRoots g)
  let cap = MUV.length vec
  cnt <- readPrimRef (graphVertexCount g)
  case cnt < cap of
    True -> return ()
    False -> do
      vec' <- MUV.grow vec cap
      writePrimRef (graphEdgeRoots g) vec'

