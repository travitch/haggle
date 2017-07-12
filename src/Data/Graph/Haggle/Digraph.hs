{-# LANGUAGE TypeFamilies #-}
-- | This graph implementation is a directed (multi-)graph that only tracks
-- successors.  This encoding is very compact.  It is a multi-graph because it
-- allows parallel edges between vertices.  If you require only simple graphs,
-- careful edge insertion is required (or another graph type might be more
-- appropriate).
--
-- Limitations:
--
--  * Removing nodes and edges is not currently possible.
--
--  * Predecessors are not accessible
--
--  * Edge existence tests are /linear/ in the number of edges for
--    the source node.
module Data.Graph.Haggle.Digraph (
  MDigraph,
  Digraph,
  newMDigraph,
  newSizedMDigraph
  ) where

import Control.Monad ( when )
import qualified Control.Monad.Primitive as P
import qualified Control.Monad.Ref as R
import qualified Data.Vector.Unboxed.Mutable as MUV
import qualified Data.Vector.Unboxed as UV

import Data.Graph.Haggle.Classes
import Data.Graph.Haggle.Internal.Basic

-- | This is a compact (mutable) directed graph.
data MDigraph m = -- See Note [Graph Representation]
  MDigraph { graphVertexCount :: R.Ref m Int
           , graphEdgeRoots :: R.Ref m (MUV.MVector (P.PrimState m) Int)
           , graphEdgeCount :: R.Ref m Int
           , graphEdgeTarget :: R.Ref m (MUV.MVector (P.PrimState m) Int)
           , graphEdgeNext :: R.Ref m (MUV.MVector (P.PrimState m) Int)
           }

data Digraph =
  Digraph { edgeRoots :: UV.Vector Int
          , edgeTargets :: UV.Vector Int
          , edgeNexts :: UV.Vector Int
          }

defaultSize :: Int
defaultSize = 128

-- | Create a new empty mutable graph with a small amount of storage
-- reserved for vertices and edges.
newMDigraph :: (P.PrimMonad m, R.MonadRef m) => m (MDigraph m)
newMDigraph = newSizedMDigraph defaultSize defaultSize

-- | Create a new empty graph with storage reserved for @szVerts@ vertices
-- and @szEdges@ edges.
--
-- > g <- newSizedMDigraph szVerts szEdges
newSizedMDigraph :: (P.PrimMonad m, R.MonadRef m) => Int -> Int -> m (MDigraph m)
newSizedMDigraph szNodes szEdges = do
  when (szNodes < 0 || szEdges < 0) $ error "Negative size (newSized)"
  nn <- R.newRef 0
  en <- R.newRef 0
  nVec <- MUV.new szNodes
  nVecRef <- R.newRef nVec
  eTarget <- MUV.new szEdges
  eTargetRef <- R.newRef eTarget
  eNext <- MUV.new szEdges
  eNextRef <- R.newRef eNext
  return $! MDigraph { graphVertexCount = nn
                   , graphEdgeRoots = nVecRef
                   , graphEdgeCount = en
                   , graphEdgeTarget = eTargetRef
                   , graphEdgeNext = eNextRef
                   }



instance MGraph MDigraph where
  type ImmutableGraph MDigraph = Digraph
  getVertices g = do
    nVerts <- R.readRef (graphVertexCount g)
    return [V v | v <- [0..nVerts-1]]

  getOutEdges g (V src) = do
    nVerts <- R.readRef (graphVertexCount g)
    case src >= nVerts of
      True -> return []
      False -> do
        roots <- R.readRef (graphEdgeRoots g)
        lstRoot <- MUV.unsafeRead roots src
        findEdges g src lstRoot

  countVertices = R.readRef . graphVertexCount
  countEdges = R.readRef . graphEdgeCount

  getSuccessors g src = do
    es <- getOutEdges g src
    return $ map edgeDest es

  freeze g = do
    nVerts <- R.readRef (graphVertexCount g)
    nEdges <- R.readRef (graphEdgeCount g)
    roots <- R.readRef (graphEdgeRoots g)
    targets <- R.readRef (graphEdgeTarget g)
    nexts <- R.readRef (graphEdgeNext g)
    roots' <- UV.freeze (MUV.take nVerts roots)
    targets' <- UV.freeze (MUV.take nEdges targets)
    nexts' <- UV.freeze (MUV.take nEdges nexts)
    return $! Digraph { edgeRoots = roots'
                    , edgeTargets = targets'
                    , edgeNexts = nexts'
                    }

instance MAddVertex MDigraph where
  addVertex g = do
    ensureNodeSpace g
    vid <- R.readRef r
    R.modifyRef' r (+1)
    vec <- R.readRef (graphEdgeRoots g)
    MUV.unsafeWrite vec vid (-1)
    return (V vid)
    where
      r = graphVertexCount g

instance MAddEdge MDigraph where
  addEdge g (V src) (V dst) = do
    nVerts <- R.readRef (graphVertexCount g)
    case src >= nVerts || dst >= nVerts of
      True -> return Nothing
      False -> do
        ensureEdgeSpace g
        eid <- R.readRef (graphEdgeCount g)
        R.modifyRef' (graphEdgeCount g) (+1)
        rootVec <- R.readRef (graphEdgeRoots g)
        -- The current list of edges for src
        curListHead <- MUV.unsafeRead rootVec src

        -- Now create the new edge
        nextVec <- R.readRef (graphEdgeNext g)
        targetVec <- R.readRef (graphEdgeTarget g)
        MUV.unsafeWrite nextVec eid curListHead
        MUV.unsafeWrite targetVec eid dst

        -- The list now starts at our new edge
        MUV.unsafeWrite rootVec src eid
        return $ Just (E eid src dst)

instance Thawable Digraph where
  type MutableGraph Digraph = MDigraph
  thaw g = do
    vc <- R.newRef (UV.length (edgeRoots g))
    ec <- R.newRef (UV.length (edgeTargets g))
    rvec <- UV.thaw (edgeRoots g)
    tvec <- UV.thaw (edgeTargets g)
    nvec <- UV.thaw (edgeNexts g)
    rref <- R.newRef rvec
    tref <- R.newRef tvec
    nref <- R.newRef nvec
    return MDigraph { graphVertexCount = vc
                    , graphEdgeCount = ec
                    , graphEdgeRoots = rref
                    , graphEdgeTarget = tref
                    , graphEdgeNext = nref
                    }


instance Graph Digraph where
  vertices g = map V [0 .. UV.length (edgeRoots g) - 1]
  edges g = concatMap (outEdges g) (vertices g)
  successors g (V v)
    | outOfRange g v = []
    | otherwise =
      let root = UV.unsafeIndex (edgeRoots g) v
      in pureSuccessors g root
  outEdges g (V v)
    | outOfRange g v = []
    | otherwise =
      let root = UV.unsafeIndex (edgeRoots g) v
      in pureEdges g v root
  maxVertexId g = UV.length (edgeRoots g) - 1
  isEmpty = (==0) . UV.length . edgeRoots

-- Helpers

outOfRange :: Digraph -> Int -> Bool
outOfRange g = (>= UV.length (edgeRoots g))

pureEdges :: Digraph -> Int -> Int -> [Edge]
pureEdges _ _ (-1) = []
pureEdges g src ix = E ix src dst : pureEdges g src nxt
  where
    dst = UV.unsafeIndex (edgeTargets g) ix
    nxt = UV.unsafeIndex (edgeNexts g) ix

pureSuccessors :: Digraph -> Int -> [Vertex]
pureSuccessors _ (-1) = []
pureSuccessors g ix = V s : pureSuccessors g nxt
  where
    s = UV.unsafeIndex (edgeTargets g) ix
    nxt = UV.unsafeIndex (edgeNexts g) ix

-- | Given the root of a successor list, traverse it and
-- accumulate all edges, stopping at -1.
findEdges :: (P.PrimMonad m, R.MonadRef m) => MDigraph m -> Int -> Int -> m [Edge]
findEdges _ _ (-1) = return []
findEdges g src root = do
  targets <- R.readRef (graphEdgeTarget g)
  nexts <- R.readRef (graphEdgeNext g)
  let go acc (-1) = return acc
      go acc ix = do
        tgt <- MUV.unsafeRead targets ix
        nxt <- MUV.unsafeRead nexts ix
        go (E ix src tgt : acc) nxt
  go [] root

-- | Given a graph, ensure that there is space in the vertex vector
-- for a new vertex.  If there is not, double the capacity.
ensureNodeSpace :: (P.PrimMonad m, R.MonadRef m) => MDigraph m -> m ()
ensureNodeSpace g = do
  vec <- R.readRef (graphEdgeRoots g)
  let cap = MUV.length vec
  cnt <- R.readRef (graphVertexCount g)
  case cnt < cap of
    True -> return ()
    False -> do
      vec' <- MUV.grow vec cap
      R.writeRef (graphEdgeRoots g) vec'

-- | Ensure that the graph has space for another edge.  If there is not,
-- double the edge capacity.
ensureEdgeSpace :: (P.PrimMonad m, R.MonadRef m) => MDigraph m -> m ()
ensureEdgeSpace g = do
  v1 <- R.readRef (graphEdgeTarget g)
  v2 <- R.readRef (graphEdgeNext g)
  nEdges <- R.readRef (graphEdgeCount g)
  let cap = MUV.length v1
  case nEdges < cap of
    True -> return ()
    False -> do
      v1' <- MUV.grow v1 cap
      v2' <- MUV.grow v2 cap
      R.writeRef (graphEdgeTarget g) v1'
      R.writeRef (graphEdgeNext g) v2'

{- Note [Graph Representation]

The edge roots vector is indexed by vertex id.  A -1 in the
vector indicates that there are no edges leaving the vertex.
Any other value is an index into BOTH the graphEdgeTarget and
graphEdgeNext vectors.

The graphEdgeTarget vector contains the vertex id of an edge
target.

The graphEdgeNext vector contains, at the same index, the index
of the next edge in the edge list (again into Target and Next).
A -1 indicates no more edges.

-}
