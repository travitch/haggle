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
import Control.Monad.ST
import Data.STRef

import qualified Data.Vector.Unboxed.Mutable as MUV
import qualified Data.Vector.Unboxed as UV

import Data.Graph.Haggle
import Data.Graph.Haggle.Internal.Basic

-- | This is a compact (mutable) directed graph.
data MDigraph s = -- See Note [Graph Representation]
  MDigraph { graphVertexCount :: STRef s Int
           , graphEdgeRoots :: STRef s (MUV.STVector s Int)
           , graphEdgeCount :: STRef s Int
           , graphEdgeTarget :: STRef s (MUV.STVector s Int)
           , graphEdgeNext :: STRef s (MUV.STVector s Int)
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
newMDigraph :: ST s (MDigraph s)
newMDigraph = newSizedMDigraph defaultSize defaultSize

-- | Create a new empty graph with storage reserved for @szVerts@ vertices
-- and @szEdges@ edges.
--
-- > g <- newSizedMDigraph szVerts szEdges
newSizedMDigraph :: Int -> Int -> ST s (MDigraph s)
newSizedMDigraph szNodes szEdges = do
  when (szNodes < 0 || szEdges < 0) $ error "Negative size (newSized)"
  nn <- newSTRef 0
  en <- newSTRef 0
  nVec <- MUV.new szNodes
  nVecRef <- newSTRef nVec
  eTarget <- MUV.new szEdges
  eTargetRef <- newSTRef eTarget
  eNext <- MUV.new szEdges
  eNextRef <- newSTRef eNext
  return $! MDigraph { graphVertexCount = nn
                   , graphEdgeRoots = nVecRef
                   , graphEdgeCount = en
                   , graphEdgeTarget = eTargetRef
                   , graphEdgeNext = eNextRef
                   }



instance MGraph MDigraph where
  type ImmutableGraph MDigraph = Digraph
  getVertices g = do
    nVerts <- readSTRef (graphVertexCount g)
    return [V v | v <- [0..nVerts-1]]

  getOutEdges g (V src) = do
    nVerts <- readSTRef (graphVertexCount g)
    case src >= nVerts of
      True -> return []
      False -> do
        roots <- readSTRef (graphEdgeRoots g)
        lstRoot <- MUV.unsafeRead roots src
        findEdges g src lstRoot

  countVertices = readSTRef . graphVertexCount
  countEdges = readSTRef . graphEdgeCount

  getSuccessors g src = do
    es <- getOutEdges g src
    return $ map edgeDest es

  freeze g = do
    nVerts <- readSTRef (graphVertexCount g)
    nEdges <- readSTRef (graphEdgeCount g)
    roots <- readSTRef (graphEdgeRoots g)
    targets <- readSTRef (graphEdgeTarget g)
    nexts <- readSTRef (graphEdgeNext g)
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
    vid <- readSTRef r
    modifySTRef' r (+1)
    vec <- readSTRef (graphEdgeRoots g)
    MUV.unsafeWrite vec vid (-1)
    return (V vid)
    where
      r = graphVertexCount g

instance MAddEdge MDigraph where
  addEdge g (V src) (V dst) = do
    nVerts <- readSTRef (graphVertexCount g)
    case src >= nVerts || dst >= nVerts of
      True -> return Nothing
      False -> do
        ensureEdgeSpace g
        eid <- readSTRef (graphEdgeCount g)
        modifySTRef' (graphEdgeCount g) (+1)
        rootVec <- readSTRef (graphEdgeRoots g)
        -- The current list of edges for src
        curListHead <- MUV.unsafeRead rootVec src

        -- Now create the new edge
        nextVec <- readSTRef (graphEdgeNext g)
        targetVec <- readSTRef (graphEdgeTarget g)
        MUV.unsafeWrite nextVec eid curListHead
        MUV.unsafeWrite targetVec eid dst

        -- The list now starts at our new edge
        MUV.unsafeWrite rootVec src eid
        return $ Just (E eid src dst)

instance Thawable Digraph where
  type MutableGraph Digraph = MDigraph
  thaw g = do
    vc <- newSTRef (UV.length (edgeRoots g))
    ec <- newSTRef (UV.length (edgeTargets g))
    rvec <- UV.thaw (edgeRoots g)
    tvec <- UV.thaw (edgeTargets g)
    nvec <- UV.thaw (edgeNexts g)
    rref <- newSTRef rvec
    tref <- newSTRef tvec
    nref <- newSTRef nvec
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
  edgeExists g v1 v2 = any (==v2) $ successors g v1
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
findEdges :: MDigraph s -> Int -> Int -> ST s [Edge]
findEdges _ _ (-1) = return []
findEdges g src root = do
  targets <- readSTRef (graphEdgeTarget g)
  nexts <- readSTRef (graphEdgeNext g)
  let go acc (-1) = return acc
      go acc ix = do
        tgt <- MUV.unsafeRead targets ix
        nxt <- MUV.unsafeRead nexts ix
        go (E ix src tgt : acc) nxt
  go [] root

-- | Given a graph, ensure that there is space in the vertex vector
-- for a new vertex.  If there is not, double the capacity.
ensureNodeSpace :: MDigraph s -> ST s ()
ensureNodeSpace g = do
  vec <- readSTRef (graphEdgeRoots g)
  let cap = MUV.length vec
  cnt <- readSTRef (graphVertexCount g)
  case cnt < cap of
    True -> return ()
    False -> do
      vec' <- MUV.grow vec cap
      writeSTRef (graphEdgeRoots g) vec'

-- | Ensure that the graph has space for another edge.  If there is not,
-- double the edge capacity.
ensureEdgeSpace :: MDigraph s -> ST s ()
ensureEdgeSpace g = do
  v1 <- readSTRef (graphEdgeTarget g)
  v2 <- readSTRef (graphEdgeNext g)
  nEdges <- readSTRef (graphEdgeCount g)
  let cap = MUV.length v1
  case nEdges < cap of
    True -> return ()
    False -> do
      v1' <- MUV.grow v1 cap
      v2' <- MUV.grow v2 cap
      writeSTRef (graphEdgeTarget g) v1'
      writeSTRef (graphEdgeNext g) v2'

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
