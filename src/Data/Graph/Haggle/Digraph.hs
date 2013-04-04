{-# LANGUAGE TypeFamilies #-}
module Data.Graph.Haggle.Digraph (
  MDigraph,
  Digraph,
  ) where

import Control.Monad ( when )
import Control.Monad.Primitive
import Data.PrimRef

import qualified Data.Vector.Unboxed.Mutable as MUV
import qualified Data.Vector.Unboxed as UV

import Data.Graph.Haggle.Interface

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
data MDigraph m = MDigraph { graphVertexCount :: PrimRef m Int
                       , graphEdgeRoots :: PrimRef m (MUV.MVector (PrimState m) Int)
                       , graphEdgeCount :: PrimRef m Int
                       , graphEdgeTarget :: PrimRef m (MUV.MVector (PrimState m) Int)
                       , graphEdgeNext :: PrimRef m (MUV.MVector (PrimState m) Int)
                       }

data Digraph = Digraph { edgeRoots :: UV.Vector Int
                   , edgeTargets :: UV.Vector Int
                   , edgeNexts :: UV.Vector Int
                   }

defaultSize :: Int
defaultSize = 128

instance MGraph MDigraph where
  type ImmutableGraph MDigraph = Digraph
  new = newSized defaultSize defaultSize
  newSized szNodes szEdges = do
    when (szNodes < 0 || szEdges < 0) $ error "Negative size (newSized)"
    nn <- newPrimRef 0
    en <- newPrimRef 0
    nVec <- MUV.new szNodes
    nVecRef <- newPrimRef nVec
    eTarget <- MUV.new szEdges
    eTargetRef <- newPrimRef eTarget
    eNext <- MUV.new szEdges
    eNextRef <- newPrimRef eNext
    return $! MDigraph { graphVertexCount = nn
                     , graphEdgeRoots = nVecRef
                     , graphEdgeCount = en
                     , graphEdgeTarget = eTargetRef
                     , graphEdgeNext = eNextRef
                     }

  addVertex g = do
    ensureNodeSpace g
    vid <- readPrimRef r
    modifyPrimRef' r (+1)
    vec <- readPrimRef (graphEdgeRoots g)
    MUV.write vec vid (-1)
    return (V vid)
    where
      r = graphVertexCount g

-- | Add an edge between two vertices.  Returns a unique identifier
-- for the edge.  If either the source or destination are not in the
-- graph, the function returns Nothing and does not modify the graph.
--
-- FIXME: It might be worth keeping edges in sorted order.
  addEdge g (V src) (V dst) = do
    nVerts <- readPrimRef (graphVertexCount g)
    case src >= nVerts || dst >= nVerts of
      True -> return Nothing
      False -> do
        ensureEdgeSpace g
        eid <- readPrimRef (graphEdgeCount g)
        modifyPrimRef' (graphEdgeCount g) (+1)
        rootVec <- readPrimRef (graphEdgeRoots g)
        -- The current list of edges for src
        curListHead <- MUV.read rootVec src

        -- Now create the new edge
        nextVec <- readPrimRef (graphEdgeNext g)
        targetVec <- readPrimRef (graphEdgeTarget g)
        MUV.write nextVec eid curListHead
        MUV.write targetVec eid dst

        -- The list now starts at our new edge
        MUV.write rootVec src eid
        return $ Just (E eid src dst)

  getOutEdges g (V src) = do
    nVerts <- readPrimRef (graphVertexCount g)
    case src >= nVerts of
      True -> return []
      False -> do
        roots <- readPrimRef (graphEdgeRoots g)
        lstRoot <- MUV.read roots src
        findEdges g src lstRoot

  countVertices = readPrimRef . graphVertexCount
  countEdges = readPrimRef . graphEdgeCount

-- | Lookup all of the successors of the given vertex.  There will be
-- duplicates in the result list if there are parallel edges.  The edge
-- targets are returned in an arbitrary order.
  getSuccessors g src = do
    es <- getOutEdges g src
    return $ map (\(E _ _ dst) -> V dst) es

-- | Freeze a mutable 'MGraph' into a pure 'Graph'
-- freeze :: (PrimMonad m) => MDigraph m -> m Digraph
  freeze g = do
    nVerts <- readPrimRef (graphVertexCount g)
    nEdges <- readPrimRef (graphEdgeCount g)
    roots <- readPrimRef (graphEdgeRoots g)
    targets <- readPrimRef (graphEdgeTarget g)
    nexts <- readPrimRef (graphEdgeNext g)
    roots' <- UV.freeze (MUV.take nVerts roots)
    targets' <- UV.freeze (MUV.take nEdges targets)
    nexts' <- UV.freeze (MUV.take nEdges nexts)
    return $! Digraph { edgeRoots = roots'
                    , edgeTargets = targets'
                    , edgeNexts = nexts'
                    }

instance Graph Digraph where
  type MutableGraph Digraph m = MDigraph m
  vertices = undefined
  edges = undefined
  thaw = undefined

-- Helpers

-- | Given the root of a successor list, traverse it and
-- accumulate all edges, stopping at -1.
findEdges :: (PrimMonad m) => MDigraph m -> Int -> Int -> m [Edge]
findEdges _ _ (-1) = return []
findEdges g src root = do
  targets <- readPrimRef (graphEdgeTarget g)
  nexts <- readPrimRef (graphEdgeNext g)
  let go acc (-1) = return acc
      go acc ix = do
        tgt <- MUV.read targets ix
        nxt <- MUV.read nexts ix
        go (E ix src tgt : acc) nxt
  go [] root

-- | Given a graph, ensure that there is space in the vertex vector
-- for a new vertex.  If there is not, double the capacity.
ensureNodeSpace :: (PrimMonad m) => MDigraph m -> m ()
ensureNodeSpace g = do
  vec <- readPrimRef (graphEdgeRoots g)
  let cap = MUV.length vec
  cnt <- readPrimRef (graphVertexCount g)
  case cnt < cap of
    True -> return ()
    False -> do
      vec' <- MUV.grow vec cap
      writePrimRef (graphEdgeRoots g) vec'

-- | Ensure that the graph has space for another edge.  If there is not,
-- double the edge capacity.
ensureEdgeSpace :: (PrimMonad m) => MDigraph m -> m ()
ensureEdgeSpace g = do
  v1 <- readPrimRef (graphEdgeTarget g)
  v2 <- readPrimRef (graphEdgeNext g)
  nEdges <- readPrimRef (graphEdgeCount g)
  let cap = MUV.length v1
  case nEdges < cap of
    True -> return ()
    False -> do
      v1' <- MUV.grow v1 cap
      v2' <- MUV.grow v2 cap
      writePrimRef (graphEdgeTarget g) v1'
      writePrimRef (graphEdgeNext g) v2'
