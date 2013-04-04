module Data.Graph.Haggle.Internal.Adapter (
  LabeledMGraph(..),
  LabeledGraph(..),
  newLabeledGraph,
  newSizedLabeledGraph,
  addLabeledVertex,
  addLabeledEdge,
  getEdgeLabel,
  getVertexLabel,
  getSuccessors,
  getOutEdges,
  countVertices,
  countEdges,
  getPredecessors,
  getInEdges,
  freeze,
  -- * Helpers
  ensureEdgeLabelStorage,
  ensureNodeLabelStorage
  ) where

import Control.Monad ( liftM )
import Control.Monad.Primitive
import Data.PrimRef
import Data.Vector ( MVector, Vector )
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Graph.Haggle.Interface as I

data LabeledMGraph g m nl el =
  LMG { rawMGraph :: g m
      , nodeLabelStorage :: PrimRef m (MVector (PrimState m) nl)
      , edgeLabelStorage :: PrimRef m (MVector (PrimState m) el)
      }

data LabeledGraph g nl el =
  LG { rawGraph :: g
     , nodeLabelStore :: Vector nl
     , edgeLabelStore :: Vector el
     }

newLabeledGraph :: (PrimMonad m, I.MGraph g)
                => m (g m)
                -> m (LabeledMGraph g m nl el)
newLabeledGraph newG = do
  g <- newG
  nstore <- MV.new 128
  nref <- newPrimRef nstore
  estore <- MV.new 128
  eref <- newPrimRef estore
  return LMG { rawMGraph = g
             , nodeLabelStorage = nref
             , edgeLabelStorage = eref
             }

newSizedLabeledGraph :: (PrimMonad m, I.MGraph g)
                     => (Int -> Int -> m (g m))
                     -> Int
                     -> Int
                     -> m (LabeledMGraph g m nl el)
newSizedLabeledGraph newG szVertices szEdges = do
  g <- newG szVertices szEdges
  nstore <- MV.new szVertices
  nref <- newPrimRef nstore
  estore <- MV.new szEdges
  eref <- newPrimRef estore
  return LMG { rawMGraph = g
             , nodeLabelStorage = nref
             , edgeLabelStorage = eref
             }

addLabeledVertex :: (PrimMonad m, I.MGraph g)
                 => LabeledMGraph g m nl el
                 -> nl
                 -> m I.Vertex
addLabeledVertex lg nl = do
  v <- I.addVertex (rawMGraph lg)
  ensureNodeLabelStorage lg
  nlVec <- readPrimRef (nodeLabelStorage lg)
  MV.write nlVec (I.vertexId v) nl
  return v

getEdgeLabel :: (PrimMonad m, I.MGraph g)
             => LabeledMGraph g m nl el
             -> I.Edge
             -> m (Maybe el)
getEdgeLabel lg e = do
  nEs <- I.countEdges (rawMGraph lg)
  case I.edgeId e >= nEs of
    True -> return Nothing
    False -> do
      elVec <- readPrimRef (edgeLabelStorage lg)
      Just `liftM` MV.read elVec (I.edgeId e)

getVertexLabel :: (PrimMonad m, I.MGraph g)
               => LabeledMGraph g m nl el
               -> I.Vertex
               -> m (Maybe nl)
getVertexLabel lg v = do
  nNs <- I.countVertices (rawMGraph lg)
  case I.vertexId v >= nNs of
    True -> return Nothing
    False -> do
      nlVec <- readPrimRef (nodeLabelStorage lg)
      Just `liftM` MV.read nlVec (I.vertexId v)

addLabeledEdge :: (PrimMonad m, I.MGraph g)
               => LabeledMGraph g m nl el
               -> I.Vertex
               -> I.Vertex
               -> el
               -> m (Maybe I.Edge)
addLabeledEdge lg src dst el = do
  e <- I.addEdge (rawMGraph lg) src dst
  case e of
    Nothing -> return e
    Just e' -> do
      ensureEdgeLabelStorage lg
      elVec <- readPrimRef (edgeLabelStorage lg)
      MV.write elVec (I.edgeId e') el
      return e

getSuccessors :: (PrimMonad m, I.MGraph g)
              => LabeledMGraph g m nl el
              -> I.Vertex
              -> m [I.Vertex]
getSuccessors lg = I.getSuccessors (rawMGraph lg)

getOutEdges :: (PrimMonad m, I.MGraph g)
            => LabeledMGraph g m nl el -> I.Vertex -> m [I.Edge]
getOutEdges lg = I.getOutEdges (rawMGraph lg)

countVertices :: (PrimMonad m, I.MGraph g) => LabeledMGraph g m nl el -> m Int
countVertices = I.countVertices . rawMGraph

countEdges :: (PrimMonad m, I.MGraph g) => LabeledMGraph g m nl el -> m Int
countEdges = I.countEdges . rawMGraph

getPredecessors :: (PrimMonad m, I.MBidirectional g)
                => LabeledMGraph g m nl el -> I.Vertex -> m [I.Vertex]
getPredecessors lg = I.getPredecessors (rawMGraph lg)

getInEdges :: (PrimMonad m, I.MBidirectional g)
           => LabeledMGraph g m nl el -> I.Vertex -> m [I.Edge]
getInEdges lg = I.getInEdges (rawMGraph lg)

freeze :: (PrimMonad m, I.MGraph g)
       => LabeledMGraph g m nl el
       -> m (LabeledGraph (I.ImmutableGraph g) nl el)
freeze lg = do
  g' <- I.freeze (rawMGraph lg)
  nc <- I.countVertices (rawMGraph lg)
  ec <- I.countEdges (rawMGraph lg)
  ns <- readPrimRef (nodeLabelStorage lg)
  es <- readPrimRef (edgeLabelStorage lg)
  ns' <- V.freeze (MV.take nc ns)
  es' <- V.freeze (MV.take ec es)
  return LG { rawGraph = g'
            , nodeLabelStore = ns'
            , edgeLabelStore = es'
            }

-- Helpers

ensureEdgeLabelStorage :: (PrimMonad m, I.MGraph g)
                       => LabeledMGraph g m nl el -> m ()
ensureEdgeLabelStorage lg = do
  elVec <- readPrimRef (edgeLabelStorage lg)
  edgeCount <- I.countEdges (rawMGraph lg)
  let cap = MV.length elVec
  case cap > edgeCount of
    True -> return ()
    False -> do
      elVec' <- MV.grow elVec cap
      writePrimRef (edgeLabelStorage lg) elVec'

ensureNodeLabelStorage :: (PrimMonad m, I.MGraph g)
                       => LabeledMGraph g m nl el -> m ()
ensureNodeLabelStorage lg = do
  nlVec <- readPrimRef (nodeLabelStorage lg)
  vertCount <- I.countVertices (rawMGraph lg)
  let cap = MV.length nlVec
  case cap > vertCount of
    True -> return ()
    False -> do
      nlVec' <- MV.grow nlVec cap
      writePrimRef (nodeLabelStorage lg) nlVec'
