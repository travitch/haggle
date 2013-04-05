{-# LANGUAGE TypeFamilies #-}
module Data.Graph.Haggle.VertexLabelAdapter (
  VertexLabeledMGraph,
  VertexLabeledGraph,
  -- * Mutable Graph API
  newVertexLabeledGraph,
  newSizedVertexLabeledGraph,
  addEdge,
  addLabeledVertex,
  getVertexLabel,
  getSuccessors,
  getOutEdges,
  countVertices,
  countEdges,
  getPredecessors,
  getInEdges,
  freeze,
  -- * Immutable Graph API
  ) where

import Control.Monad.Primitive
import qualified Data.Graph.Haggle.Internal.Adapter as A
import qualified Data.Graph.Haggle.Interface as I

newtype VertexLabeledMGraph g nl m = VLMG { unVLMG :: A.LabeledMGraph g nl () m }
newtype VertexLabeledGraph g nl = VLG { unVLG :: A.LabeledGraph g nl () }

instance (I.Graph g) => I.Graph (VertexLabeledGraph g nl) where
  type MutableGraph (VertexLabeledGraph g nl) =
    VertexLabeledMGraph (I.MutableGraph g) nl
  vertices = I.vertices . unVLG
  edges = I.edges . unVLG
  successors (VLG lg) = I.successors lg
  outEdges (VLG lg) = I.outEdges lg
  edgeExists (VLG lg) = I.edgeExists lg
  thaw (VLG lg) = do
    g' <- I.thaw lg
    return $ VLMG g'

instance I.HasVertexLabel (VertexLabeledGraph g nl) where
  type VertexLabel (VertexLabeledGraph g nl) = nl
  vertexLabel (VLG g) = I.vertexLabel g

newVertexLabeledGraph :: (PrimMonad m, I.MGraph g)
                      => m (g m)
                      -> m (VertexLabeledMGraph g nl m)
newVertexLabeledGraph newG = do
  g <- A.newLabeledGraph newG
  return $ VLMG g
{-# INLINE newVertexLabeledGraph #-}

newSizedVertexLabeledGraph :: (PrimMonad m, I.MGraph g)
                           => (Int -> Int -> m (g m))
                           -> Int
                           -> Int
                           -> m (VertexLabeledMGraph g nl m)
newSizedVertexLabeledGraph newG szV szE = do
  g <- A.newSizedLabeledGraph newG szV szE
  return $ VLMG g
{-# INLINE newSizedVertexLabeledGraph #-}

addEdge :: (PrimMonad m, I.MGraph g)
        => VertexLabeledMGraph g nl m
        -> I.Vertex
        -> I.Vertex
        -> m (Maybe I.Edge)
addEdge lg = I.addEdge (A.rawMGraph (unVLMG lg))
{-# INLINE addEdge #-}

addLabeledVertex :: (PrimMonad m, I.MGraph g)
                 => VertexLabeledMGraph g nl m
                 -> nl
                 -> m I.Vertex
addLabeledVertex lg = A.addLabeledVertex (unVLMG lg)
{-# INLINE addLabeledVertex #-}

getVertexLabel :: (PrimMonad m, I.MGraph g)
               => VertexLabeledMGraph g nl m
               -> I.Vertex
               -> m (Maybe nl)
getVertexLabel lg = A.getVertexLabel (unVLMG lg)
{-# INLINE getVertexLabel #-}

getSuccessors :: (PrimMonad m, I.MGraph g)
              => VertexLabeledMGraph g nl m
              -> I.Vertex
              -> m [I.Vertex]
getSuccessors lg = A.getSuccessors (unVLMG lg)
{-# INLINE getSuccessors #-}

getOutEdges :: (PrimMonad m, I.MGraph g)
            => VertexLabeledMGraph g nl m -> I.Vertex -> m [I.Edge]
getOutEdges lg = A.getOutEdges (unVLMG lg)
{-# INLINE getOutEdges #-}

countVertices :: (PrimMonad m, I.MGraph g) => VertexLabeledMGraph g nl m -> m Int
countVertices = A.countVertices . unVLMG
{-# INLINE countVertices #-}

countEdges :: (PrimMonad m, I.MGraph g) => VertexLabeledMGraph g nl m -> m Int
countEdges = A.countEdges . unVLMG
{-# INLINE countEdges #-}

getPredecessors :: (PrimMonad m, I.MBidirectional g)
                => VertexLabeledMGraph g nl m -> I.Vertex -> m [I.Vertex]
getPredecessors lg = A.getPredecessors (unVLMG lg)
{-# INLINE getPredecessors #-}

getInEdges :: (PrimMonad m, I.MBidirectional g)
           => VertexLabeledMGraph g nl m -> I.Vertex -> m [I.Edge]
getInEdges lg = A.getInEdges (unVLMG lg)
{-# INLINE getInEdges #-}

freeze :: (PrimMonad m, I.MGraph g)
       => VertexLabeledMGraph g nl m
       -> m (VertexLabeledGraph (I.ImmutableGraph g) nl)
freeze lg = do
  g' <- A.freeze (unVLMG lg)
  return $ VLG g'
{-# INLINE freeze #-}

