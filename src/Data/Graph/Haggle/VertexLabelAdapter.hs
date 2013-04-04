module Data.Graph.Haggle.VertexLabelAdapter (
  VertexLabeledMGraph,
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
  getInEdges
  ) where

import Control.Monad.Primitive
import qualified Data.Graph.Haggle.Internal.Adapter as A
import qualified Data.Graph.Haggle.Interface as I

newtype VertexLabeledMGraph g m nl = VLG { unVLG :: A.LabeledMGraph g m nl () }

newVertexLabeledGraph :: (PrimMonad m, I.MGraph g)
                      => m (g m)
                      -> m (VertexLabeledMGraph g m nl)
newVertexLabeledGraph newG = do
  g <- A.newLabeledGraph newG
  return $ VLG g
{-# INLINE newVertexLabeledGraph #-}

newSizedVertexLabeledGraph :: (PrimMonad m, I.MGraph g)
                           => (Int -> Int -> m (g m))
                           -> Int
                           -> Int
                           -> m (VertexLabeledMGraph g m nl)
newSizedVertexLabeledGraph newG szV szE = do
  g <- A.newSizedLabeledGraph newG szV szE
  return $ VLG g
{-# INLINE newSizedVertexLabeledGraph #-}

addEdge :: (PrimMonad m, I.MGraph g)
        => VertexLabeledMGraph g m nl
        -> I.Vertex
        -> I.Vertex
        -> m (Maybe I.Edge)
addEdge lg = I.addEdge (A.rawGraph (unVLG lg))
{-# INLINE addEdge #-}

addLabeledVertex :: (PrimMonad m, I.MGraph g)
                 => VertexLabeledMGraph g m nl
                 -> nl
                 -> m I.Vertex
addLabeledVertex lg = A.addLabeledVertex (unVLG lg)
{-# INLINE addLabeledVertex #-}

getVertexLabel :: (PrimMonad m, I.MGraph g)
               => VertexLabeledMGraph g m nl
               -> I.Vertex
               -> m (Maybe nl)
getVertexLabel lg = A.getVertexLabel (unVLG lg)
{-# INLINE getVertexLabel #-}

getSuccessors :: (PrimMonad m, I.MGraph g)
              => VertexLabeledMGraph g m nl
              -> I.Vertex
              -> m [I.Vertex]
getSuccessors lg = A.getSuccessors (unVLG lg)
{-# INLINE getSuccessors #-}

getOutEdges :: (PrimMonad m, I.MGraph g)
            => VertexLabeledMGraph g m nl -> I.Vertex -> m [I.Edge]
getOutEdges lg = A.getOutEdges (unVLG lg)
{-# INLINE getOutEdges #-}

countVertices :: (PrimMonad m, I.MGraph g) => VertexLabeledMGraph g m nl -> m Int
countVertices = A.countVertices . unVLG
{-# INLINE countVertices #-}

countEdges :: (PrimMonad m, I.MGraph g) => VertexLabeledMGraph g m nl -> m Int
countEdges = A.countEdges . unVLG
{-# INLINE countEdges #-}

getPredecessors :: (PrimMonad m, I.MBidirectional g)
                => VertexLabeledMGraph g m nl -> I.Vertex -> m [I.Vertex]
getPredecessors lg = A.getPredecessors (unVLG lg)
{-# INLINE getPredecessors #-}

getInEdges :: (PrimMonad m, I.MBidirectional g)
           => VertexLabeledMGraph g m nl -> I.Vertex -> m [I.Edge]
getInEdges lg = A.getInEdges (unVLG lg)
{-# INLINE getInEdges #-}

