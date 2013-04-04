module Data.Graph.Haggle.EdgeLabelAdapter (
  EdgeLabeledMGraph,
  newEdgeLabeledGraph,
  newSizedEdgeLabeledGraph,
  addLabeledEdge,
  addVertex,
  getEdgeLabel,
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

newtype EdgeLabeledMGraph g m el = ELG { unELG :: A.LabeledMGraph g m () el }

newEdgeLabeledGraph :: (PrimMonad m, I.MGraph g)
                    => m (g m)
                    -> m (EdgeLabeledMGraph g m nl)
newEdgeLabeledGraph newG = do
  g <- A.newLabeledGraph newG
  return $ ELG g
{-# INLINE newEdgeLabeledGraph #-}

newSizedEdgeLabeledGraph :: (PrimMonad m, I.MGraph g)
                         => (Int -> Int -> m (g m))
                         -> Int
                         -> Int
                         -> m (EdgeLabeledMGraph g m el)
newSizedEdgeLabeledGraph newG szV szE = do
  g <- A.newSizedLabeledGraph newG szV szE
  return $ ELG g
{-# INLINE newSizedEdgeLabeledGraph #-}

addLabeledEdge :: (PrimMonad m, I.MGraph g)
               => EdgeLabeledMGraph g m el
               -> I.Vertex
               -> I.Vertex
               -> el
               -> m (Maybe I.Edge)
addLabeledEdge lg = A.addLabeledEdge (unELG lg)
{-# INLINE addLabeledEdge #-}

addVertex :: (PrimMonad m, I.MGraph g)
          => EdgeLabeledMGraph g m el
          -> m I.Vertex
addVertex lg = I.addVertex (A.rawGraph (unELG lg))
{-# INLINE addVertex #-}

getEdgeLabel :: (PrimMonad m, I.MGraph g)
             => EdgeLabeledMGraph g m el
             -> I.Edge
             -> m (Maybe el)
getEdgeLabel lg = A.getEdgeLabel (unELG lg)
{-# INLINE getEdgeLabel #-}

getSuccessors :: (PrimMonad m, I.MGraph g)
              => EdgeLabeledMGraph g m el
              -> I.Vertex
              -> m [I.Vertex]
getSuccessors lg = A.getSuccessors (unELG lg)
{-# INLINE getSuccessors #-}

getOutEdges :: (PrimMonad m, I.MGraph g)
            => EdgeLabeledMGraph g m el -> I.Vertex -> m [I.Edge]
getOutEdges lg = A.getOutEdges (unELG lg)
{-# INLINE getOutEdges #-}

countVertices :: (PrimMonad m, I.MGraph g) => EdgeLabeledMGraph g m el -> m Int
countVertices = A.countVertices . unELG
{-# INLINE countVertices #-}

countEdges :: (PrimMonad m, I.MGraph g) => EdgeLabeledMGraph g m el -> m Int
countEdges = A.countEdges . unELG
{-# INLINE countEdges #-}

getPredecessors :: (PrimMonad m, I.MBidirectional g)
                => EdgeLabeledMGraph g m el -> I.Vertex -> m [I.Vertex]
getPredecessors lg = A.getPredecessors (unELG lg)
{-# INLINE getPredecessors #-}

getInEdges :: (PrimMonad m, I.MBidirectional g)
           => EdgeLabeledMGraph g m el -> I.Vertex -> m [I.Edge]
getInEdges lg = A.getInEdges (unELG lg)
{-# INLINE getInEdges #-}

