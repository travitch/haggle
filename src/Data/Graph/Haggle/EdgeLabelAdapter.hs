module Data.Graph.Haggle.EdgeLabelAdapter (
  EdgeLabeledMGraph,
  EdgeLabeledGraph,
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
  getInEdges,
  freeze
  ) where

import Control.Monad.Primitive
import qualified Data.Graph.Haggle.Internal.Adapter as A
import qualified Data.Graph.Haggle.Interface as I

newtype EdgeLabeledMGraph g m el = ELMG { unELMG :: A.LabeledMGraph g m () el }
newtype EdgeLabeledGraph g el = ELG { unELG :: A.LabeledGraph g () el }

newEdgeLabeledGraph :: (PrimMonad m, I.MGraph g)
                    => m (g m)
                    -> m (EdgeLabeledMGraph g m nl)
newEdgeLabeledGraph newG = do
  g <- A.newLabeledGraph newG
  return $ ELMG g
{-# INLINE newEdgeLabeledGraph #-}

newSizedEdgeLabeledGraph :: (PrimMonad m, I.MGraph g)
                         => (Int -> Int -> m (g m))
                         -> Int
                         -> Int
                         -> m (EdgeLabeledMGraph g m el)
newSizedEdgeLabeledGraph newG szV szE = do
  g <- A.newSizedLabeledGraph newG szV szE
  return $ ELMG g
{-# INLINE newSizedEdgeLabeledGraph #-}

addLabeledEdge :: (PrimMonad m, I.MGraph g)
               => EdgeLabeledMGraph g m el
               -> I.Vertex
               -> I.Vertex
               -> el
               -> m (Maybe I.Edge)
addLabeledEdge lg = A.addLabeledEdge (unELMG lg)
{-# INLINE addLabeledEdge #-}

addVertex :: (PrimMonad m, I.MGraph g)
          => EdgeLabeledMGraph g m el
          -> m I.Vertex
addVertex lg = I.addVertex (A.rawMGraph (unELMG lg))
{-# INLINE addVertex #-}

getEdgeLabel :: (PrimMonad m, I.MGraph g)
             => EdgeLabeledMGraph g m el
             -> I.Edge
             -> m (Maybe el)
getEdgeLabel lg = A.getEdgeLabel (unELMG lg)
{-# INLINE getEdgeLabel #-}

getSuccessors :: (PrimMonad m, I.MGraph g)
              => EdgeLabeledMGraph g m el
              -> I.Vertex
              -> m [I.Vertex]
getSuccessors lg = A.getSuccessors (unELMG lg)
{-# INLINE getSuccessors #-}

getOutEdges :: (PrimMonad m, I.MGraph g)
            => EdgeLabeledMGraph g m el -> I.Vertex -> m [I.Edge]
getOutEdges lg = A.getOutEdges (unELMG lg)
{-# INLINE getOutEdges #-}

countVertices :: (PrimMonad m, I.MGraph g) => EdgeLabeledMGraph g m el -> m Int
countVertices = A.countVertices . unELMG
{-# INLINE countVertices #-}

countEdges :: (PrimMonad m, I.MGraph g) => EdgeLabeledMGraph g m el -> m Int
countEdges = A.countEdges . unELMG
{-# INLINE countEdges #-}

getPredecessors :: (PrimMonad m, I.MBidirectional g)
                => EdgeLabeledMGraph g m el -> I.Vertex -> m [I.Vertex]
getPredecessors lg = A.getPredecessors (unELMG lg)
{-# INLINE getPredecessors #-}

getInEdges :: (PrimMonad m, I.MBidirectional g)
           => EdgeLabeledMGraph g m el -> I.Vertex -> m [I.Edge]
getInEdges lg = A.getInEdges (unELMG lg)
{-# INLINE getInEdges #-}

freeze :: (PrimMonad m, I.MGraph g)
       => EdgeLabeledMGraph g m el
       -> m (EdgeLabeledGraph (I.ImmutableGraph g) el)
freeze lg = do
  g' <- A.freeze (unELMG lg)
  return $ ELG g'
{-# INLINE freeze #-}

