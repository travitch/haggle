{-# LANGUAGE TypeFamilies #-}
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
  freeze,
  ) where

import Control.Monad.Primitive
import qualified Data.Graph.Haggle.Internal.Adapter as A
import qualified Data.Graph.Haggle.Interface as I

newtype EdgeLabeledMGraph g el m = ELMG { unELMG :: A.LabeledMGraph g () el m }
newtype EdgeLabeledGraph g el = ELG { unELG :: A.LabeledGraph g () el }

vertices :: (I.Graph g) => EdgeLabeledGraph g el -> [I.Vertex]
vertices = I.vertices . unELG
{-# INLINE vertices #-}

edges :: (I.Graph g) => EdgeLabeledGraph g el -> [I.Edge]
edges = I.edges . unELG
{-# INLINE edges #-}

successors :: (I.Graph g) => EdgeLabeledGraph g el -> I.Vertex -> [I.Vertex]
successors (ELG lg) = I.successors lg
{-# INLINE successors #-}

outEdges :: (I.Graph g) => EdgeLabeledGraph g el -> I.Vertex -> [I.Edge]
outEdges (ELG lg) = I.outEdges lg
{-# INLINE outEdges #-}

edgeExists :: (I.Graph g) => EdgeLabeledGraph g el -> I.Vertex -> I.Vertex -> Bool
edgeExists (ELG lg) = I.edgeExists lg
{-# INLINE edgeExists #-}

instance (I.Graph g) => I.Graph (EdgeLabeledGraph g el) where
  type MutableGraph (EdgeLabeledGraph g el) =
    EdgeLabeledMGraph (I.MutableGraph g) el
  vertices = vertices
  edges = edges
  successors = successors
  outEdges = outEdges
  edgeExists = edgeExists
  thaw (ELG lg) = do
    g' <- I.thaw lg
    return $ ELMG g'

predecessors :: (I.Bidirectional g) => EdgeLabeledGraph g el -> I.Vertex -> [I.Vertex]
predecessors (ELG lg) = I.predecessors lg
{-# INLINE predecessors #-}

inEdges :: (I.Bidirectional g) => EdgeLabeledGraph g el -> I.Vertex -> [I.Edge]
inEdges (ELG lg) = I.inEdges lg
{-# INLINE inEdges #-}

instance (I.Bidirectional g) => I.Bidirectional (EdgeLabeledGraph g el) where
  predecessors = predecessors
  inEdges = inEdges

edgeLabel :: EdgeLabeledGraph g el -> I.Edge -> Maybe el
edgeLabel (ELG lg) = I.edgeLabel lg
{-# INLINE edgeLabel #-}

instance I.HasEdgeLabel (EdgeLabeledGraph g el) where
  type EdgeLabel (EdgeLabeledGraph g el) = el
  edgeLabel = edgeLabel

newEdgeLabeledGraph :: (PrimMonad m, I.MGraph g)
                    => m (g m)
                    -> m (EdgeLabeledMGraph g nl m)
newEdgeLabeledGraph newG = do
  g <- A.newLabeledGraph newG
  return $ ELMG g
{-# INLINE newEdgeLabeledGraph #-}

newSizedEdgeLabeledGraph :: (PrimMonad m, I.MGraph g)
                         => (Int -> Int -> m (g m))
                         -> Int
                         -> Int
                         -> m (EdgeLabeledMGraph g el m)
newSizedEdgeLabeledGraph newG szV szE = do
  g <- A.newSizedLabeledGraph newG szV szE
  return $ ELMG g
{-# INLINE newSizedEdgeLabeledGraph #-}

addLabeledEdge :: (PrimMonad m, I.MGraph g)
               => EdgeLabeledMGraph g el m
               -> I.Vertex
               -> I.Vertex
               -> el
               -> m (Maybe I.Edge)
addLabeledEdge lg = A.addLabeledEdge (unELMG lg)
{-# INLINE addLabeledEdge #-}

addVertex :: (PrimMonad m, I.MGraph g)
          => EdgeLabeledMGraph g el m
          -> m I.Vertex
addVertex lg = I.addVertex (A.rawMGraph (unELMG lg))
{-# INLINE addVertex #-}

getEdgeLabel :: (PrimMonad m, I.MGraph g)
             => EdgeLabeledMGraph g el m
             -> I.Edge
             -> m (Maybe el)
getEdgeLabel lg = A.getEdgeLabel (unELMG lg)
{-# INLINE getEdgeLabel #-}

getSuccessors :: (PrimMonad m, I.MGraph g)
              => EdgeLabeledMGraph g el m
              -> I.Vertex
              -> m [I.Vertex]
getSuccessors lg = A.getSuccessors (unELMG lg)
{-# INLINE getSuccessors #-}

getOutEdges :: (PrimMonad m, I.MGraph g)
            => EdgeLabeledMGraph g el m -> I.Vertex -> m [I.Edge]
getOutEdges lg = A.getOutEdges (unELMG lg)
{-# INLINE getOutEdges #-}

countVertices :: (PrimMonad m, I.MGraph g) => EdgeLabeledMGraph g el m -> m Int
countVertices = A.countVertices . unELMG
{-# INLINE countVertices #-}

countEdges :: (PrimMonad m, I.MGraph g) => EdgeLabeledMGraph g el m -> m Int
countEdges = A.countEdges . unELMG
{-# INLINE countEdges #-}

getPredecessors :: (PrimMonad m, I.MBidirectional g)
                => EdgeLabeledMGraph g el m -> I.Vertex -> m [I.Vertex]
getPredecessors lg = A.getPredecessors (unELMG lg)
{-# INLINE getPredecessors #-}

getInEdges :: (PrimMonad m, I.MBidirectional g)
           => EdgeLabeledMGraph g el m -> I.Vertex -> m [I.Edge]
getInEdges lg = A.getInEdges (unELMG lg)
{-# INLINE getInEdges #-}

freeze :: (PrimMonad m, I.MGraph g)
       => EdgeLabeledMGraph g el m
       -> m (EdgeLabeledGraph (I.ImmutableGraph g) el)
freeze lg = do
  g' <- A.freeze (unELMG lg)
  return $ ELG g'
{-# INLINE freeze #-}

