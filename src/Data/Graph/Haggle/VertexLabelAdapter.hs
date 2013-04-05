{-# LANGUAGE TypeFamilies #-}
module Data.Graph.Haggle.VertexLabelAdapter (
  VertexLabeledMGraph,
  VertexLabeledGraph,
  -- * Mutable Graph API
  newVertexLabeledGraph,
  newSizedVertexLabeledGraph,
  ) where

import Control.Monad.Primitive
import qualified Data.Graph.Haggle as I
import qualified Data.Graph.Haggle.Internal.Adapter as A

newtype VertexLabeledMGraph g nl m = VLMG { unVLMG :: A.LabeledMGraph g nl () m }
newtype VertexLabeledGraph g nl = VLG { unVLG :: A.LabeledGraph g nl () }

vertices :: (I.Graph g) => VertexLabeledGraph g nl -> [I.Vertex]
vertices = I.vertices . unVLG
{-# INLINE vertices #-}

edges :: (I.Graph g) => VertexLabeledGraph g nl -> [I.Edge]
edges = I.edges . unVLG
{-# INLINE edges #-}

successors :: (I.Graph g) => VertexLabeledGraph g nl -> I.Vertex -> [I.Vertex]
successors (VLG lg) = I.successors lg
{-# INLINE successors #-}

outEdges :: (I.Graph g) => VertexLabeledGraph g nl -> I.Vertex -> [I.Edge]
outEdges (VLG lg) = I.outEdges lg
{-# INLINE outEdges #-}

edgeExists :: (I.Graph g) => VertexLabeledGraph g nl -> I.Vertex -> I.Vertex -> Bool
edgeExists (VLG lg) = I.edgeExists lg
{-# INLINE edgeExists #-}

instance (I.Graph g) => I.Graph (VertexLabeledGraph g nl) where
  type MutableGraph (VertexLabeledGraph g nl) =
    VertexLabeledMGraph (I.MutableGraph g) nl
  vertices = vertices
  edges = edges
  successors = successors
  outEdges = outEdges
  edgeExists = edgeExists
  thaw (VLG lg) = do
    g' <- I.thaw lg
    return $ VLMG g'

predecessors :: (I.Bidirectional g) => VertexLabeledGraph g nl -> I.Vertex -> [I.Vertex]
predecessors (VLG lg) = I.predecessors lg
{-# INLINE predecessors #-}

inEdges :: (I.Bidirectional g) => VertexLabeledGraph g nl -> I.Vertex -> [I.Edge]
inEdges (VLG lg) = I.inEdges lg
{-# INLINE inEdges #-}

instance (I.Bidirectional g) => I.Bidirectional (VertexLabeledGraph g nl) where
  predecessors = predecessors
  inEdges = inEdges

vertexLabel :: VertexLabeledGraph g nl -> I.Vertex -> Maybe nl
vertexLabel (VLG g) = I.vertexLabel g
{-# INLINE vertexLabel #-}

instance I.HasVertexLabel (VertexLabeledGraph g nl) where
  type VertexLabel (VertexLabeledGraph g nl) = nl
  vertexLabel = vertexLabel

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

addEdge :: (PrimMonad m, I.MGraph g, I.MAddEdge g)
        => VertexLabeledMGraph g nl m
        -> I.Vertex
        -> I.Vertex
        -> m (Maybe I.Edge)
addEdge lg = I.addEdge (A.rawMGraph (unVLMG lg))
{-# INLINE addEdge #-}

addLabeledVertex :: (PrimMonad m, I.MGraph g, I.MAddVertex g)
                 => VertexLabeledMGraph g nl m
                 -> nl
                 -> m I.Vertex
addLabeledVertex lg = I.addLabeledVertex (unVLMG lg)
{-# INLINE addLabeledVertex #-}

getVertexLabel :: (PrimMonad m, I.MGraph g, I.MAddVertex g)
               => VertexLabeledMGraph g nl m
               -> I.Vertex
               -> m (Maybe nl)
getVertexLabel lg = I.getVertexLabel (unVLMG lg)
{-# INLINE getVertexLabel #-}

getSuccessors :: (PrimMonad m, I.MGraph g)
              => VertexLabeledMGraph g nl m
              -> I.Vertex
              -> m [I.Vertex]
getSuccessors lg = I.getSuccessors (unVLMG lg)
{-# INLINE getSuccessors #-}

getOutEdges :: (PrimMonad m, I.MGraph g)
            => VertexLabeledMGraph g nl m -> I.Vertex -> m [I.Edge]
getOutEdges lg = I.getOutEdges (unVLMG lg)
{-# INLINE getOutEdges #-}

countVertices :: (PrimMonad m, I.MGraph g) => VertexLabeledMGraph g nl m -> m Int
countVertices = I.countVertices . unVLMG
{-# INLINE countVertices #-}

countEdges :: (PrimMonad m, I.MGraph g) => VertexLabeledMGraph g nl m -> m Int
countEdges = I.countEdges . unVLMG
{-# INLINE countEdges #-}

getPredecessors :: (PrimMonad m, I.MBidirectional g)
                => VertexLabeledMGraph g nl m -> I.Vertex -> m [I.Vertex]
getPredecessors lg = I.getPredecessors (unVLMG lg)
{-# INLINE getPredecessors #-}

getInEdges :: (PrimMonad m, I.MBidirectional g)
           => VertexLabeledMGraph g nl m -> I.Vertex -> m [I.Edge]
getInEdges lg = I.getInEdges (unVLMG lg)
{-# INLINE getInEdges #-}

checkEdgeExists :: (PrimMonad m, I.MGraph g)
                => VertexLabeledMGraph g nl m
                -> I.Vertex
                -> I.Vertex
                -> m Bool
checkEdgeExists lg = I.checkEdgeExists (unVLMG lg)
{-# INLINE checkEdgeExists #-}

freeze :: (PrimMonad m, I.MGraph g)
       => VertexLabeledMGraph g nl m
       -> m (VertexLabeledGraph (I.ImmutableGraph g) nl)
freeze lg = do
  g' <- I.freeze (unVLMG lg)
  return $ VLG g'
{-# INLINE freeze #-}

instance (I.MGraph g) => I.MGraph (VertexLabeledMGraph g nl) where
  type ImmutableGraph (VertexLabeledMGraph g nl) =
    VertexLabeledGraph (I.ImmutableGraph g) nl
  getSuccessors = getSuccessors
  getOutEdges = getOutEdges
  countVertices = countVertices
  countEdges = countEdges
  checkEdgeExists = checkEdgeExists
  freeze = freeze

instance (I.MAddVertex g) => I.MLabeledVertex (VertexLabeledMGraph g nl) where
  type MVertexLabel (VertexLabeledMGraph g nl) = nl
  getVertexLabel = getVertexLabel
  addLabeledVertex = addLabeledVertex

instance (I.MBidirectional g) => I.MBidirectional (VertexLabeledMGraph g nl) where
  getPredecessors = getPredecessors
  getInEdges = getInEdges

instance (I.MAddEdge g) => I.MAddEdge (VertexLabeledMGraph g nl) where
  addEdge = addEdge


