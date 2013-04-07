{-# LANGUAGE TypeFamilies, PatternGuards, RankNTypes #-}
module Data.Graph.Haggle.VertexLabelAdapter (
  VertexLabeledMGraph,
  VertexLabeledGraph,
  -- * Mutable Graph API
  newVertexLabeledGraph,
  newSizedVertexLabeledGraph,
  -- * Immutable Graph API
  fromEdgeList
  ) where

import Control.Monad ( foldM )
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Map ( Map )
import qualified Data.Map as M

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

maxVertexId :: (I.Graph g) => VertexLabeledGraph g nl -> Int
maxVertexId = I.maxVertexId . unVLG
{-# INLINE maxVertexId #-}

isEmpty :: (I.Graph g) => VertexLabeledGraph g nl -> Bool
isEmpty = I.isEmpty . unVLG
{-# INLINE isEmpty #-}

instance (I.Graph g) => I.Graph (VertexLabeledGraph g nl) where
  type MutableGraph (VertexLabeledGraph g nl) =
    VertexLabeledMGraph (I.MutableGraph g) nl
  vertices = vertices
  edges = edges
  successors = successors
  outEdges = outEdges
  edgeExists = edgeExists
  maxVertexId = maxVertexId
  isEmpty = isEmpty
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

-- | Build a new (immutable) graph from a list of edges.  Edges are defined
-- by pairs of /node labels/.  A new 'Vertex' will be allocated for each
-- node label.
--
-- The type of the constructed graph is controlled by the first argument,
-- which is a constructor for a mutable graph.
--
-- Example:
--
-- > import Data.Graph.Haggle.VertexLabelAdapter
-- > import Data.Graph.Haggle.SimpleBiDigraph
-- >
-- > let g = fromEdgeList newMSimpleBiDigraph [(0,1), (1,2), (2,3), (3,0)]
--
-- @g@ has type SimpleBiDigraph.
--
-- An alternative that is fully polymorphic in the return type would be
-- possible, but it would require type annotations on the result of
-- 'fromEdgeList', which could be very annoying.
fromEdgeList :: (I.MGraph g, I.MAddEdge g, I.MAddVertex g, Ord nl)
             => (forall s . ST s (g (ST s)))
             -> [(nl, nl)]
             -> VertexLabeledGraph (I.ImmutableGraph g) nl
fromEdgeList con es = runST $ do
  g <- newVertexLabeledGraph con
  _ <- foldM (fromListAddEdge g) M.empty es
  I.freeze g

fromListAddEdge :: (PrimMonad m, I.MAddVertex g, I.MAddEdge g, Ord nl)
                => VertexLabeledMGraph g nl m
                -> Map nl I.Vertex
                -> (nl, nl)
                -> m (Map nl I.Vertex)
fromListAddEdge g m (src, dst) = do
  (vsrc, m1) <- getVertex g src m
  (vdst, m2) <- getVertex g dst m1
  _ <- addEdge g vsrc vdst
  return m2

getVertex :: (PrimMonad m, I.MAddVertex g, Ord nl)
          => VertexLabeledMGraph g nl m
          -> nl
          -> Map nl I.Vertex
          -> m (I.Vertex, Map nl I.Vertex)
getVertex g lbl m
  | Just v <- M.lookup lbl m = return (v, m)
  | otherwise = do
    v <- addLabeledVertex g lbl
    let m' = M.insert lbl v m
    return (v, m')

