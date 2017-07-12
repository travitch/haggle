{-# LANGUAGE TypeFamilies, PatternGuards, RankNTypes #-}
-- | An adapter to create graphs with labeled vertices and unlabeled edges.
--
-- See 'LabeledGraph' for an overview.  The only significant difference
-- is that this graph only supports adding unlabeled edges, and thus you
-- must use 'addEdge' instead of 'addLabeledEdge'.
module Data.Graph.Haggle.VertexLabelAdapter (
  VertexLabeledMGraph,
  VertexLabeledGraph,
  -- * Mutable Graph API
  newVertexLabeledGraph,
  newSizedVertexLabeledGraph,
  -- * Immutable Graph API
  mapVertexLabel,
  fromEdgeList
  ) where

import qualified Control.Monad.Primitive as P
import qualified Control.Monad.Ref as R
import Control.Monad.ST ( ST, runST )

import qualified Data.Graph.Haggle.Classes as I
import qualified Data.Graph.Haggle.VertexMap as VM
import qualified Data.Graph.Haggle.Internal.Adapter as A

newtype VertexLabeledMGraph g nl m = VLMG { unVLMG :: A.LabeledMGraph g nl () m }
newtype VertexLabeledGraph g nl = VLG { unVLG :: A.LabeledGraph g nl () }

mapVertexLabel :: VertexLabeledGraph g nl -> (nl -> nl') -> VertexLabeledGraph g nl'
mapVertexLabel g = VLG . A.mapVertexLabel (unVLG g)
{-# INLINE mapVertexLabel #-}

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

edgesBetween :: (I.Graph g) => VertexLabeledGraph g nl -> I.Vertex -> I.Vertex -> [I.Edge]
edgesBetween (VLG lg) = I.edgesBetween lg
{-# INLINE edgesBetween #-}

maxVertexId :: (I.Graph g) => VertexLabeledGraph g nl -> Int
maxVertexId = I.maxVertexId . unVLG
{-# INLINE maxVertexId #-}

isEmpty :: (I.Graph g) => VertexLabeledGraph g nl -> Bool
isEmpty = I.isEmpty . unVLG
{-# INLINE isEmpty #-}

instance (I.Graph g) => I.Graph (VertexLabeledGraph g nl) where
  vertices = vertices
  edges = edges
  successors = successors
  outEdges = outEdges
  edgesBetween = edgesBetween
  maxVertexId = maxVertexId
  isEmpty = isEmpty

instance (I.Thawable g) => I.Thawable (VertexLabeledGraph g nl) where
  type MutableGraph (VertexLabeledGraph g nl) =
    VertexLabeledMGraph (I.MutableGraph g) nl
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

vertexLabel :: (I.Graph g) => VertexLabeledGraph g nl -> I.Vertex -> Maybe nl
vertexLabel (VLG g) = I.vertexLabel g
{-# INLINE vertexLabel #-}

instance (I.Graph g) => I.HasVertexLabel (VertexLabeledGraph g nl) where
  type VertexLabel (VertexLabeledGraph g nl) = nl
  vertexLabel = vertexLabel
  labeledVertices = labeledVertices

labeledVertices :: (I.Graph g) => VertexLabeledGraph g nl -> [(I.Vertex, nl)]
labeledVertices = I.labeledVertices . unVLG
{-# INLINE labeledVertices #-}

newVertexLabeledGraph :: (I.MGraph g, P.PrimMonad m, R.MonadRef m)
                      => m (g m)
                      -> m (VertexLabeledMGraph g nl m)
newVertexLabeledGraph newG = do
  g <- A.newLabeledGraph newG
  return $ VLMG g
{-# INLINE newVertexLabeledGraph #-}

newSizedVertexLabeledGraph :: (I.MGraph g, P.PrimMonad m, R.MonadRef m)
                           => (Int -> Int -> m (g m))
                           -> Int
                           -> Int
                           -> m (VertexLabeledMGraph g nl m)
newSizedVertexLabeledGraph newG szV szE = do
  g <- A.newSizedLabeledGraph newG szV szE
  return $ VLMG g
{-# INLINE newSizedVertexLabeledGraph #-}

addEdge :: (I.MGraph g, I.MAddEdge g, P.PrimMonad m, R.MonadRef m)
        => VertexLabeledMGraph g nl m
        -> I.Vertex
        -> I.Vertex
        -> m (Maybe I.Edge)
addEdge lg = I.addEdge (A.rawMGraph (unVLMG lg))
{-# INLINE addEdge #-}

addLabeledVertex :: (I.MGraph g, I.MAddVertex g, P.PrimMonad m, R.MonadRef m)
                 => VertexLabeledMGraph g nl m
                 -> nl
                 -> m I.Vertex
addLabeledVertex lg = I.addLabeledVertex (unVLMG lg)
{-# INLINE addLabeledVertex #-}

getVertexLabel :: (I.MGraph g, I.MAddVertex g, P.PrimMonad m, R.MonadRef m)
               => VertexLabeledMGraph g nl m
               -> I.Vertex
               -> m (Maybe nl)
getVertexLabel lg = I.getVertexLabel (unVLMG lg)
{-# INLINE getVertexLabel #-}

getSuccessors :: (I.MGraph g, P.PrimMonad m, R.MonadRef m)
              => VertexLabeledMGraph g nl m
              -> I.Vertex
              -> m [I.Vertex]
getSuccessors lg = I.getSuccessors (unVLMG lg)
{-# INLINE getSuccessors #-}

getOutEdges :: (I.MGraph g, P.PrimMonad m, R.MonadRef m)
            => VertexLabeledMGraph g nl m -> I.Vertex -> m [I.Edge]
getOutEdges lg = I.getOutEdges (unVLMG lg)
{-# INLINE getOutEdges #-}

countVertices :: (I.MGraph g, P.PrimMonad m, R.MonadRef m) => VertexLabeledMGraph g nl m -> m Int
countVertices = I.countVertices . unVLMG
{-# INLINE countVertices #-}

getVertices :: (I.MGraph g, P.PrimMonad m, R.MonadRef m) => VertexLabeledMGraph g nl m -> m [I.Vertex]
getVertices = I.getVertices . unVLMG
{-# INLINE getVertices #-}

countEdges :: (I.MGraph g, P.PrimMonad m, R.MonadRef m) => VertexLabeledMGraph g nl m -> m Int
countEdges = I.countEdges . unVLMG
{-# INLINE countEdges #-}

getPredecessors :: (I.MBidirectional g, P.PrimMonad m, R.MonadRef m)
                => VertexLabeledMGraph g nl m -> I.Vertex -> m [I.Vertex]
getPredecessors lg = I.getPredecessors (unVLMG lg)
{-# INLINE getPredecessors #-}

getInEdges :: (I.MBidirectional g, P.PrimMonad m, R.MonadRef m)
           => VertexLabeledMGraph g nl m -> I.Vertex -> m [I.Edge]
getInEdges lg = I.getInEdges (unVLMG lg)
{-# INLINE getInEdges #-}

checkEdgeExists :: (I.MGraph g, P.PrimMonad m, R.MonadRef m)
                => VertexLabeledMGraph g nl m
                -> I.Vertex
                -> I.Vertex
                -> m Bool
checkEdgeExists lg = I.checkEdgeExists (unVLMG lg)
{-# INLINE checkEdgeExists #-}

freeze :: (I.MGraph g, P.PrimMonad m, R.MonadRef m)
       => VertexLabeledMGraph g nl m
       -> m (VertexLabeledGraph (I.ImmutableGraph g) nl)
freeze lg = do
  g' <- I.freeze (unVLMG lg)
  return $ VLG g'
{-# INLINE freeze #-}

instance (I.MGraph g) => I.MGraph (VertexLabeledMGraph g nl) where
  type ImmutableGraph (VertexLabeledMGraph g nl) =
    VertexLabeledGraph (I.ImmutableGraph g) nl
  getVertices = getVertices
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
             -> (VertexLabeledGraph (I.ImmutableGraph g) nl, VM.VertexMap nl)
fromEdgeList con es = runST $ do
  g <- newVertexLabeledGraph con
  vm <- VM.newVertexMapRef
  mapM_ (fromListAddEdge g vm) es
  g' <- I.freeze g
  vm' <- VM.vertexMapFromRef vm
  return (g', vm')

fromListAddEdge :: (I.MAddVertex g, I.MAddEdge g, Ord nl, P.PrimMonad m, R.MonadRef m)
                => VertexLabeledMGraph g nl m
                -> VM.VertexMapRef nl m
                -> (nl, nl)
                -> m ()
fromListAddEdge g vm (src, dst) = do
  vsrc <- VM.vertexForLabelRef g vm src
  vdst <- VM.vertexForLabelRef g vm dst
  _ <- addEdge g vsrc vdst
  return ()


