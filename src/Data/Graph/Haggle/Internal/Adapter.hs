{-# LANGUAGE TypeFamilies, PatternGuards, RankNTypes #-}
-- | This internal module implements code shared between all of the
-- adapter interfaces.  The adapters add support for vertex and edge
-- labels without modifications to the underlying graph.  Any graph
-- implementing the 'MGraph' interface can have labels added with
-- these adapters.
--
-- Analogous adapters will be added for the pure graph interface, too.
module Data.Graph.Haggle.Internal.Adapter (
  -- * Types
  LabeledMGraph(..),
  LabeledGraph(..),
  -- * Mutable graph API
  newLabeledGraph,
  newSizedLabeledGraph,
  -- * Immutable graph API
  mapVertexLabel,
  mapEdgeLabel,
  fromLabeledEdgeList,
  -- * Helpers
  ensureEdgeLabelStorage,
  ensureNodeLabelStorage,
  unsafeGetEdgeLabel
  ) where

import Control.Monad ( liftM )
import qualified Control.Monad.Primitive as P
import qualified Control.Monad.Ref as R
import Control.Monad.ST ( ST, runST )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Graph.Haggle.Classes as I
import qualified Data.Graph.Haggle.VertexMap as VM
import qualified Data.Graph.Haggle.Internal.Basic as I

-- | An adapter adding support for both vertex and edge labels for mutable
-- graphs.
data LabeledMGraph g nl el m =
  LMG { rawMGraph :: g m
      , nodeLabelStorage :: R.Ref m (MV.MVector (P.PrimState m) nl)
      , edgeLabelStorage :: R.Ref m (MV.MVector (P.PrimState m) el)
      }

-- | An adapter adding support for both vertex and edge labels for immutable
-- graphs.
data LabeledGraph g nl el =
  LG { rawGraph :: g
     , nodeLabelStore :: Vector nl
     , edgeLabelStore :: Vector el
     }

newLabeledGraph :: (I.MGraph g, P.PrimMonad m, R.MonadRef m)
                => m (g m)
                -> m (LabeledMGraph g nl el m)
newLabeledGraph newG = do
  g <- newG
  nstore <- MV.new 128
  nref <- R.newRef nstore
  estore <- MV.new 128
  eref <- R.newRef estore
  return LMG { rawMGraph = g
             , nodeLabelStorage = nref
             , edgeLabelStorage = eref
             }

newSizedLabeledGraph :: (I.MGraph g, P.PrimMonad m, R.MonadRef m)
                     => (Int -> Int -> m (g m))
                     -> Int
                     -> Int
                     -> m (LabeledMGraph g nl el m)
newSizedLabeledGraph newG szVertices szEdges = do
  g <- newG szVertices szEdges
  nstore <- MV.new szVertices
  nref <- R.newRef nstore
  estore <- MV.new szEdges
  eref <- R.newRef estore
  return LMG { rawMGraph = g
             , nodeLabelStorage = nref
             , edgeLabelStorage = eref
             }

addLabeledVertex :: (I.MGraph g, I.MAddVertex g, P.PrimMonad m, R.MonadRef m)
                 => LabeledMGraph g nl el m
                 -> nl
                 -> m I.Vertex
addLabeledVertex lg nl = do
  v <- I.addVertex (rawMGraph lg)
  ensureNodeLabelStorage lg
  nlVec <- R.readRef (nodeLabelStorage lg)
  MV.write nlVec (I.vertexId v) nl
  return v
--
-- getEdgeLabel :: (PrimMonad m, I.MGraph g)
--              => LabeledMGraph g nl el m
--              -> I.Edge
--              -> m (Maybe el)
-- getEdgeLabel lg e = do
--   nEs <- I.countEdges (rawMGraph lg)
--   case I.edgeId e >= nEs of
--     True -> return Nothing
--     False -> do
--       elVec <- readSTRef (edgeLabelStorage lg)
--       Just `liftM` MV.read elVec (I.edgeId e)

-- FIXME: Just implement this one and push the safe version to have the default
-- impl
unsafeGetEdgeLabel :: (I.MGraph g, P.PrimMonad m, R.MonadRef m)
                   => LabeledMGraph g nl el m
                   -> I.Edge
                   -> m el
unsafeGetEdgeLabel (LMG _ _ stor) (I.E eid _ _) = do
  elVec <- R.readRef stor
  MV.unsafeRead elVec eid
{-# INLINE unsafeGetEdgeLabel #-}

getVertexLabel :: (I.MGraph g, P.PrimMonad m, R.MonadRef m)
               => LabeledMGraph g nl el m
               -> I.Vertex
               -> m (Maybe nl)
getVertexLabel lg v = do
  nNs <- I.countVertices (rawMGraph lg)
  case I.vertexId v >= nNs of
    True -> return Nothing
    False -> do
      nlVec <- R.readRef (nodeLabelStorage lg)
      Just `liftM` MV.read nlVec (I.vertexId v)

addLabeledEdge :: (I.MGraph g, I.MAddEdge g, P.PrimMonad m, R.MonadRef m)
               => LabeledMGraph g nl el m
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
      elVec <- R.readRef (edgeLabelStorage lg)
      MV.write elVec (I.edgeId e') el
      return e

getSuccessors :: (I.MGraph g, P.PrimMonad m, R.MonadRef m)
              => LabeledMGraph g nl el m
              -> I.Vertex
              -> m [I.Vertex]
getSuccessors lg = I.getSuccessors (rawMGraph lg)
{-# INLINE getSuccessors #-}

getOutEdges :: (I.MGraph g, P.PrimMonad m, R.MonadRef m)
            => LabeledMGraph g nl el m -> I.Vertex -> m [I.Edge]
getOutEdges lg = I.getOutEdges (rawMGraph lg)
{-# INLINE getOutEdges #-}

countVertices :: (I.MGraph g, P.PrimMonad m, R.MonadRef m) => LabeledMGraph g nl el m -> m Int
countVertices = I.countVertices . rawMGraph
{-# INLINE countVertices #-}

countEdges :: (I.MGraph g, P.PrimMonad m, R.MonadRef m) => LabeledMGraph g nl el m -> m Int
countEdges = I.countEdges . rawMGraph
{-# INLINE countEdges #-}

getVertices :: (I.MGraph g, P.PrimMonad m, R.MonadRef m) => LabeledMGraph g nl el m -> m [I.Vertex]
getVertices = I.getVertices . rawMGraph
{-# INLINE getVertices #-}

getPredecessors :: (I.MBidirectional g, P.PrimMonad m, R.MonadRef m)
                => LabeledMGraph g nl el m -> I.Vertex -> m [I.Vertex]
getPredecessors lg = I.getPredecessors (rawMGraph lg)
{-# INLINE getPredecessors #-}

getInEdges :: (I.MBidirectional g, P.PrimMonad m, R.MonadRef m)
           => LabeledMGraph g nl el m -> I.Vertex -> m [I.Edge]
getInEdges lg = I.getInEdges (rawMGraph lg)
{-# INLINE getInEdges #-}

checkEdgeExists :: (I.MGraph g, P.PrimMonad m, R.MonadRef m)
                => LabeledMGraph g nl el m
                -> I.Vertex
                -> I.Vertex
                -> m Bool
checkEdgeExists lg = I.checkEdgeExists (rawMGraph lg)
{-# INLINE checkEdgeExists #-}

freeze :: (I.MGraph g, P.PrimMonad m, R.MonadRef m)
       => LabeledMGraph g nl el m
       -> m (LabeledGraph (I.ImmutableGraph g) nl el)
freeze lg = do
  g' <- I.freeze (rawMGraph lg)
  nc <- I.countVertices (rawMGraph lg)
  ec <- I.countEdges (rawMGraph lg)
  ns <- R.readRef (nodeLabelStorage lg)
  es <- R.readRef (edgeLabelStorage lg)
  ns' <- V.freeze (MV.take nc ns)
  es' <- V.freeze (MV.take ec es)
  return LG { rawGraph = g'
            , nodeLabelStore = ns'
            , edgeLabelStore = es'
            }

instance (I.MGraph g) => I.MGraph (LabeledMGraph g nl el) where
  type ImmutableGraph (LabeledMGraph g nl el) = LabeledGraph (I.ImmutableGraph g) nl el
  getVertices = getVertices
  getSuccessors = getSuccessors
  getOutEdges = getOutEdges
  countEdges = countEdges
  countVertices = countVertices
  checkEdgeExists = checkEdgeExists
  freeze = freeze

instance (I.MBidirectional g) => I.MBidirectional (LabeledMGraph g nl el) where
  getPredecessors = getPredecessors
  getInEdges = getInEdges

instance (I.MAddEdge g) => I.MLabeledEdge (LabeledMGraph g nl el) where
  type MEdgeLabel (LabeledMGraph g nl el) = el
  -- getEdgeLabel = getEdgeLabel
  unsafeGetEdgeLabel = unsafeGetEdgeLabel
  addLabeledEdge = addLabeledEdge

instance (I.MAddVertex g) => I.MLabeledVertex (LabeledMGraph g nl el) where
  type MVertexLabel (LabeledMGraph g nl el) = nl
  getVertexLabel = getVertexLabel
  addLabeledVertex = addLabeledVertex

vertices :: (I.Graph g) => LabeledGraph g nl el -> [I.Vertex]
vertices = I.vertices . rawGraph
{-# INLINE vertices #-}

edges :: (I.Graph g) => LabeledGraph g nl el -> [I.Edge]
edges = I.edges . rawGraph
{-# INLINE edges #-}

successors :: (I.Graph g) => LabeledGraph g nl el -> I.Vertex -> [I.Vertex]
successors lg = I.successors (rawGraph lg)
{-# INLINE successors #-}

outEdges :: (I.Graph g) => LabeledGraph g nl el -> I.Vertex -> [I.Edge]
outEdges lg = I.outEdges (rawGraph lg)
{-# INLINE outEdges #-}

edgesBetween :: (I.Graph g) => LabeledGraph g nl el -> I.Vertex -> I.Vertex -> [I.Edge]
edgesBetween lg = I.edgesBetween (rawGraph lg)
{-# INLINE edgesBetween #-}

maxVertexId :: (I.Graph g) => LabeledGraph g nl el -> Int
maxVertexId = I.maxVertexId . rawGraph
{-# INLINE maxVertexId #-}

isEmpty :: (I.Graph g) => LabeledGraph g nl el -> Bool
isEmpty = I.isEmpty . rawGraph
{-# INLINE isEmpty #-}

thaw :: (I.Thawable g, P.PrimMonad m, R.MonadRef m)
     => LabeledGraph g nl el
     -> m (LabeledMGraph (I.MutableGraph g) nl el m)
thaw lg = do
  g' <- I.thaw (rawGraph lg)
  nlVec <- V.thaw (nodeLabelStore lg)
  elVec <- V.thaw (edgeLabelStore lg)
  nref <- R.newRef nlVec
  eref <- R.newRef elVec
  return LMG { rawMGraph = g'
             , nodeLabelStorage = nref
             , edgeLabelStorage = eref
             }

instance (I.Thawable g) => I.Thawable (LabeledGraph g nl el) where
  type MutableGraph (LabeledGraph g nl el) = LabeledMGraph (I.MutableGraph g) nl el
  thaw = thaw

instance (I.Graph g) => I.Graph (LabeledGraph g nl el) where
  vertices = vertices
  edges = edges
  successors = successors
  outEdges = outEdges
  edgesBetween = edgesBetween
  maxVertexId = maxVertexId
  isEmpty = isEmpty

predecessors :: (I.Bidirectional g) => LabeledGraph g nl el -> I.Vertex -> [I.Vertex]
predecessors lg = I.predecessors (rawGraph lg)
{-# INLINE predecessors #-}

inEdges :: (I.Bidirectional g) => LabeledGraph g nl el -> I.Vertex -> [I.Edge]
inEdges lg = I.inEdges (rawGraph lg)
{-# INLINE inEdges #-}

instance (I.Bidirectional g) => I.Bidirectional (LabeledGraph g nl el) where
  predecessors = predecessors
  inEdges = inEdges

instance (I.Bidirectional g) => I.BidirectionalEdgeLabel (LabeledGraph g nl el)

edgeLabel :: LabeledGraph g nl el -> I.Edge -> Maybe el
edgeLabel lg e = edgeLabelStore lg V.!? I.edgeId e
{-# INLINE edgeLabel #-}

instance (I.Graph g) => I.HasEdgeLabel (LabeledGraph g nl el) where
  type EdgeLabel (LabeledGraph g nl el) = el
  edgeLabel = edgeLabel
  labeledEdges = labeledEdges

vertexLabel :: LabeledGraph g nl el -> I.Vertex -> Maybe nl
vertexLabel lg v = nodeLabelStore lg V.!? I.vertexId v
{-# INLINE vertexLabel #-}

instance (I.Graph g) => I.HasVertexLabel (LabeledGraph g nl el) where
  type VertexLabel (LabeledGraph g nl el) = nl
  vertexLabel = vertexLabel
  labeledVertices = labeledVertices

-- | Note that we are not just using the @nodeLabelStore@ directly.  In
-- graphs that support vertex removal, we do not want to include removed
-- vertices, so we go through the public accessor.  This is slower but easier
-- to see as correct.
labeledVertices :: (I.Graph g) => LabeledGraph g nl el -> [(I.Vertex, nl)]
labeledVertices g = map toLabVert $ I.vertices (rawGraph g)
  where
    toLabVert v =
      let Just lab = vertexLabel g v
      in (v, lab)

-- | Likewise, we use 'edges' here instead of directly reading from the edge
-- label storage array.
labeledEdges :: (I.Graph g) => LabeledGraph g nl el -> [(I.Edge, el)]
labeledEdges g = map toLabEdge $ I.edges (rawGraph g)
  where
    toLabEdge e =
      let Just lab = edgeLabel g e
      in (e, lab)

mapEdgeLabel :: LabeledGraph g nl el -> (el -> el') -> LabeledGraph g nl el'
mapEdgeLabel g f = g { edgeLabelStore = V.map f (edgeLabelStore g) }

mapVertexLabel :: LabeledGraph g nl el -> (nl -> nl') -> LabeledGraph g nl' el
mapVertexLabel g f = g { nodeLabelStore = V.map f (nodeLabelStore g) }

-- | Construct a graph from a labeled list of edges.  The node endpoint values
-- are used as vertex labels, and the last element of the triple is used as an
-- edge label.
fromLabeledEdgeList :: (Ord nl, I.MGraph g, I.MAddVertex g, I.MAddEdge g)
                    => (forall s . ST s (g (ST s)))
                    -> [(nl, nl, el)]
                    -> (LabeledGraph (I.ImmutableGraph g) nl el, VM.VertexMap nl)
fromLabeledEdgeList con es = runST $ do
  g <- newLabeledGraph con
  vm <- VM.newVertexMapRef
  mapM_ (fromListAddEdge g vm) es
  g' <- I.freeze g
  vm' <- VM.vertexMapFromRef vm
  return (g', vm')

fromListAddEdge :: (I.MAddVertex g, I.MAddEdge g, Ord nl, P.PrimMonad m, R.MonadRef m)
                => LabeledMGraph g nl el m
                -> VM.VertexMapRef nl m
                -> (nl, nl, el)
                -> m ()
fromListAddEdge g vm (src, dst, lbl) = do
  vsrc <- VM.vertexForLabelRef g vm src
  vdst <- VM.vertexForLabelRef g vm dst
  _ <- addLabeledEdge g vsrc vdst lbl
  return ()

-- Helpers

ensureEdgeLabelStorage :: (I.MGraph g, P.PrimMonad m, R.MonadRef m)
                       => LabeledMGraph g nl el m -> m ()
ensureEdgeLabelStorage lg = do
  elVec <- R.readRef (edgeLabelStorage lg)
  edgeCount <- I.countEdges (rawMGraph lg)
  let cap = MV.length elVec
  case cap > edgeCount of
    True -> return ()
    False -> do
      elVec' <- MV.grow elVec cap
      R.writeRef (edgeLabelStorage lg) elVec'

ensureNodeLabelStorage :: (I.MGraph g, P.PrimMonad m, R.MonadRef m)
                       => LabeledMGraph g nl el m -> m ()
ensureNodeLabelStorage lg = do
  nlVec <- R.readRef (nodeLabelStorage lg)
  vertCount <- I.countVertices (rawMGraph lg)
  let cap = MV.length nlVec
  case cap > vertCount of
    True -> return ()
    False -> do
      nlVec' <- MV.grow nlVec cap
      R.writeRef (nodeLabelStorage lg) nlVec'
