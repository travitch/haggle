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
import Control.Monad.ST
import Data.STRef
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Graph.Haggle as I
import qualified Data.Graph.Haggle.VertexMap as VM
import qualified Data.Graph.Haggle.Internal.Basic as I

-- | An adapter adding support for both vertex and edge labels for mutable
-- graphs.
data LabeledMGraph g nl el s =
  LMG { rawMGraph :: g s
      , nodeLabelStorage :: STRef s (MV.STVector s nl)
      , edgeLabelStorage :: STRef s (MV.STVector s el)
      }

-- | An adapter adding support for both vertex and edge labels for immutable
-- graphs.
data LabeledGraph g nl el =
  LG { rawGraph :: g
     , nodeLabelStore :: Vector nl
     , edgeLabelStore :: Vector el
     }

newLabeledGraph :: (I.MGraph g)
                => ST s (g s)
                -> ST s (LabeledMGraph g nl el s)
newLabeledGraph newG = do
  g <- newG
  nstore <- MV.new 128
  nref <- newSTRef nstore
  estore <- MV.new 128
  eref <- newSTRef estore
  return LMG { rawMGraph = g
             , nodeLabelStorage = nref
             , edgeLabelStorage = eref
             }

newSizedLabeledGraph :: (I.MGraph g)
                     => (Int -> Int -> ST s (g s))
                     -> Int
                     -> Int
                     -> ST s (LabeledMGraph g nl el s)
newSizedLabeledGraph newG szVertices szEdges = do
  g <- newG szVertices szEdges
  nstore <- MV.new szVertices
  nref <- newSTRef nstore
  estore <- MV.new szEdges
  eref <- newSTRef estore
  return LMG { rawMGraph = g
             , nodeLabelStorage = nref
             , edgeLabelStorage = eref
             }

addLabeledVertex :: (I.MGraph g, I.MAddVertex g)
                 => LabeledMGraph g nl el s
                 -> nl
                 -> ST s I.Vertex
addLabeledVertex lg nl = do
  v <- I.addVertex (rawMGraph lg)
  ensureNodeLabelStorage lg
  nlVec <- readSTRef (nodeLabelStorage lg)
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
unsafeGetEdgeLabel :: (I.MGraph g)
                   => LabeledMGraph g nl el s
                   -> I.Edge
                   -> ST s el
unsafeGetEdgeLabel (LMG _ _ stor) (I.E eid _ _) = do
--  let stor = {-# SCC "unsafeRead.edgeLabelStorage" #-} edgeLabelStorage lg
--      eid = {-# SCC "unsafeRead.eid" #-} I.edgeId e
  elVec <- {-# SCC "unsafeRead.readSTRef" #-} readSTRef stor
  l <- {-# SCC "unsafeRead.read" #-} MV.unsafeRead elVec eid
  {-# SCC "unsafeRead.ret" #-} return l
{-# INLINE unsafeGetEdgeLabel #-}

getVertexLabel :: (I.MGraph g)
               => LabeledMGraph g nl el s
               -> I.Vertex
               -> ST s (Maybe nl)
getVertexLabel lg v = do
  nNs <- I.countVertices (rawMGraph lg)
  case I.vertexId v >= nNs of
    True -> return Nothing
    False -> do
      nlVec <- readSTRef (nodeLabelStorage lg)
      Just `liftM` MV.read nlVec (I.vertexId v)

addLabeledEdge :: (I.MGraph g, I.MAddEdge g)
               => LabeledMGraph g nl el s
               -> I.Vertex
               -> I.Vertex
               -> el
               -> ST s (Maybe I.Edge)
addLabeledEdge lg src dst el = do
  e <- I.addEdge (rawMGraph lg) src dst
  case e of
    Nothing -> return e
    Just e' -> do
      ensureEdgeLabelStorage lg
      elVec <- readSTRef (edgeLabelStorage lg)
      MV.write elVec (I.edgeId e') el
      return e

getSuccessors :: (I.MGraph g)
              => LabeledMGraph g nl el s
              -> I.Vertex
              -> ST s [I.Vertex]
getSuccessors lg = I.getSuccessors (rawMGraph lg)
{-# INLINE getSuccessors #-}

getOutEdges :: (I.MGraph g)
            => LabeledMGraph g nl el s -> I.Vertex -> ST s [I.Edge]
getOutEdges lg = I.getOutEdges (rawMGraph lg)
{-# INLINE getOutEdges #-}

countVertices :: (I.MGraph g) => LabeledMGraph g nl el s -> ST s Int
countVertices = I.countVertices . rawMGraph
{-# INLINE countVertices #-}

countEdges :: (I.MGraph g) => LabeledMGraph g nl el s -> ST s Int
countEdges = I.countEdges . rawMGraph
{-# INLINE countEdges #-}

getVertices :: (I.MGraph g) => LabeledMGraph g nl el s -> ST s [I.Vertex]
getVertices = I.getVertices . rawMGraph
{-# INLINE getVertices #-}

getPredecessors :: (I.MBidirectional g)
                => LabeledMGraph g nl el s -> I.Vertex -> ST s [I.Vertex]
getPredecessors lg = I.getPredecessors (rawMGraph lg)
{-# INLINE getPredecessors #-}

getInEdges :: (I.MBidirectional g)
           => LabeledMGraph g nl el s -> I.Vertex -> ST s [I.Edge]
getInEdges lg = I.getInEdges (rawMGraph lg)
{-# INLINE getInEdges #-}

checkEdgeExists :: (I.MGraph g)
                => LabeledMGraph g nl el s
                -> I.Vertex
                -> I.Vertex
                -> ST s Bool
checkEdgeExists lg = I.checkEdgeExists (rawMGraph lg)
{-# INLINE checkEdgeExists #-}

freeze :: (I.MGraph g)
       => LabeledMGraph g nl el s
       -> ST s (LabeledGraph (I.ImmutableGraph g) nl el)
freeze lg = do
  g' <- I.freeze (rawMGraph lg)
  nc <- I.countVertices (rawMGraph lg)
  ec <- I.countEdges (rawMGraph lg)
  ns <- readSTRef (nodeLabelStorage lg)
  es <- readSTRef (edgeLabelStorage lg)
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

edgeExists :: (I.Graph g) => LabeledGraph g nl el -> I.Vertex -> I.Vertex -> Bool
edgeExists lg = I.edgeExists (rawGraph lg)
{-# INLINE edgeExists #-}

maxVertexId :: (I.Graph g) => LabeledGraph g nl el -> Int
maxVertexId = I.maxVertexId . rawGraph
{-# INLINE maxVertexId #-}

isEmpty :: (I.Graph g) => LabeledGraph g nl el -> Bool
isEmpty = I.isEmpty . rawGraph
{-# INLINE isEmpty #-}

thaw :: (I.Thawable g)
     => LabeledGraph g nl el
     -> ST s (LabeledMGraph (I.MutableGraph g) nl el s)
thaw lg = do
  g' <- I.thaw (rawGraph lg)
  nlVec <- V.thaw (nodeLabelStore lg)
  elVec <- V.thaw (edgeLabelStore lg)
  nref <- newSTRef nlVec
  eref <- newSTRef elVec
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
  edgeExists = edgeExists
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
                    => (forall s . ST s (g s))
                    -> [(nl, nl, el)]
                    -> (LabeledGraph (I.ImmutableGraph g) nl el, VM.VertexMap nl)
fromLabeledEdgeList con es = runST $ do
  g <- newLabeledGraph con
  vm <- VM.newVertexMapRef
  mapM_ (fromListAddEdge g vm) es
  g' <- I.freeze g
  vm' <- VM.vertexMapFromRef vm
  return (g', vm')

fromListAddEdge :: (I.MAddVertex g, I.MAddEdge g, Ord nl)
                => LabeledMGraph g nl el s
                -> VM.VertexMapRef nl s
                -> (nl, nl, el)
                -> ST s ()
fromListAddEdge g vm (src, dst, lbl) = do
  vsrc <- VM.vertexForLabelRef g vm src
  vdst <- VM.vertexForLabelRef g vm dst
  _ <- addLabeledEdge g vsrc vdst lbl
  return ()

-- Helpers

ensureEdgeLabelStorage :: (I.MGraph g)
                       => LabeledMGraph g nl el s -> ST s ()
ensureEdgeLabelStorage lg = do
  elVec <- readSTRef (edgeLabelStorage lg)
  edgeCount <- I.countEdges (rawMGraph lg)
  let cap = MV.length elVec
  case cap > edgeCount of
    True -> return ()
    False -> do
      elVec' <- MV.grow elVec cap
      writeSTRef (edgeLabelStorage lg) elVec'

ensureNodeLabelStorage :: (I.MGraph g)
                       => LabeledMGraph g nl el s -> ST s ()
ensureNodeLabelStorage lg = do
  nlVec <- readSTRef (nodeLabelStorage lg)
  vertCount <- I.countVertices (rawMGraph lg)
  let cap = MV.length nlVec
  case cap > vertCount of
    True -> return ()
    False -> do
      nlVec' <- MV.grow nlVec cap
      writeSTRef (nodeLabelStorage lg) nlVec'
