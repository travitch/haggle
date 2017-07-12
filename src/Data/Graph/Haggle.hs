{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
-- | Haggle is a Haskell graph library.
--
-- The main idea behind haggle is that graphs are constructed with mutation
-- (either in 'IO' or 'ST').  After the graph is constructed, it is frozen
-- into an immutable graph.  This split is a major difference between
-- haggle and the other major Haskell graph library, fgl, which is
-- formulated in terms of inductive graphs that can always be modified
-- in a purely-functional way.  Supporting the inductive graph interface
-- severely limits implementation choices and optimization opportunities, so
-- haggle tries a different approach.
--
-- Furthermore, the types of vertices (nodes in FGL) and edges are held
-- as abstract in haggle, allowing for changes later if necessary.  That said,
-- changes are unlikely and the representations are exposed (with no
-- guarantees) through an Internal module.
--
-- Enough talk, example time:
--
-- > import Control.Monad ( replicateM )
-- > import Data.Graph.Haggle
-- > import Data.Graph.Haggle.Digraph
-- > import Data.Graph.Haggle.Algorithms.DFS
-- >
-- > main :: IO ()
-- > main = do
-- >   g <- newMDigraph
-- >   [v0, v1, v2] <- replicateM 3 (addVertex g)
-- >   e1 <- addEdge g v0 v1
-- >   e2 <- addEdge g v1 v2
-- >   gi <- freeze g
-- >   print (dfs gi v1) -- [V 1, V 2] since the first vertex is 0
--
-- The example builds a graph with three vertices and performs a DFS
-- from the middle vertex.  Note that the DFS algorithm is implemented on
-- immutable graphs, so we freeze the mutable graph before traversing it.  The
-- graph type in this example is a directed graph.
--
-- There are other graph variants that support efficient access to predecessor
-- edges: bidirectional graphs.  There are also simple graph variants that
-- prohibit parallel edges.
--
-- The core graph implementations support only vertices and edges.  /Adapters/
-- add support for 'Vertex' and 'Edge' labels.  See 'EdgeLabelAdapter',
-- 'VertexLabelAdapter', and 'LabelAdapter' (which supports both).  This
-- split allows the core implementations of graphs and graph algorithms to
-- be fast and compact (since they do not need to allocate storage for or
-- manipulate labels).  The adapters store labels on the side, similarly
-- to the property maps of Boost Graph Library.  Also note that the adapters
-- are strongly typed.  To add edges to a graph with edge labels, you must call
-- 'addLabeledEdge' instead of 'addEdge'.  Likewise for graphs with vertex
-- labels and 'addLabeledVertex'/'addVertex'.  This requirement is enforced
-- in the type system so that labels cannot become out-of-sync with the
-- structure of the graph.  The adapters each work with any type of underlying
-- graph.
module Data.Graph.Haggle (
  -- * Graph types
  -- ** Mutable graphs
  D.MDigraph,
  D.newMDigraph,
  D.newSizedMDigraph,
  B.MBiDigraph,
  B.newMBiDigraph,
  B.newSizedMBiDigraph,
  SBD.MSimpleBiDigraph,
  SBD.newMSimpleBiDigraph,
  SBD.newSizedMSimpleBiDigraph,
  -- *** Adapters
  EA.EdgeLabeledMGraph,
  EA.newEdgeLabeledGraph,
  EA.newSizedEdgeLabeledGraph,
  VA.VertexLabeledMGraph,
  VA.newVertexLabeledGraph,
  VA.newSizedVertexLabeledGraph,
  A.LabeledMGraph,
  A.newLabeledGraph,
  A.newSizedLabeledGraph,
  -- ** Immutable graphs
  D.Digraph,
  B.BiDigraph,
  SBD.SimpleBiDigraph,
  -- *** Adapters
  EA.EdgeLabeledGraph,
  VA.VertexLabeledGraph,
  VA.fromEdgeList,
  A.LabeledGraph,
  A.fromLabeledEdgeList,
  -- ** Inductive graphs
  PT.PatriciaTree,
  -- * Basic types
  I.Vertex,
  I.Edge,
  I.edgeSource,
  I.edgeDest,

  -- * Mutable graph operations
  getVertices,
  getSuccessors,
  getOutEdges,
  countVertices,
  countEdges,
  checkEdgeExists,
  freeze,

  addVertex,
  addEdge,

  getEdgeLabel,
  unsafeGetEdgeLabel,
  addLabeledEdge,

  getVertexLabel,
  addLabeledVertex,
  getLabeledVertices,

  removeVertex,
  removeEdgesBetween,
  removeEdge,


  getPredecessors,
  getInEdges,

  -- ** Mutable labeled graph operations
  A.mapEdgeLabel,
  A.mapVertexLabel,

  -- * Immutable graph operations
  vertices,
  edges,
  successors,
  outEdges,
  edgesBetween,
  edgeExists,
  isEmpty,
  thaw,

  predecessors,
  inEdges,

  edgeLabel,
  labeledEdges,
  labeledOutEdges,

  vertexLabel,
  labeledVertices,

  labeledInEdges,

  -- * Inductive graph operations
  emptyGraph,
  match,
  context,
  insertLabeledVertex,
  insertLabeledEdge,
  deleteEdge,
  deleteEdgesBetween,
  replaceLabeledEdge,
  deleteVertex,
  I.Context(..),

  -- * Classes

  -- | These classes are a critical implementation detail, but are
  -- re-exported to simplify writing type signatures for generic
  -- functions.
  I.MGraph,
  I.ImmutableGraph,
  I.MAddVertex,
  I.MAddEdge,
  I.MLabeledEdge,
  I.MEdgeLabel,
  I.MLabeledVertex,
  I.MVertexLabel,
  I.MRemovable,
  I.MBidirectional,
  I.Graph,
  I.Thawable,
  I.MutableGraph,
  I.Bidirectional,
  I.HasEdgeLabel,
  I.EdgeLabel,
  I.HasVertexLabel,
  I.VertexLabel,
  I.BidirectionalEdgeLabel,
  I.InductiveGraph
  ) where

import qualified Control.Monad.Primitive as P
import qualified Control.Monad.Ref as R

import qualified Data.Graph.Haggle.Classes as I
import qualified Data.Graph.Haggle.Digraph as D
import qualified Data.Graph.Haggle.BiDigraph as B
import qualified Data.Graph.Haggle.SimpleBiDigraph as SBD
import qualified Data.Graph.Haggle.PatriciaTree as PT

import qualified Data.Graph.Haggle.EdgeLabelAdapter as EA
import qualified Data.Graph.Haggle.VertexLabelAdapter as VA
import qualified Data.Graph.Haggle.LabelAdapter as A

-- Mutable graphs

getVertices :: (I.MGraph g, P.PrimMonad m, R.MonadRef m) => g m -> m [I.Vertex]
getVertices = I.getVertices
{-# INLINABLE getVertices #-}

getSuccessors :: (I.MGraph g, P.PrimMonad m, R.MonadRef m) => g m -> I.Vertex -> m [I.Vertex]
getSuccessors = I.getSuccessors
{-# INLINABLE getSuccessors #-}

getOutEdges :: (I.MGraph g, P.PrimMonad m, R.MonadRef m) => g m -> I.Vertex -> m [I.Edge]
getOutEdges = I.getOutEdges
{-# INLINABLE getOutEdges #-}

countVertices :: (I.MGraph g, P.PrimMonad m, R.MonadRef m) => g m -> m Int
countVertices = I.countVertices
{-# INLINABLE countVertices #-}

countEdges :: (I.MGraph g, P.PrimMonad m, R.MonadRef m) => g m -> m Int
countEdges = I.countEdges
{-# INLINABLE countEdges #-}

checkEdgeExists :: (I.MGraph g, P.PrimMonad m, R.MonadRef m) => g m -> I.Vertex -> I.Vertex -> m Bool
checkEdgeExists = I.checkEdgeExists
{-# INLINABLE checkEdgeExists #-}

freeze :: (I.MGraph g, P.PrimMonad m, R.MonadRef m) => g m -> m (I.ImmutableGraph g)
freeze = I.freeze
{-# INLINABLE freeze #-}

addVertex :: (I.MAddVertex g, P.PrimMonad m, R.MonadRef m) => g m -> m I.Vertex
addVertex = I.addVertex
{-# INLINABLE addVertex #-}

addEdge :: (I.MAddEdge g, P.PrimMonad m, R.MonadRef m) => g m -> I.Vertex -> I.Vertex -> m (Maybe I.Edge)
addEdge = I.addEdge
{-# INLINABLE addEdge #-}

getEdgeLabel :: (I.MLabeledEdge g, P.PrimMonad m, R.MonadRef m) => g m -> I.Edge -> m (Maybe (I.MEdgeLabel g))
getEdgeLabel = I.getEdgeLabel
{-# INLINABLE getEdgeLabel #-}

unsafeGetEdgeLabel :: (I.MLabeledEdge g, P.PrimMonad m, R.MonadRef m) => g m -> I.Edge -> m (I.MEdgeLabel g)
unsafeGetEdgeLabel = I.unsafeGetEdgeLabel
{-# INLINABLE unsafeGetEdgeLabel #-}

addLabeledEdge :: (I.MLabeledEdge g, P.PrimMonad m, R.MonadRef m) => g m -> I.Vertex -> I.Vertex -> I.MEdgeLabel g -> m (Maybe I.Edge)
addLabeledEdge = I.addLabeledEdge
{-# INLINABLE addLabeledEdge #-}

getVertexLabel :: (I.MLabeledVertex g, P.PrimMonad m, R.MonadRef m) => g m -> I.Vertex -> m (Maybe (I.MVertexLabel g))
getVertexLabel = I.getVertexLabel
{-# INLINABLE getVertexLabel #-}

addLabeledVertex :: (I.MLabeledVertex g, P.PrimMonad m, R.MonadRef m) => g m -> I.MVertexLabel g -> m I.Vertex
addLabeledVertex = I.addLabeledVertex
{-# INLINABLE addLabeledVertex #-}

getLabeledVertices :: (I.MLabeledVertex g, P.PrimMonad m, R.MonadRef m) => g m -> m [(I.Vertex, I.MVertexLabel g)]
getLabeledVertices = I.getLabeledVertices
{-# INLINABLE getLabeledVertices #-}

removeVertex :: (I.MRemovable g, P.PrimMonad m, R.MonadRef m) => g m -> I.Vertex -> m ()
removeVertex = I.removeVertex
{-# INLINABLE removeVertex #-}

removeEdgesBetween :: (I.MRemovable g, P.PrimMonad m, R.MonadRef m) => g m -> I.Vertex -> I.Vertex -> m ()
removeEdgesBetween = I.removeEdgesBetween
{-# INLINABLE removeEdgesBetween #-}

removeEdge :: (I.MRemovable g, P.PrimMonad m, R.MonadRef m) => g m -> I.Edge -> m ()
removeEdge = I.removeEdge
{-# INLINABLE removeEdge #-}

getPredecessors :: (I.MBidirectional g, P.PrimMonad m, R.MonadRef m) => g m -> I.Vertex -> m [I.Vertex]
getPredecessors = I.getPredecessors
{-# INLINABLE getPredecessors #-}

getInEdges :: (I.MBidirectional g, P.PrimMonad m, R.MonadRef m) => g m -> I.Vertex -> m [I.Edge]
getInEdges = I.getInEdges
{-# INLINABLE getInEdges #-}

-- Immutable graphs

vertices :: (I.Graph g) => g -> [I.Vertex]
vertices = I.vertices
{-# INLINABLE vertices #-}

edges :: (I.Graph g) => g -> [I.Edge]
edges = I.edges
{-# INLINABLE edges #-}

successors :: (I.Graph g) => g -> I.Vertex -> [I.Vertex]
successors = I.successors
{-# INLINABLE successors #-}

outEdges :: (I.Graph g) => g -> I.Vertex -> [I.Edge]
outEdges = I.outEdges
{-# INLINABLE outEdges #-}

edgesBetween :: (I.Graph g) => g -> I.Vertex -> I.Vertex -> [I.Edge]
edgesBetween = I.edgesBetween
{-# INLINABLE edgesBetween #-}

edgeExists :: (I.Graph g) => g -> I.Vertex -> I.Vertex -> Bool
edgeExists = I.edgeExists
{-# INLINABLE edgeExists #-}

isEmpty :: (I.Graph g) => g -> Bool
isEmpty = I.isEmpty
{-# INLINABLE isEmpty #-}

thaw :: (I.Thawable g, P.PrimMonad m, R.MonadRef m) => g -> m (I.MutableGraph g m)
thaw = I.thaw
{-# INLINABLE thaw #-}

predecessors :: (I.Bidirectional g) => g -> I.Vertex -> [I.Vertex]
predecessors = I.predecessors
{-# INLINABLE predecessors #-}

inEdges :: (I.Bidirectional g) => g -> I.Vertex -> [I.Edge]
inEdges = I.inEdges
{-# INLINABLE inEdges #-}

edgeLabel :: (I.HasEdgeLabel g) => g -> I.Edge -> Maybe (I.EdgeLabel g)
edgeLabel = I.edgeLabel
{-# INLINABLE edgeLabel #-}

labeledEdges :: (I.HasEdgeLabel g) => g -> [(I.Edge, I.EdgeLabel g)]
labeledEdges = I.labeledEdges
{-# INLINABLE labeledEdges #-}

labeledOutEdges :: (I.HasEdgeLabel g) => g -> I.Vertex -> [(I.Edge, I.EdgeLabel g)]
labeledOutEdges = I.labeledOutEdges
{-# INLINABLE labeledOutEdges #-}

labeledInEdges :: (I.BidirectionalEdgeLabel g) => g -> I.Vertex -> [(I.Edge, I.EdgeLabel g)]
labeledInEdges = I.labeledInEdges
{-# INLINABLE labeledInEdges #-}

vertexLabel :: (I.HasVertexLabel g) => g -> I.Vertex -> Maybe (I.VertexLabel g)
vertexLabel = I.vertexLabel
{-# INLINABLE vertexLabel #-}

labeledVertices :: (I.HasVertexLabel g) => g -> [(I.Vertex, I.VertexLabel g)]
labeledVertices = I.labeledVertices
{-# INLINABLE labeledVertices #-}

emptyGraph :: (I.InductiveGraph g, I.Graph g, I.HasEdgeLabel g, I.HasVertexLabel g) => g
emptyGraph = I.emptyGraph
{-# INLINABLE emptyGraph #-}

match :: (I.InductiveGraph g, I.Graph g, I.HasEdgeLabel g, I.HasVertexLabel g) => g -> I.Vertex -> Maybe (I.Context g, g)
match = I.match
{-# INLINABLE match #-}

context :: (I.InductiveGraph g, I.Graph g, I.HasEdgeLabel g, I.HasVertexLabel g) => g -> I.Vertex -> Maybe (I.Context g)
context = I.context
{-# INLINABLE context #-}

insertLabeledVertex :: (I.InductiveGraph g, I.Graph g, I.HasEdgeLabel g, I.HasVertexLabel g) => g -> I.VertexLabel g -> (I.Vertex, g)
insertLabeledVertex = I.insertLabeledVertex
{-# INLINABLE insertLabeledVertex #-}

insertLabeledEdge :: (I.InductiveGraph g, I.Graph g, I.HasEdgeLabel g, I.HasVertexLabel g) => g -> I.Vertex -> I.Vertex -> I.EdgeLabel g -> Maybe (I.Edge, g)
insertLabeledEdge = I.insertLabeledEdge
{-# INLINABLE insertLabeledEdge #-}

deleteEdge :: (I.InductiveGraph g, I.Graph g, I.HasEdgeLabel g, I.HasVertexLabel g) => g -> I.Edge -> g
deleteEdge = I.deleteEdge
{-# INLINABLE deleteEdge #-}

deleteEdgesBetween :: (I.InductiveGraph g, I.Graph g, I.HasEdgeLabel g, I.HasVertexLabel g) => g -> I.Vertex -> I.Vertex -> g
deleteEdgesBetween = I.deleteEdgesBetween
{-# INLINABLE deleteEdgesBetween #-}

replaceLabeledEdge :: (I.InductiveGraph g, I.Graph g, I.HasEdgeLabel g, I.HasVertexLabel g) => g -> I.Vertex -> I.Vertex -> I.EdgeLabel g -> Maybe (I.Edge, g)
replaceLabeledEdge = I.replaceLabeledEdge
{-# INLINABLE replaceLabeledEdge #-}

deleteVertex :: (I.InductiveGraph g, I.Graph g, I.HasEdgeLabel g, I.HasVertexLabel g) => g -> I.Vertex -> g
deleteVertex = I.deleteVertex
{-# INLINABLE deleteVertex #-}
