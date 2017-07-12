{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Graph.Haggle.Classes (
  -- * Basic Types
  Vertex,
  Edge,
  edgeSource,
  edgeDest,
  -- * Mutable Graphs
  MGraph(..),
  MAddEdge(..),
  MAddVertex(..),
  MRemovable(..),
  MBidirectional(..),
  MLabeledEdge(..),
  MLabeledVertex(..),
  -- * Immutable Graphs
  Graph(..),
  edgeExists,
  Thawable(..),
  Bidirectional(..),
  HasEdgeLabel(..),
  HasVertexLabel(..),
  BidirectionalEdgeLabel(..),
  -- * Inductive Graphs
  InductiveGraph(..),
  Context(..)
  ) where


import Control.Monad ( forM, liftM )
import qualified Control.Monad.Primitive as P
import qualified Control.Monad.Ref as R
import Data.Maybe ( fromMaybe )
import Data.Graph.Haggle.Internal.Basic

-- | The interface supported by a mutable graph.
class MGraph g where
  -- | The type generated by 'freeze'ing a mutable graph
  type ImmutableGraph g

  -- | List all of the vertices in the graph.
  getVertices :: (P.PrimMonad m, R.MonadRef m) => g m -> m [Vertex]

  -- | List the successors for the given 'Vertex'.
  getSuccessors :: (P.PrimMonad m, R.MonadRef m) => g m -> Vertex -> m [Vertex]

  -- | Get all of the 'Edge's with the given 'Vertex' as their source.
  getOutEdges :: (P.PrimMonad m, R.MonadRef m) => g m -> Vertex -> m [Edge]

  -- | Return the number of vertices in the graph
  countVertices :: (P.PrimMonad m, R.MonadRef m) => g m -> m Int

  -- | Return the number of edges in the graph
  countEdges :: (P.PrimMonad m, R.MonadRef m) => g m -> m Int

  -- | Edge existence test; this has a default implementation,
  -- but can be overridden if an implementation can support a
  -- better-than-linear version.
  checkEdgeExists :: (P.PrimMonad m, R.MonadRef m) => g m -> Vertex -> Vertex -> m Bool
  checkEdgeExists g src dst = do
    succs <- getSuccessors g src
    return $ any (==dst) succs

  -- | Freeze the mutable graph into an immutable graph.
  freeze :: (P.PrimMonad m, R.MonadRef m) => g m -> m (ImmutableGraph g)

class (MGraph g) => MAddVertex g where
  -- | Add a new 'Vertex' to the graph, returning its handle.
  addVertex :: (P.PrimMonad m, R.MonadRef m) => g m -> m Vertex

class (MGraph g) => MAddEdge g where
  -- | Add a new 'Edge' to the graph from @src@ to @dst@.  If either
  -- the source or destination is not in the graph, returns Nothing.
  -- Otherwise, the 'Edge' reference is returned.
  addEdge :: (P.PrimMonad m, R.MonadRef m) => g m -> Vertex -> Vertex -> m (Maybe Edge)

class (MGraph g) => MLabeledEdge g where
  type MEdgeLabel g
  getEdgeLabel :: (P.PrimMonad m, R.MonadRef m) => g m -> Edge -> m (Maybe (MEdgeLabel g))
  getEdgeLabel g e = do
    nEs <- countEdges g
    case edgeId e >= nEs of
      True -> return Nothing
      False -> liftM Just (unsafeGetEdgeLabel g e)
  unsafeGetEdgeLabel :: (P.PrimMonad m, R.MonadRef m) => g m -> Edge -> m (MEdgeLabel g)
  addLabeledEdge :: (P.PrimMonad m, R.MonadRef m) => g m -> Vertex -> Vertex -> MEdgeLabel g -> m (Maybe Edge)

class (MGraph g) => MLabeledVertex g where
  type MVertexLabel g
  getVertexLabel :: (P.PrimMonad m, R.MonadRef m) => g m -> Vertex -> m (Maybe (MVertexLabel g))
  addLabeledVertex :: (P.PrimMonad m, R.MonadRef m) => g m -> MVertexLabel g -> m Vertex
  getLabeledVertices :: (P.PrimMonad m, R.MonadRef m) => g m -> m [(Vertex, MVertexLabel g)]
  getLabeledVertices g = do
    vs <- getVertices g
    forM vs $ \v -> do
      Just l <- getVertexLabel g v
      return (v, l)

-- | An interface for graphs that allow vertex and edge removal.  Note that
-- implementations are not required to reclaim storage from removed
-- vertices (just make them inaccessible).
class (MGraph g) => MRemovable g where
  removeVertex :: (P.PrimMonad m, R.MonadRef m) => g m -> Vertex -> m ()
  removeEdgesBetween :: (P.PrimMonad m, R.MonadRef m) => g m -> Vertex -> Vertex -> m ()
  removeEdge :: (P.PrimMonad m, R.MonadRef m) => g m -> Edge -> m ()

-- | An interface for graphs that support looking at predecessor (incoming
-- edges) efficiently.
class (MGraph g) => MBidirectional g where
  getPredecessors :: (P.PrimMonad m, R.MonadRef m) => g m -> Vertex -> m [Vertex]
  getInEdges :: (P.PrimMonad m, R.MonadRef m) => g m -> Vertex -> m [Edge]

-- | The basic interface of immutable graphs.
class Graph g where
  vertices :: g -> [Vertex]
  edges :: g -> [Edge]
  successors :: g -> Vertex -> [Vertex]
  outEdges :: g -> Vertex -> [Edge]
  maxVertexId :: g -> Int
  isEmpty :: g -> Bool
  -- | This has a default implementation in terms of 'outEdges', but is part
  -- of the class so that instances can offer a more efficient implementation
  -- when possible.
  edgesBetween :: g -> Vertex -> Vertex -> [Edge]
  edgesBetween g src dst = filter ((dst ==) . edgeDest) (outEdges g src)

edgeExists :: Graph g => g -> Vertex -> Vertex -> Bool
edgeExists g v1 v2 = not . null $ edgesBetween g v1 v2

class (Graph g) => Thawable g where
  type MutableGraph g :: (* -> *) -> *
  thaw :: (P.PrimMonad m, R.MonadRef m) => g -> m (MutableGraph g m)

-- | The interface for immutable graphs with efficient access to
-- incoming edges.
class (Graph g) => Bidirectional g where
  predecessors :: g -> Vertex -> [Vertex]
  inEdges :: g -> Vertex -> [Edge]

-- | The interface for immutable graphs with labeled edges.
class (Graph g) => HasEdgeLabel g where
  type EdgeLabel g
  edgeLabel :: g -> Edge -> Maybe (EdgeLabel g)
  labeledEdges :: g -> [(Edge, EdgeLabel g)]
  labeledOutEdges :: g -> Vertex -> [(Edge, EdgeLabel g)]
  labeledOutEdges g v = map (addEdgeLabel g) (outEdges g v)


class (HasEdgeLabel g, Bidirectional g) => BidirectionalEdgeLabel g where
  labeledInEdges :: g -> Vertex -> [(Edge, EdgeLabel g)]
  labeledInEdges g v = map (addEdgeLabel g) (inEdges g v)

-- | The interface for immutable graphs with labeled vertices.
class (Graph g) => HasVertexLabel g where
  type VertexLabel g
  vertexLabel :: g -> Vertex -> Maybe (VertexLabel g)
  labeledVertices :: g -> [(Vertex, VertexLabel g)]

-- | Contexts represent the "context" of a 'Vertex', which includes the incoming edges of the 'Vertex',
-- the label of the 'Vertex', and the outgoing edges of the 'Vertex'.
data Context g = Context [(EdgeLabel g, Vertex)] (VertexLabel g) [(EdgeLabel g, Vertex)]

class (Graph g, HasEdgeLabel g, HasVertexLabel g) => InductiveGraph g where
  -- | The empty inductive graph
  emptyGraph :: g
  -- | The call
  --
  -- > let (c, g') = match g v
  --
  -- decomposes the graph into the 'Context' c of @v@ and the rest of
  -- the graph @g'@.
  match :: g -> Vertex -> Maybe (Context g, g)
  -- | Return the context of a 'Vertex'
  context :: g -> Vertex -> Maybe (Context g)
  -- | Insert a new labeled 'Vertex' into the graph.
  insertLabeledVertex :: g -> VertexLabel g -> (Vertex, g)
  -- | Must return 'Nothing' if either the source or destination 'Vertex' is not
  -- in the graph.  Also returns 'Nothing' if the edge already exists and the
  -- underlying graph does not support parallel edges.
  --
  -- Otherwise return the inserted 'Edge' and updated graph.
  insertLabeledEdge :: g -> Vertex -> Vertex -> EdgeLabel g -> Maybe (Edge, g)
  -- | Delete the given 'Edge'.  In a multigraph, this lets you remove
  -- a single parallel edge between two vertices.
  deleteEdge :: g -> Edge -> g
  -- | Delete all edges between a pair of vertices.
  deleteEdgesBetween :: g -> Vertex -> Vertex -> g

  -- | Like 'insertLabeledEdge', but overwrite any existing edges.  Equivalent
  -- to:
  --
  -- > let g' = deleteEdgesBetween g v1 v2
  -- > in insertLabeledEdge g v1 v2 lbl
  replaceLabeledEdge :: g -> Vertex -> Vertex -> EdgeLabel g -> Maybe (Edge, g)
  replaceLabeledEdge g src dst lbl =
    let g' = deleteEdgesBetween g src dst
    in insertLabeledEdge g' src dst lbl

  -- | Remove a 'Vertex' from the graph
  deleteVertex :: g -> Vertex -> g
  deleteVertex g v = fromMaybe g $ do
    (_, g') <- match g v
    return g'

addEdgeLabel :: (HasEdgeLabel g) => g -> Edge -> (Edge, EdgeLabel g)
addEdgeLabel g e = (e, el)
  where
   Just el = edgeLabel g e
