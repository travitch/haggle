-- | Depth-first search and derived operations.
--
-- All of the search variants take a list of 'Vertex' that serves as
-- roots for the search.
--
-- The [x] variants ('xdfsWith' and 'xdffWith') are the most general
-- and are fully configurable in direction and action.  They take a
-- \"direction\" function that tells the search what vertices are
-- next from the current 'Vertex'.  They also take a summarization function
-- to convert a 'Vertex' into some other value.  This could be 'id' or a
-- function to extract a label, if supported by your graph type.
--
-- The [r] variants are reverse searches, while the [u] variants are
-- undirected.
--
-- A depth-first forest is a collection (list) of depth-first trees.  A
-- depth-first tree is an n-ary tree rooted at a vertex that contains
-- the vertices reached in a depth-first search from that root.  The
-- edges in the tree are a subset of the edges in the graph.
module Data.Graph.Haggle.Algorithms.DFS (
  -- * Depth-first Searches
  xdfsWith,
  dfsWith,
  dfs,
  rdfsWith,
  rdfs,
  udfsWith,
  udfs,
  -- * Depth-first Forests
  xdffWith,
  dffWith,
  dff,
  rdffWith,
  rdff,
  udffWith,
  udff,
  -- * Derived Queries
  components,
  noComponents,
  isConnected,
  topsort,
  scc,
  reachable
  ) where

import Control.Monad ( filterM, foldM, liftM )
import Control.Monad.ST
import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.Sequence as Seq
import Data.Tree ( Tree )
import qualified Data.Tree as T

import Prelude

import Data.Graph.Haggle
import Data.Graph.Haggle.Classes ( maxVertexId )
import Data.Graph.Haggle.Internal.BitSet

-- | The most general DFS
xdfsWith :: (Graph g)
         => g
         -> (Vertex -> [Vertex])
         -> (Vertex -> c)
         -> [Vertex]
         -> [c]
xdfsWith g nextVerts f roots
  | isEmpty g || null roots = []
  | otherwise = runST $ do
    bs <- newBitSet (maxVertexId g + 1)
    res <- foldM (go bs) [] roots
    return $ reverse res
  where
    go bs acc v = do
      isMarked <- testBit bs (vertexId v)
      case isMarked of
        True -> return acc
        False -> do
          setBit bs (vertexId v)
          nxt <- filterM (notVisited bs) (nextVerts v)
          foldM (go bs) (f v : acc) nxt

notVisited :: BitSet s -> Vertex -> ST s Bool
notVisited bs v = liftM not (testBit bs (vertexId v))

-- | Forward parameterized DFS
dfsWith :: (Graph g)
        => g
        -> (Vertex -> c)
        -> [Vertex]
        -> [c]
dfsWith g = xdfsWith g (successors g)

-- | Forward DFS
dfs :: (Graph g) => g -> [Vertex] -> [Vertex]
dfs g = dfsWith g id

-- | Reverse parameterized DFS
rdfsWith :: (Bidirectional g)
         => g
         -> (Vertex -> c)
         -> [Vertex]
         -> [c]
rdfsWith g = xdfsWith g (predecessors g)

-- | Reverse DFS
rdfs :: (Bidirectional g) => g -> [Vertex] -> [Vertex]
rdfs g = rdfsWith g id

-- | Undirected parameterized DFS.  This variant follows both
-- incoming and outgoing edges from each 'Vertex'.
udfsWith :: (Bidirectional g)
         => g
         -> (Vertex -> c)
         -> [Vertex]
         -> [c]
udfsWith g = xdfsWith g (neighbors g)

-- | Undirected DFS
udfs :: (Bidirectional g) => g -> [Vertex] -> [Vertex]
udfs g = udfsWith g id

-- | The most general depth-first forest.
xdffWith :: (Graph g)
         => g
         -> (Vertex -> [Vertex])
         -> (Vertex -> c)
         -> [Vertex]
         -> [Tree c]
xdffWith g nextVerts f roots
  | isEmpty g || null roots = []
  | otherwise = runST $ do
    bs <- newBitSet (maxVertexId g + 1)
    res <- foldM (go bs) [] roots
    return $ reverse res
  where
    go bs acc v = do
      isMarked <- testBit bs (vertexId v)
      case isMarked of
        True -> return acc
        False -> do
          setBit bs (vertexId v)
          nxt <- filterM (notVisited bs) (nextVerts v)
          ts <- foldM (go bs) [] nxt
          return $ T.Node (f v) (reverse ts) : acc

dffWith :: (Graph g)
        => g
        -> (Vertex -> c)
        -> [Vertex]
        -> [Tree c]
dffWith g = xdffWith g (successors g)

dff :: (Graph g) => g -> [Vertex] -> [Tree Vertex]
dff g = dffWith g id

rdffWith :: (Bidirectional g) => g -> (Vertex -> c) -> [Vertex] -> [Tree c]
rdffWith g = xdffWith g (predecessors g)

rdff :: (Bidirectional g) => g -> [Vertex] -> [Tree Vertex]
rdff g = rdffWith g id

udffWith :: (Bidirectional g) => g -> (Vertex -> c) -> [Vertex] -> [Tree c]
udffWith g = xdffWith g (neighbors g)

udff :: (Bidirectional g) => g -> [Vertex] -> [Tree Vertex]
udff g = udffWith g id

-- Derived

-- | Return a list of each connected component in the graph
components :: (Bidirectional g) => g -> [[Vertex]]
components g = map preorder $ udff g (vertices g)

-- | The number of components in the graph
noComponents :: (Bidirectional g) => g -> Int
noComponents = length . components

-- | True if there is only a single component in the graph.
isConnected :: (Bidirectional g) => g -> Bool
isConnected = (==1) . noComponents

-- | Topologically sort the graph; the input must be a DAG.
topsort :: (Graph g) => g -> [Vertex]
topsort g = reverse $ F.toList $ postflattenF $ dff g (vertices g)

-- | Return a list of each /strongly-connected component/ in the graph.
-- In a strongly-connected component, every vertex is reachable from every
-- other vertex.
scc :: (Bidirectional g) => g -> [[Vertex]]
scc g = map preorder (rdff g (topsort g))

-- | Compute the set of vertices reachable from a root 'Vertex'.
reachable :: (Graph g) => Vertex -> g -> [Vertex]
reachable v g = preorderF (dff g [v])

-- Helpers

neighbors :: (Bidirectional g) => g -> Vertex -> [Vertex]
neighbors g v = successors g v ++ predecessors g v

preorder :: Tree a -> [a]
preorder = T.flatten

preorderF :: [Tree a] -> [a]
preorderF = concatMap preorder

postflatten :: Tree a -> Seq.Seq a
postflatten (T.Node v ts) = postflattenF ts <> Seq.singleton v

postflattenF :: [Tree a] -> Seq.Seq a
postflattenF = F.foldMap postflatten
