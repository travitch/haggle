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
import Data.Tree ( Tree )
import qualified Data.Tree as T

import Data.Graph.Haggle
import Data.Graph.Haggle.Internal.Basic
import Data.Graph.Haggle.Internal.BitSet

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

dfsWith :: (Graph g)
        => g
        -> (Vertex -> c)
        -> [Vertex]
        -> [c]
dfsWith g = xdfsWith g (successors g)

dfs :: (Graph g) => g -> [Vertex] -> [Vertex]
dfs g = dfsWith g id

rdfsWith :: (Bidirectional g)
         => g
         -> (Vertex -> c)
         -> [Vertex]
         -> [c]
rdfsWith g = xdfsWith g (predecessors g)

rdfs :: (Bidirectional g) => g -> [Vertex] -> [Vertex]
rdfs g = rdfsWith g id

udfsWith :: (Bidirectional g)
         => g
         -> (Vertex -> c)
         -> [Vertex]
         -> [c]
udfsWith g = xdfsWith g (neighbors g)

udfs :: (Bidirectional g) => g -> [Vertex] -> [Vertex]
udfs g = udfsWith g id

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

components :: (Bidirectional g) => g -> [[Vertex]]
components g = map preorder $ udff g (vertices g)

noComponents :: (Bidirectional g) => g -> Int
noComponents = length . components

isConnected :: (Bidirectional g) => g -> Bool
isConnected = (==1) . noComponents

-- | Topologically sort the graph; the input must be a DAG.
topsort :: (Graph g) => g -> [Vertex]
topsort g = reverse $ postflattenF $ dff g (vertices g)

scc :: (Bidirectional g) => g -> [[Vertex]]
scc g = map preorder (rdff g (topsort g))

reachable :: (Graph g) => Vertex -> g -> [Vertex]
reachable v g = preorderF (dff g [v])

-- Helpers

neighbors :: (Bidirectional g) => g -> Vertex -> [Vertex]
neighbors g v = successors g v ++ predecessors g v

preorder :: Tree a -> [a]
preorder = T.flatten

preorderF :: [Tree a] -> [a]
preorderF = concatMap preorder

postflatten :: Tree a -> [a]
postflatten (T.Node v ts) = postflattenF ts ++ [v]

postflattenF :: [Tree a] -> [a]
postflattenF = concatMap postflatten
