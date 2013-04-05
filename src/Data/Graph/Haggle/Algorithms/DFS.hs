module Data.Graph.Haggle.Algorithms.DFS (
  xdfsWith,
  dfsWith,
  dfs,
  rdfsWith,
  rdfs
  ) where

import Control.Monad ( foldM )
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
          foldM (go bs) (f v : acc) (nextVerts v)

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

{-
xdffWith :: (Graph g)
         => g
         -> (Vertex -> [Vertex])
         -> (Vertex -> c)
         -> [Vertex]
         -> [Tree c]
xdffWith g next f roots
  | isEmpty g || null roots = []
  | otherwise =
-}

