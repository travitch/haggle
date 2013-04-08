module Data.Graph.Haggle.Algorithms.Dominators (
  immediateDominators,
  dominators
  ) where

import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Tree ( Tree(..) )
import qualified Data.Tree as T
import Data.Vector ( Vector, (!) )
import qualified Data.Vector as V

import Data.Graph.Haggle
import Data.Graph.Haggle.Algorithms.DFS

type ToNode = Vector Vertex
type FromNode = Map Vertex Int
type IDom = Vector Int
type Preds = Vector [Int]

immediateDominators :: (Graph g) => g -> Vertex -> [(Vertex, Vertex)]
immediateDominators g root = fromMaybe [] $ do
  (res, toNode, _) <- domWork g root
  return $ tail $ V.toList $ V.imap (\i n -> (toNode!i, toNode!n)) res

dominators :: (Graph g) => g -> Vertex -> [(Vertex, [Vertex])]
dominators g root = fromMaybe [] $ do
  (res, toNode, fromNode) <- domWork g root
  let dom' = getDom toNode res
      rest = M.keys (M.filter (-1 ==) fromNode)
      verts = vertices g
  return $ [(toNode ! i, dom' ! i) | i <- [0..V.length dom' - 1]] ++
           [(n, verts) | n <- rest]

domWork :: (Graph g) => g -> Vertex -> Maybe (IDom, ToNode, FromNode)
domWork g root
  | null trees = Nothing
  | otherwise = return (idom, toNode, fromNode)
  where
    -- Build up a depth-first tree from the root as a first approximation
    trees@(~[tree]) = dff g [root]
    (s, ntree) = numberTree 0 tree
    -- Start with an approximation (idom0) where the idom of each node is
    -- its parent in the depth-first tree.  Note that index 0 is the root,
    -- which we will basically be ignoring (since it has no dominator).
    dom0Map = M.fromList (treeEdges (-1) ntree)
    idom0 = V.generate (M.size dom0Map) (dom0Map M.!)
    -- Build a mapping from graph vertices to internal indices.  @treeNodes@
    -- are nodes that are in the depth-first tree from the root.  @otherNodes@
    -- are the rest of the nodes in the graph, mapped to -1 (since they aren't
    -- going to be in the result)
    treeNodes = M.fromList $ zip (T.flatten tree) (T.flatten ntree)
    otherNodes = M.fromList $ zip (vertices g) (repeat (-1))
    fromNode = M.unionWith const treeNodes otherNodes
    -- Translate from internal nodes back to graph nodes (only need the nodes
    -- in the depth-first tree)
    toNodeMap = M.fromList $ zip (T.flatten ntree) (T.flatten tree)
    toNode = V.generate (M.size toNodeMap) (toNodeMap M.!)

    -- Use a pre-pass over the graph to collect predecessors so that we don't
    -- require a Bidirectional graph.  We need a linear pass over the graph
    -- here anyway, so we don't lose anything.
    predMap = fmap S.toList $ foldr (toPredecessor g) M.empty (vertices g)
    preds = V.fromList $ [0] : [filter (/= -1) (map (fromNode M.!) (predMap M.! (toNode ! i)))
                               | i <- [1..s-1]]
    idom = fixEq (refineIDom preds) idom0

toPredecessor :: (Graph g)
              => g
              -> Vertex
              -> Map Vertex (Set Vertex)
              -> Map Vertex (Set Vertex)
toPredecessor g pre m = foldr addPred m (successors g pre)
  where
    addPred suc = M.insertWith S.union suc (S.singleton pre)

refineIDom :: Preds -> IDom -> IDom
refineIDom preds idom = fmap (foldl1 (intersect idom)) preds

intersect :: IDom -> Int -> Int -> Int
intersect idom a b =
  case a `compare` b of
    LT -> intersect idom a (idom ! b)
    EQ -> a
    GT -> intersect idom (idom ! a) b

-- Helpers

getDom :: ToNode -> IDom -> Vector [Vertex]
getDom toNode idom = res
  where
    -- The root dominates itself (the only dominator for the root)
    root = [toNode ! 0]
    res = V.fromList $ root : [toNode ! i : res ! (idom ! i) | i <- [1..V.length idom - 1]]

treeEdges :: a -> Tree a -> [(a,a)]
treeEdges a (Node b ts) = (b,a) : concatMap (treeEdges b) ts

-- relabel tree, labeling vertices with consecutive numbers in depth first order
numberTree :: Int -> Tree a -> (Int, Tree Int)
numberTree n (Node _ ts) = let (n', ts') = numberForest (n+1) ts
                           in  (n', Node n ts')

-- same as numberTree, for forests.
numberForest :: Int -> [Tree a] -> (Int, [Tree Int])
numberForest n []     = (n, [])
numberForest n (t:ts) = let (n', t')   = numberTree n t
                            (n'', ts') = numberForest n' ts
                        in  (n'', t':ts')

fixEq :: Eq a => (a -> a) -> a -> a
fixEq f v
  | v' == v   = v
  | otherwise = fixEq f v'
  where
    v' = f v
