{-# LANGUAGE CPP #-}
-- | This module tests Haggle by comparing its results to those of FGL.
-- This assumes that FGL is reasonably correct.
--
-- The arbitrary instance for GraphPair generates a list of edges and
-- then constructs equivalent FGL and Haggle graphs.  The quickcheck
-- properties for each operation try to ensure that the two implementations
-- return the same results.
module Main ( main ) where

import Test.Framework ( defaultMain, Test )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.Framework.Providers.HUnit ( hUnitTestToTests )
import Test.HUnit
import Test.QuickCheck

import Control.Arrow ( first, second )
import qualified Data.Bifunctor as Bi
import Control.Monad ( replicateM )
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Maybe ( fromJust, isNothing )
import qualified Data.Set as S
#if MIN_VERSION_base(4, 11, 0)
#else
import           Data.Monoid ( (<>) )
#endif

import qualified Data.Graph.Inductive as FGL
import qualified Data.Graph.Haggle as HGL
import qualified Data.Graph.Haggle.VertexLabelAdapter as HGL
import qualified Data.Graph.Haggle.SimpleBiDigraph as HGL
import qualified Data.Graph.Haggle.Algorithms.DFS as HGL
import qualified Data.Graph.Haggle.Algorithms.Dominators as HGL

-- import Debug.Trace
-- debug = flip trace

type BaseGraph = FGL.Gr Int ()
type TestGraph = HGL.VertexLabeledGraph HGL.SimpleBiDigraph Int

data GraphPair = GP [(Int, Int)] BaseGraph TestGraph

instance Arbitrary GraphPair where
  arbitrary = sized mkGraphPair

instance Show GraphPair where
  show (GP es _ _) = show es

newtype NodeId = NID Int
  deriving (Show)
instance Arbitrary NodeId where
  arbitrary = sized mkNodeId
    where
      mkNodeId n = do
        i <- choose (0, n)
        return (NID i)

mkGraphPair :: Int -> Gen GraphPair
mkGraphPair sz = do
  nEdges <- choose (2, 2 * sz)
  srcs <- replicateM nEdges (choose (0, sz))
  dsts <- replicateM nEdges (choose (0, sz))
  let edges = unique $ zip srcs dsts
      nids = unique (srcs ++ dsts)
      ns = zip nids nids
      bg = FGL.mkGraph ns (map (\(s, d) -> (s, d, ())) edges)
      (tg, _) = HGL.fromEdgeList HGL.newMSimpleBiDigraph edges
  return $! GP edges bg tg

main :: IO ()
main = defaultMain tests

tests :: [Test.Framework.Test]
tests = [ testProperty "prop_sameVertexCount" prop_sameVertexCount
        , testProperty "prop_sameEdgeCount" prop_sameEdgeCount
        , testProperty "prop_sameSuccessorsAtLabel" prop_sameSuccessorsAtLabel
        , testProperty "prop_samePredecessorsAtLabel" prop_samePredecessorsAtLabel
        , testProperty "prop_dfsSame" prop_dfsSame
        , testProperty "prop_sameComponents" prop_sameComponents
        , testProperty "prop_sameNoComponents" prop_sameNoComponents
        , testProperty "prop_immDominatorsSame" prop_immDominatorsSame
        , testProperty "prop_dominatorsSame" prop_dominatorsSame
        ] <>  testPatricia

prop_sameVertexCount :: GraphPair -> Bool
prop_sameVertexCount (GP _ bg tg) =
  length (FGL.nodes bg) == length (HGL.vertices tg)

prop_sameEdgeCount :: GraphPair -> Bool
prop_sameEdgeCount (GP _ bg tg) =
  length (FGL.edges bg) == length (HGL.edges tg)

prop_sameSuccessorsAtLabel :: (NodeId, GraphPair) -> Bool
prop_sameSuccessorsAtLabel (NID nid, GP _ bg tg)
  | not (FGL.gelem nid bg) && isNothing (vertexFromLabel tg nid) = True
  | otherwise = bss == tss
  where
    bss = S.fromList $ fmap Just $ FGL.suc bg nid
    ts = maybe [] (map (HGL.vertexLabel tg) . HGL.successors tg) (vertexFromLabel tg nid)
    tss = S.fromList ts

prop_samePredecessorsAtLabel :: (NodeId, GraphPair) -> Bool
prop_samePredecessorsAtLabel (NID nid, GP _ bg tg)
  | not (FGL.gelem nid bg) && isNothing (vertexFromLabel tg nid) = True
  | otherwise = bss == tss
  where
    bss = S.fromList $ fmap Just $ FGL.pre bg nid
    ts = maybe [] (map (HGL.vertexLabel tg) . HGL.predecessors tg) (vertexFromLabel tg nid)
    tss = S.fromList ts

-- Note that this is only checking the *set* of vertices reached.  Unfortunately,
-- verifying the *order* is difficult because there are many valid DFS orders
-- (depending on the order edges are stored).  A test using the DFS number
-- (derived from the depth in the depth-first tree) would be a good complement
-- to this.
prop_dfsSame :: (NodeId, GraphPair) -> Bool
prop_dfsSame (NID root, GP _ bg tg) =
  S.fromList bres == S.fromList tres
  where
    bres = map Just $ FGL.dfs [root] bg
    v = vertexFromLabel tg root
    tres = maybe [] (map (HGL.vertexLabel tg) . HGL.dfs tg . (:[])) v

prop_immDominatorsSame :: (NodeId, GraphPair) -> Bool
prop_immDominatorsSame (NID root, GP _ bg tg)
  | not (FGL.gelem root bg) && isNothing (vertexFromLabel tg root) = True
  | otherwise = S.fromList bdoms == S.fromList tdoms
  where
    bdoms = FGL.iDom bg root
    toLabs (v1, v2) =
      let Just v1l = HGL.vertexLabel tg v1
          Just v2l = HGL.vertexLabel tg v2
      in (v1l, v2l)
    tdoms = maybe [] (map toLabs . HGL.immediateDominators tg) (vertexFromLabel tg root)

prop_dominatorsSame :: (NodeId, GraphPair) -> Bool
prop_dominatorsSame (NID root, GP _ bg tg)
  | not (FGL.gelem root bg) && isNothing (vertexFromLabel tg root) = True
  | otherwise = S.fromList (map (first Just) bdoms) == S.fromList (map (first (HGL.vertexLabel tg)) tdoms)
  where
    bdoms = map (second (S.fromList . map Just)) $ FGL.dom bg root
    Just rv = vertexFromLabel tg root
    tdoms = map (second (S.fromList . map (HGL.vertexLabel tg))) $ HGL.dominators tg rv

prop_sameComponents :: GraphPair -> Bool
prop_sameComponents (GP _ bg tg) = bcs == tcs
  where
    bcs = S.map (S.fromList . map Just) $ S.fromList $ FGL.components bg
    tcs = S.map (S.fromList . map (HGL.vertexLabel tg)) $ S.fromList $ HGL.components tg

prop_sameNoComponents :: GraphPair -> Bool
prop_sameNoComponents (GP _ bg tg) =
  FGL.noComponents bg == HGL.noComponents tg

-- Helpers

vertexFromLabel :: TestGraph -> Int -> Maybe HGL.Vertex
vertexFromLabel g lbl = F.find labelMatch (HGL.vertices g)
  where
    labelMatch v = Just lbl == (HGL.vertexLabel g v)

unique :: (Ord a) => [a] -> [a]
unique = S.toList . S.fromList

----------------------------------------------------------------------

-- Explicit tests for various functionality

testPatricia :: [Test.Framework.Test]
testPatricia =
  let gr0 = foldl (\g -> snd . HGL.insertLabeledVertex g)
                 (HGL.emptyGraph :: HGL.PatriciaTree Int Char)
                 [1,2,4,3,5,0]
      vs = fst <$> HGL.labeledVertices gr0
      gr1 = foldl (\g (f,t,l) ->
                     snd $ fromJust $ HGL.insertLabeledEdge g f t l)
            gr0
            [ (vs !! 1, vs !! 2, 'a')
            , (vs !! 0, vs !! 2, 'b')
            , (vs !! 1, vs !! 5, 'c')
            ]
  in hUnitTestToTests $ test
     [ "create graph" ~:
       do sum (snd <$> HGL.labeledVertices gr1) @?= 15
          L.sort (snd <$> HGL.labeledEdges gr1) @?= "abc"

     , "bifunctor first (nodes)" ~:
       do let gr2 = Bi.first (+3) gr1
          sum (snd <$> HGL.labeledVertices gr2) @?= 33
          L.sort (snd <$> HGL.labeledEdges gr2) @?= "abc"

     , "bifunctor second (edges)" ~:
       do let gr2 = Bi.second (succ . succ . succ) gr1
          sum (snd <$> HGL.labeledVertices gr2) @?= 15
          L.sort (snd <$> HGL.labeledEdges gr2) @?= "def"

     , "bifunctor bimap" ~:
       do let gr2 = Bi.bimap (+2) (succ . succ) gr1
          sum (snd <$> HGL.labeledVertices gr2) @?= 27
          L.sort (snd <$> HGL.labeledEdges gr2) @?= "cde"
     ]
