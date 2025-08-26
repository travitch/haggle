{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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
import Data.Function ( on )
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

-- | Generates a pair of a Haggle graph and the corresponding FGL graph to serve
-- as an oracle.
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

        -- prop_dominatorsSame requires fgl 5.8.1.1, which fixes errors in fgl's
        -- dom functionality that is used as the oracle for the tests here.
        , testProperty "prop_dominatorsSame" prop_dominatorsSame

        , testProperty "patricia match: remaining vertices" prop_match_patricia_remvertices
        , testProperty "patricia match: vertex label removed" prop_match_patricia_vlblremoved
        , testProperty "patricia match: disconnected to" prop_match_patricia_no_in_edges
        , testProperty "patricia match: disconnected from" prop_match_patricia_no_out_edges
        , testProperty "patricia match: edges removed" prop_match_patricia_remedges
        ] <>  testPatricia
        <> testExplicit

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

testExplicit :: [Test.Framework.Test]
testExplicit =
  let gr0 = foldl (\g -> snd . HGL.insertLabeledVertex g)
            (HGL.emptyGraph :: HGL.PatriciaTree Int Char)
            [1,2,4]
      vs = fst <$> HGL.labeledVertices gr0

      plusEdge g f t = snd $ fromJust $ HGL.insertLabeledEdge g f t 'a'

      gr1 = plusEdge gr0 (vs !! 0) (vs !! 1)
      -- gr1 has three nodes, two are connected, one is not connected (i.e. two
      -- independent subgraphs)

      gr2 = plusEdge (plusEdge gr0 (vs !! 1) (vs !! 0)) (vs !! 1) (vs !! 1)


  in hUnitTestToTests $ test
     [ "haggle (patricia) [1-2,4] reachable from 1" ~:
       do HGL.reachable (vs !! 0) gr1 @?= [ (vs !! 0)
                                          , (vs !! 1)
                                          ]
     , "haggle (patricia) [1-2,4] reachable from 2" ~:
       do HGL.reachable (vs !! 1) gr1 @?= [ (vs !! 1)
                                          ]
     , "haggle (patricia) [2-1,2-2] reachable from 1" ~:
       do HGL.reachable (vs !! 0) gr2 @?= [ (vs !! 0)
                                          ]
     , "no cycle in [1-2,4] from 1" ~:
       do HGL.hasCycle (vs !! 0) gr1 @?= False

     , "cycle in [1-2-1,4] from 1" ~:
       do let tg = plusEdge gr1 (vs !! 1) (vs !! 0)
          HGL.hasCycle (vs !! 0) tg @?= True

     , "cycle in [1*-2,4] from 1" ~:
       do let tg = plusEdge gr1 (vs !! 0) (vs !! 0)
          HGL.hasCycle (vs !! 0) tg @?= True

     , "haggle dominator [1-2,4] from 1" ~:
       do HGL.dominators gr1 (vs !! 0) @?= [ (vs !! 0, [ (vs !! 0) ])
                                           , (vs !! 1, [ (vs !! 1), (vs !! 0) ])
                                           ]

     , "haggle dominator [2-1,2-2,4] from 1" ~:
       do HGL.dominators gr2 (vs !! 0) @?= [ (vs !! 0, [ (vs !! 0) ])
                                           ]

     , "haggle add self-edge" ~:
       do let Just (e,g) = HGL.insertLabeledEdge gr0 (vs!!0) (vs!!0) 's'
          HGL.edges g @?= [e]

     , "haggle delete self-edge" ~:
       do let Just (_,g) = HGL.insertLabeledEdge gr0 (vs!!0) (vs!!0) 's'
          HGL.edges (HGL.deleteEdgesBetween g (vs!!0) (vs!!0)) @?= []


     -- n.b. fgl's dominator is broken (as haggle's original version also was) in
     -- that its return includes (4, [1,2,4]), which is invalid: 4 is in an
     -- independent subgraph and cannot be dominated by 1.
     -- , "fgl dominator for [1-2,4] from 1" ~:
     --   do let fgr0 = FGL.mkGraph [(1,1), (2,2), (4,4)] [(0, 1, 'f')] :: FGL.Gr Int Char
     --      FGL.dom fgr0 1 @?= [ (1, [ 1 ])
     --                         , (2, [ 1, 2 ])
     --                         ]

     -- n.b. fgl's dominator is also broken in regards to reachability.  For a
     -- dom return for [2-1, 2-2] from 1 also returns (2, [1,2]) which is
     -- invalid, because 2 is not reachable from 1 and so 1 cannot be a dominator
     -- for 2
     -- , "fgl dominator for [2-1,2-2] from 1" ~:
     --   do let fgr2 = FGL.mkGraph [(1,1), (2,2)] [(2,1,'f'), (2,2,'s')] :: FGL.Gr Int Char
     --      FGL.dom fgr2 1 @?= [ (1, [ 1 ]) ]
     ]

testPatricia :: [Test.Framework.Test]
testPatricia =
  let gr0 = foldl (\g -> snd . HGL.insertLabeledVertex g)
            (HGL.emptyGraph :: HGL.PatriciaTree Int Char)
            [1,2,4,3,5,0]
      vs = fst <$> (L.sortBy (compare `on` snd) $ HGL.labeledVertices gr0)
      gr1 = foldl (\g (f,t,l) ->
                     snd $ fromJust $ HGL.insertLabeledEdge g f t l)
            gr0
            [ (vs !! 2, vs !! 4, 'a')
            , (vs !! 1, vs !! 4, 'b')
            , (vs !! 2, vs !! 0, 'c')
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

     , "replaceLabeledVertex" ~:
       do let gr2 = HGL.replaceLabeledVertex gr1 (vs !! 4) 11
          -- Vertex label changed?
          sum (snd <$> HGL.labeledVertices gr2) @?= (15 + (11 - 4))
          -- Edges are still in place?
          L.sort (snd <$> HGL.labeledEdges gr2) @?= "abc"
     ]


----------------------------------------------------------------------


newtype NodeLabel = NL Int deriving (Eq, Show)
newtype EdgeLabel = EL Int deriving (Eq, Show)

-- type InductiveGraphBuilder g = (g NodeLabel EdgeLabel -> g NodeLabel EdgeLabel)
data InductiveGraphBuilder g =
  IGB { build :: g NodeLabel EdgeLabel -> g NodeLabel EdgeLabel }

instance ( HGL.InductiveGraph (g NodeLabel EdgeLabel)
         , HGL.HasVertexLabel (g NodeLabel EdgeLabel)
         , HGL.HasEdgeLabel (g NodeLabel EdgeLabel)
         , HGL.VertexLabel (g NodeLabel EdgeLabel) ~ NodeLabel
         , HGL.EdgeLabel (g NodeLabel EdgeLabel) ~ EdgeLabel
         ) => Arbitrary (InductiveGraphBuilder g) where
  arbitrary = oneof [ solitaryNode
                    , edgeToNewNode
                    , edgeBetweenExistingNodes
                    , edgeToSelf
                    ]
    where solitaryNode = return $ IGB $ \g ->
            let vLabel = NL $ length $ HGL.vertices g
            in snd $ HGL.insertLabeledVertex g vLabel
          edgeToNewNode = do
            srcNum <- choose (0, 1024)
            return $ IGB $ \g ->
              let vs = HGL.vertices g
                  srcV = cycle vs !! srcNum
                  vLabel = NL $ length $ vs
                  eLabel = EL $ length $ HGL.edges g
                  (nv, ng) = HGL.insertLabeledVertex g vLabel
              in if null vs
                 then ng
                 else maybe g snd $ HGL.insertLabeledEdge ng srcV nv eLabel
          edgeBetweenExistingNodes = do
            srcNum <- choose (0, 1024)
            dstNum <- choose (0, 1024)
            -- n.b. the inductive graphs don't like duplicated edges, but they
            -- will just return Nothing on inserting the edge, which returns the
            -- existing graph, so this duplication attempt is quietly ignored.
            return $ IGB $ \g ->
              let vs = HGL.vertices g
                  srcV = cycle (HGL.vertices g) !! srcNum
                  dstV = cycle (HGL.vertices g) !! dstNum
                  eLabel = EL $ length $ HGL.edges g
              in if null vs
                 then snd $ HGL.insertLabeledVertex g $ NL 0
                 else maybe g snd $ HGL.insertLabeledEdge g srcV dstV eLabel
          edgeToSelf = do
            srcNum <- choose (0, 1024)
            -- see note above re: duplicate edges
            return $ IGB $ \g ->
              let vs = HGL.vertices g
                  srcV = cycle (HGL.vertices g) !! srcNum
                  eLabel = EL $ length $ HGL.edges g
              in if null vs
                 then snd $ HGL.insertLabeledVertex g $ NL 0
                 else maybe g snd $ HGL.insertLabeledEdge g srcV srcV eLabel


type InductiveProperty g = InductiveCase g -> Bool

data InductiveCase g = IGC g HGL.Vertex deriving Show

instance (Arbitrary g, HGL.Graph g) => Arbitrary (InductiveCase g) where
  arbitrary = do g <- arbitrary
                 v <- elements $ HGL.vertices g
                 return $ IGC g v

onMatchResult :: HGL.InductiveGraph g
              => HGL.Graph g
              => (g -> HGL.Vertex -> (HGL.Context g, g) -> Bool)
              -> InductiveProperty g
onMatchResult prop (IGC g v) =
  case HGL.match g v of
    Nothing -> False
    Just mr -> prop g v mr

prop_match_inductive_remvertices :: HGL.InductiveGraph g => InductiveProperty g
prop_match_inductive_remvertices = onMatchResult $ \g -> \_ -> \(_ctxt, g') ->
  length (HGL.vertices g) == length (HGL.vertices g') + 1

prop_match_inductive_vlblremoved :: HGL.InductiveGraph g
                                 => Eq (HGL.VertexLabel g)
                                 => InductiveProperty g
prop_match_inductive_vlblremoved = onMatchResult $ \_ -> \v -> \(ctxt, g') ->
  let HGL.Context _ vl _ = ctxt
  in not $ (v,vl) `elem` HGL.labeledVertices g'

prop_match_inductive_no_in_edges :: HGL.InductiveGraph g
                                 => InductiveProperty g
prop_match_inductive_no_in_edges = onMatchResult $ \_ -> \v -> \(ctxt, g') ->
  let HGL.Context intos _ _ = ctxt
      edgeInTo (_,sv) = v /= sv && v `elem` HGL.successors g' sv
  in not $ any edgeInTo intos

prop_match_inductive_no_out_edges :: HGL.InductiveGraph g
                                  => HGL.Bidirectional g
        => Show g
                                  => InductiveProperty g
prop_match_inductive_no_out_edges = onMatchResult $ \_ -> \v -> \(ctxt, g') ->
  let HGL.Context _ _ outs = ctxt
      edgeOutTo (_,dv) = v /= dv && v `elem` HGL.predecessors g' dv
  in not $ any edgeOutTo outs

prop_match_inductive_remedges :: HGL.InductiveGraph g
                              => HGL.HasEdgeLabel g
                              => Eq (HGL.EdgeLabel g)
                              => InductiveProperty g
prop_match_inductive_remedges = onMatchResult $ \_ -> \_ -> \(ctxt, g') ->
  let HGL.Context intos _ outs = ctxt
      remainingEdgeLabels = snd <$> HGL.labeledEdges g'
      hasEdge (el,_) = el `elem` remainingEdgeLabels
  in not $ any hasEdge $ intos <> outs

--------------------

type PatriciaProperty = InductiveProperty (HGL.PatriciaTree NodeLabel EdgeLabel)

instance Arbitrary (HGL.PatriciaTree NodeLabel EdgeLabel) where
  arbitrary = do mkGraph <- listOf1 arbitrary
                 return $ foldr build HGL.emptyGraph mkGraph

instance Show (HGL.PatriciaTree NodeLabel EdgeLabel) where
  show g = "PatriciaTree/" <> show (length $ HGL.vertices g)
           <> "/" <> show (length $ HGL.edges g)

prop_match_patricia_remvertices :: PatriciaProperty
prop_match_patricia_remvertices = prop_match_inductive_remvertices

prop_match_patricia_vlblremoved :: PatriciaProperty
prop_match_patricia_vlblremoved = prop_match_inductive_vlblremoved

prop_match_patricia_no_in_edges :: PatriciaProperty
prop_match_patricia_no_in_edges = prop_match_inductive_no_in_edges

prop_match_patricia_no_out_edges :: PatriciaProperty
prop_match_patricia_no_out_edges = prop_match_inductive_no_out_edges

prop_match_patricia_remedges :: PatriciaProperty
prop_match_patricia_remedges = prop_match_inductive_remedges
