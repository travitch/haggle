{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main ( main ) where

import           Criterion.Main

import           Control.DeepSeq
import           Data.Bifunctor ( bimap )
import qualified Data.Foldable as F
import           Data.Maybe ( fromJust )
import qualified Data.Set as S

import qualified Data.Graph.Inductive as FGL
import qualified Data.Graph.Haggle as HGL
import qualified Data.Graph.Haggle.Algorithms.DFS as HGL


-- | Generates a list of nodes and edges for a sample graph of the specified
-- depth (height).  The root node will be 0, and every node will have "width"
-- children down to the desired depth.  There are also some extra cross-level
-- edges added to keep things from being too regular.
mkEdges :: Int -> Int -> ([Int], [(Int,Int)])
mkEdges width depth = bimap unique unique $ go [0] 0
  where go cur l =
          if l >= depth then (mempty, mempty)
          else let subStart i = if i == 0 then 1 else width ^ i + subStart (i-1)
                   subVals = [ subStart l .. ]
                   (subNs, subEs) =
                     fst $ foldr addToRoot ((mempty, mempty), subVals) cur
                   addToRoot r ((rnodes, redges), vals) =
                     let (thisSub, remSub) = splitAt width vals
                         thisEdges = (r,) <$> thisSub
                     in ((rnodes <> thisSub, redges <> thisEdges), remSub)
                   next = go subNs (l+1)
                   extra =
                     let esrcs = concat $ replicate width cur
                         etgts = [ t
                                 | t <- drop l $ fst next
                                 , t `mod` (width + 3) == 0 ]
                     in zip esrcs etgts
               in (cur <> subNs <> fst next, subEs <> extra <> snd next)


mkFglGraph :: Int -> Int -> FGL.Gr Int ()
mkFglGraph depth width =
  let (nodes, edgs) = mkEdges width depth
      edges = fmap (\(s,d) -> (s,d,())) edgs
  in FGL.mkGraph (zip nodes nodes) edges


mkHaggleDiGraph :: Int -> Int
                -> (HGL.Vertex, HGL.VertexLabeledGraph HGL.Digraph Int)
mkHaggleDiGraph depth width =
  let (_nodes, edges) = mkEdges width depth
      g = fst $ HGL.fromEdgeList  HGL.newMDigraph edges
      Just v = vertexFromLabel g 0
  in (v, g)

mkHaggleBiDiGraph :: Int -> Int -> (HGL.Vertex, HGL.VertexLabeledGraph HGL.BiDigraph Int)
mkHaggleBiDiGraph depth width =
  let (_nodes, edges) = mkEdges width depth
      g = fst $ HGL.fromEdgeList HGL.newMBiDigraph edges
      Just v = vertexFromLabel g 0
  in (v, g)

mkHaggleSimpleBiDiGraph :: Int -> Int -> (HGL.Vertex, HGL.VertexLabeledGraph HGL.SimpleBiDigraph Int)
mkHaggleSimpleBiDiGraph depth width =
  let (_nodes, edges) = mkEdges width depth
      g = fst $ HGL.fromEdgeList HGL.newMSimpleBiDigraph edges
      Just v = vertexFromLabel g 0
  in (v, g)

mkHagglePatriciaGraph :: Int -> Int -> (HGL.Vertex, HGL.PatriciaTree Int ())
mkHagglePatriciaGraph depth width =
  let (nodes, edges) = mkEdges width depth
      addNode g n = snd $ HGL.insertLabeledVertex g n
      gnodes = foldl addNode HGL.emptyGraph nodes
      addEdge g (s,d) = fromJust $ do sv <- vertexFromLabel g s
                                      dv <- vertexFromLabel g d
                                      (_,g') <- HGL.insertLabeledEdge g sv dv ()
                                      return g'
      pgraph = foldl addEdge gnodes edges
      Just v = vertexFromLabel pgraph 0
  in (v, pgraph)


vertexFromLabel :: HGL.HasVertexLabel g
                => Eq (HGL.VertexLabel g)
                => g -> HGL.VertexLabel g -> Maybe HGL.Vertex
vertexFromLabel g lbl = F.find labelMatch (HGL.vertices g)
  where
    labelMatch v = Just lbl == (HGL.vertexLabel g v)

unique :: (Ord a) => [a] -> [a]
unique = S.toList . S.fromList

testFglDFS :: FGL.Graph gr => gr a b -> [FGL.Node]
testFglDFS g = let r = FGL.dfs [0,0,0] g in if null r then error "bad fgl dfs" else r

testHaggleDFS :: HGL.HasVertexLabel g
              => Eq (HGL.VertexLabel g)
              => Num (HGL.VertexLabel g)
              => (HGL.Vertex, g) -> [Maybe (HGL.VertexLabel g)]
testHaggleDFS (r,g) = HGL.vertexLabel g <$> HGL.dfs g [r,r,r]

testHaggleXDFS :: HGL.HasVertexLabel g
               => Eq (HGL.VertexLabel g)
               => Num (HGL.VertexLabel g)
               => (HGL.Vertex, g) -> [Maybe (HGL.VertexLabel g)]
testHaggleXDFS (r,g) = HGL.vertexLabel g
                       <$> HGL.xdfsWith g (HGL.successors g) id [r,r,r]

main :: IO ()
main = do setup
          defaultMain [
            bgroup "dfs" [
                bgroup "nf" [
                    bench "fgl" $ nf testFglDFS g1f
                    , bench "haggle.di" $ nf testHaggleDFS g1hd
                    , bench "haggle.bidi" $ nf testHaggleDFS g1hbd
                    , bench "haggle.simplebidi" $ nf testHaggleDFS g1hsbd
                    , bench "haggle.patricia" $ nf testHaggleDFS g1hp
                    ]
                ]
            , bgroup "xdfs" [
                bgroup "nf" [
                    bench "haggle.di" $ nf testHaggleXDFS g1hd
                    , bench "haggle.bidi" $ nf testHaggleXDFS g1hbd
                    , bench "haggle.simplebidi" $ nf testHaggleXDFS g1hsbd
                    , bench "haggle.patricia" $ nf testHaggleXDFS g1hp
                    ]
                -- , bgroup "whnf" [
                --     bench "haggle.di" $ whnf testHaggleXDFS g1hd
                --     , bench "haggle.bidi" $ whnf testHaggleXDFS g1hbd
                --     , bench "haggle.simplebidi" $ whnf testHaggleXDFS g1hsbd
                --     , bench "haggle.patricia" $ whnf testHaggleXDFS g1hp
                --     ]
                ]
            , bgroup "topsort" [
                bgroup "nf" [
                    bench "fgl" $ nf FGL.topsort g1f
                    , bench "haggle.di" $ nf HGL.topsort $ snd g1hd
                    , bench "haggle.bidi" $ nf HGL.topsort $ snd g1hbd
                    , bench "haggle.simplebidi" $ nf HGL.topsort $ snd g1hsbd
                    , bench "haggle.patricia" $ nf HGL.topsort $ snd g1hp
                    ]
                ]
            , bgroup "scc" [
                bgroup "nf" [
                    bench "fgl" $ nf FGL.scc g1f
                    -- , bench "haggle.di" $ nf HGL.scc $ snd g1hd
                    , bench "haggle.bidi" $ nf HGL.scc $ snd g1hbd
                    , bench "haggle.simplebidi" $ nf HGL.scc $ snd g1hsbd
                    , bench "haggle.patricia" $ nf HGL.scc $ snd g1hp
                    ]
                ]
            , bgroup "isConnected" [
                bgroup "nf" [
                    bench "fgl" $ nf FGL.isConnected g1f
                    -- , bench "haggle.di" $ nf HGL.isConnected $ snd g1hd
                    , bench "haggle.bidi" $ nf HGL.isConnected $ snd g1hbd
                    , bench "haggle.simplebidi" $ nf HGL.isConnected $ snd g1hsbd
                    , bench "haggle.patricia" $ nf HGL.isConnected $ snd g1hp
                    ]
                ]
            ]
  where
    setup = g1f
            -- `deepseq` g1hd   -- error: uninitialised element (from Vector)
            -- `deepseq` g1hbd  -- no instance for NFData
            -- `deepseq` g1hsbd   -- error: uninitialised element (from Vector)
            `deepseq` g1hp
            `deepseq` return ()

    g1f :: FGL.Gr Int ()
    g1f = mkFglGraph 4 5

    g1hd = mkHaggleDiGraph 4 5
    g1hbd = mkHaggleBiDiGraph 4 5
    g1hsbd = mkHaggleSimpleBiDiGraph 4 5
    g1hp = mkHagglePatriciaGraph 4 5
