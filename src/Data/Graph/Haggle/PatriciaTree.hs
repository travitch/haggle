{-# LANGUAGE TypeFamilies, BangPatterns, DeriveFunctor #-}
-- | This graph is based on the implementation in fgl (using
-- big-endian patricia-tries -- IntMap).
--
-- This formulation does not support parallel edges.
module Data.Graph.Haggle.PatriciaTree ( PatriciaTree ) where

import           Control.DeepSeq
import           Control.Monad ( guard )
import           Data.Bifunctor
import           Data.Foldable ( toList )
import           Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import           Data.Maybe ( fromMaybe )
import           Data.Monoid

import           Prelude

import qualified Data.Graph.Haggle.Classes as I
import qualified Data.Graph.Haggle.Internal.Basic as I

data Ctx nl el = Ctx !(IntMap el) I.Vertex nl !(IntMap el)
  deriving Functor

instance (NFData nl, NFData el) => NFData (Ctx nl el) where
  rnf (Ctx p v nl s) =
    p `deepseq` s `deepseq` nl `deepseq` v `seq` ()

-- | The 'PatriciaTree' is a graph implementing the 'I.InductiveGraph'
-- interface (as well as the other immutable graph interfaces).  It is
-- based on the graph type provided by fgl.
--
-- Inductive graphs support more interesting decompositions than the
-- other graph interfaces in this library, at the cost of less compact
-- representations and some additional overhead on some operations, as
-- most must go through the 'I.match' operator.
--
-- This graph type is most useful for incremental construction in pure
-- code.  It also supports node removal from pure code.
data PatriciaTree nl el = Gr { graphRepr :: IntMap (Ctx nl el) }
  deriving Functor

instance (NFData nl, NFData el) => NFData (PatriciaTree nl el) where
  rnf (Gr im) = im `deepseq` ()

instance Bifunctor PatriciaTree where
  first f (Gr im) =
    let onNode (Ctx inM v n outM) = Ctx inM v (f n) outM
    in Gr $ fmap onNode im
  second f (Gr im) =
    let onEdge (Ctx inM v n outM) = Ctx (f <$> inM) v n (f <$> outM)
    in Gr $ fmap onEdge im

instance I.Graph (PatriciaTree nl el) where
  vertices = map I.V . IM.keys . graphRepr
  isEmpty = IM.null . graphRepr
  maxVertexId (Gr g)
    | IM.null g = 0
    | otherwise = fst $ IM.findMax g
  edgesBetween (Gr g) (I.V src) (I.V dst) = toList $ do
    Ctx _ _ _ ss <- IM.lookup src g
    guard (IM.member dst ss)
    return (I.E (-1) src dst)
  edges g = concatMap (I.outEdges g) (I.vertices g)
  successors (Gr g) (I.V v) = fromMaybe [] $ do
    Ctx _ _ _ ss <- IM.lookup v g
    return $ map I.V $ IM.keys ss
  outEdges (Gr g) (I.V v) = fromMaybe [] $ do
    Ctx _ _ _ ss <- IM.lookup v g
    return $ map toEdge (IM.keys ss)
    where
      toEdge d = I.E (-1) v d

instance I.HasEdgeLabel (PatriciaTree nl el) where
  type EdgeLabel (PatriciaTree nl el) = el
  edgeLabel (Gr g) (I.E _ src dst) = do
    Ctx _ _ _ ss <- IM.lookup src g
    IM.lookup dst ss
  labeledEdges gr = map toLabEdge (I.edges gr)
    where
      toLabEdge e =
        case I.edgeLabel gr e of
          Just lab -> (e, lab)
          Nothing -> error "Impossible: PatriciaTree instances always have edge labels"

  labeledOutEdges (Gr g) (I.V s) = fromMaybe [] $ do
    Ctx _ _ _ ss <- IM.lookup s g
    return $ IM.foldrWithKey toOut [] ss
    where
      toOut d lbl acc = (I.E (-1) s d, lbl) : acc

instance I.HasVertexLabel (PatriciaTree nl el) where
  type VertexLabel (PatriciaTree nl el) = nl
  vertexLabel (Gr g) (I.V v) = do
    Ctx _ _ lbl _ <- IM.lookup v g
    return lbl
  labeledVertices gr = map toLabVert (I.vertices gr)
    where
      toLabVert v =
        case I.vertexLabel gr v of
          Just l -> (v, l)
          Nothing -> error "Impossible: PatriciaTree instances always have vertex labels"

instance I.Bidirectional (PatriciaTree nl el) where
  predecessors (Gr g) (I.V v) = fromMaybe [] $ do
    Ctx pp _ _ _ <- IM.lookup v g
    return $ map I.V (IM.keys pp)
  inEdges (Gr g) (I.V v) = fromMaybe [] $ do
    Ctx pp _ _ _ <- IM.lookup v g
    return $ map toEdge (IM.keys pp)
    where
      toEdge s = I.E (-1) s v

instance I.BidirectionalEdgeLabel (PatriciaTree nl el) where
  labeledInEdges (Gr g) (I.V d) = fromMaybe [] $ do
    Ctx pp _ _ _ <- IM.lookup d g
    return $ IM.foldrWithKey toIn [] pp
    where
      toIn s lbl acc = (I.E (-1) s d, lbl) : acc

instance I.InductiveGraph (PatriciaTree nl el) where
  emptyGraph = Gr IM.empty
  insertLabeledVertex gr@(Gr g) lab =
    let vid = I.maxVertexId gr + 1
        v = I.V vid
        g' = IM.insert vid (Ctx mempty v lab mempty) g
    in (v, Gr g')
  replaceLabeledVertex (Gr g) (I.V v) vl =
    let updLabel (Ctx ie nv _nl oe) = Ctx ie nv vl oe
    in Gr $ IM.adjust updLabel v g
  insertLabeledEdge gr@(Gr g) v1@(I.V src) (I.V dst) lab | src == dst = do
    guard (IM.member src g)
    guard (not (I.edgeExists gr v1 v1))
    let e = I.E (-1) src src
    Ctx spp sv sl sss <- IM.lookup src g
    let ctx' = Ctx (IM.insert src lab spp) sv sl (IM.insert dst lab sss)
        !g' = IM.insert src ctx' g
    return (e, Gr g')
  insertLabeledEdge gr@(Gr g) v1@(I.V src) v2@(I.V dst) lab = do
    guard (IM.member src g && IM.member dst g)
    guard (not (I.edgeExists gr v1 v2))
    let e = I.E (-1) src dst
    Ctx spp sv sl sss <- IM.lookup src g
    Ctx dpp dv dl dss <- IM.lookup dst g
    let sctx' = Ctx spp sv sl (IM.insert dst lab sss)
        dctx' = Ctx (IM.insert src lab dpp) dv dl dss
        !g' = IM.insert src sctx' g
        !g'' = IM.insert dst dctx' g'
    return (e, Gr g'')
  deleteEdge g (I.E _ s d) = I.deleteEdgesBetween g (I.V s) (I.V d)
  deleteEdgesBetween gr@(Gr g) (I.V src) (I.V dst) | src == dst = fromMaybe gr $ do
    Ctx spp sv sl sss <- IM.lookup src g
    let ctx' = Ctx (IM.delete src spp) sv sl (IM.delete src sss)
        !g' = IM.insert src ctx' g
    return (Gr g')
  deleteEdgesBetween gr@(Gr g) (I.V src) (I.V dst) = fromMaybe gr $ do
    Ctx spp sv sl sss <- IM.lookup src g
    Ctx dpp dv dl dss <- IM.lookup dst g
    let sctx' = Ctx spp sv sl (IM.delete dst sss)
        dctx' = Ctx (IM.delete src dpp) dv dl dss
        !g' = IM.insert src sctx' g
        !g'' = IM.insert dst dctx' g'
    return (Gr g'')
  context (Gr g) (I.V v) = do
    Ctx pp _ l ss <- IM.lookup v g
    return $ I.Context (toAdj pp) l (toAdj ss)
  match (Gr g) (I.V v) = do
    Ctx pp _ l ss <- IM.lookup v g
    let g' = foldr (IM.adjust (removeSucc v)) g (IM.keys pp)
        g'' = foldr (IM.adjust (removePred v)) g' (IM.keys ss)
        g''' = IM.delete v g''
    return $ (I.Context (toAdj pp) l (toAdj ss), Gr g''')

toAdj :: IntMap a -> [(a, I.Vertex)]
toAdj = IM.foldrWithKey f []
  where
    f dst lbl acc = (lbl, I.V dst) : acc

removeSucc :: Int -> Ctx nl el -> Ctx nl el
removeSucc v (Ctx pp vert lbl ss) =
  Ctx pp vert lbl (IM.delete v ss)

removePred :: Int -> Ctx nl el -> Ctx nl el
removePred v (Ctx pp vert lbl ss) =
  Ctx (IM.delete v pp) vert lbl ss

{- Note [Representation]

Since this graph does not support parallel edges, the edge ID does not
actually matter.  This implementation will let it always be zero.  Edge
identity can be recovered with just (src, dst).

-}


