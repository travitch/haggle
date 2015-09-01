{-# LANGUAGE TypeFamilies #-}
-- | This graph is an efficient representation of bidirectional graphs with
-- parallel edges.
--
-- This is in contrast to 'Data.Graph.Haggle.SimpleBiDigraph', which
-- can only handle simple graphs (i.e., without parallel edges).
--
-- The representation is slightly less efficient as a result.
module Data.Graph.Haggle.BiDigraph (
  MBiDigraph,
  BiDigraph,
  newMBiDigraph,
  newSizedMBiDigraph
  ) where

import Control.Monad ( when )
import qualified Control.Monad.Primitive as P
import qualified Control.Monad.Ref as R
import Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V

import Data.Graph.Haggle
import Data.Graph.Haggle.Internal.Basic

-- | A mutable bidirectional graph
data MBiDigraph m =
  MBiDigraph { mgraphVertexCount :: R.Ref m Int
             , mgraphEdgeCount :: R.Ref m Int
             , mgraphEdgeIdSrc :: R.Ref m Int
             , mgraphPreds :: R.Ref m (MV.MVector (P.PrimState m) (IntMap [Edge]))
             , mgraphSuccs :: R.Ref m (MV.MVector (P.PrimState m) (IntMap [Edge]))
             }

-- | An immutable bidirectional graph
data BiDigraph =
  BiDigraph { vertexCount :: {-# UNPACK #-} !Int
            , edgeCount :: {-# UNPACK #-} !Int
            , edgeIdSrc :: {-# UNPACK #-} !Int
            , graphPreds :: V.Vector (IntMap [Edge])
            , graphSuccs :: V.Vector (IntMap [Edge])
            }


defaultSize :: Int
defaultSize = 128

-- | Allocate a new mutable bidirectional graph with a default size
newMBiDigraph :: (P.PrimMonad m, R.MonadRef m) => m (MBiDigraph m)
newMBiDigraph = newSizedMBiDigraph defaultSize 0

-- | Allocate a new mutable bidirectional graph with space reserved
-- for nodes and edges.  This can be more efficient and avoid resizing.
newSizedMBiDigraph :: (P.PrimMonad m, R.MonadRef m)
                   => Int -- ^ Reserved space for nodes
                   -> Int -- ^ Reserved space for edges
                   -> m (MBiDigraph m)
newSizedMBiDigraph szNodes _ = do
  when (szNodes < 0) $ error "newSizedMBiDigraph: Negative size"
  nn <- R.newRef 0
  en <- R.newRef 0
  esrc <- R.newRef 0
  pvec <- MV.new szNodes
  svec <- MV.new szNodes
  pref <- R.newRef pvec
  sref <- R.newRef svec
  return $! MBiDigraph { mgraphVertexCount = nn
                       , mgraphEdgeCount = en
                       , mgraphEdgeIdSrc = esrc
                       , mgraphPreds = pref
                       , mgraphSuccs = sref
                       }

instance MGraph MBiDigraph where
  type ImmutableGraph MBiDigraph = BiDigraph
  getVertices g = do
    nVerts <- R.readRef (mgraphVertexCount g)
    return [ V v | v <- [0.. nVerts - 1] ]

  getOutEdges g (V src) = do
    nVerts <- R.readRef (mgraphVertexCount g)
    case src >= nVerts of
      True -> return []
      False -> do
        svec <- R.readRef (mgraphSuccs g)
        succs <- MV.unsafeRead svec src
        return $ concat (IM.elems succs)
  countVertices = R.readRef . mgraphVertexCount
  countEdges = R.readRef . mgraphEdgeCount

  getSuccessors g (V src) = do
    nVerts <- R.readRef (mgraphVertexCount g)
    case src >= nVerts of
      True -> return []
      False -> do
        svec <- R.readRef (mgraphSuccs g)
        succs <- MV.unsafeRead svec src
        return $ map V $ IM.keys succs

  checkEdgeExists g (V src) (V dst) = do
    nVerts <- R.readRef (mgraphVertexCount g)
    case src >= nVerts || dst >= nVerts of
      True -> return False
      False -> do
        svec <- R.readRef (mgraphSuccs g)
        succs <- MV.unsafeRead svec src
        return $ IM.member dst succs

  freeze g = do
    nVerts <- R.readRef (mgraphVertexCount g)
    nEdges <- R.readRef (mgraphEdgeCount g)
    esrc <- R.readRef (mgraphEdgeIdSrc g)
    pvec <- R.readRef (mgraphPreds g)
    svec <- R.readRef (mgraphSuccs g)
    pvec' <- V.freeze (MV.take nVerts pvec)
    svec' <- V.freeze (MV.take nVerts svec)
    return $! BiDigraph { vertexCount = nVerts
                        , edgeCount = nEdges
                        , edgeIdSrc = esrc
                        , graphPreds = pvec'
                        , graphSuccs = svec'
                        }

instance MAddVertex MBiDigraph where
  addVertex g = do
    ensureNodeSpace g
    vid <- R.readRef r
    R.modifyRef' r (+1)
    pvec <- R.readRef (mgraphPreds g)
    svec <- R.readRef (mgraphSuccs g)
    MV.write pvec vid IM.empty
    MV.write svec vid IM.empty
    return (V vid)
    where
      r = mgraphVertexCount g


instance MAddEdge MBiDigraph where
  addEdge g v1@(V src) v2@(V dst) = do
    nVerts <- R.readRef (mgraphVertexCount g)
    exists <- checkEdgeExists g v1 v2
    case exists || src >= nVerts || dst >= nVerts of
      True -> return Nothing
      False -> do
        eid <- R.readRef (mgraphEdgeIdSrc g)
        R.modifyRef' (mgraphEdgeIdSrc g) (+1)
        R.modifyRef' (mgraphEdgeCount g) (+1)
        let e = E eid src dst
        pvec <- R.readRef (mgraphPreds g)
        preds <- MV.unsafeRead pvec dst
        MV.unsafeWrite pvec dst (IM.insertWith (++) src [e] preds)

        svec <- R.readRef (mgraphSuccs g)
        succs <- MV.unsafeRead svec src
        MV.unsafeWrite svec src (IM.insertWith (++) dst [e] succs)

        return $ Just e

instance MBidirectional MBiDigraph where
  getPredecessors g (V vid) = do
    nVerts <- R.readRef (mgraphVertexCount g)
    case vid < nVerts of
      False -> return []
      True -> do
        pvec <- R.readRef (mgraphPreds g)
        preds <- MV.unsafeRead pvec vid
        return $ map V $ IM.keys preds

  getInEdges g (V vid) = do
    nVerts <- R.readRef (mgraphVertexCount g)
    case vid < nVerts of
      False -> return []
      True -> do
        pvec <- R.readRef (mgraphPreds g)
        preds <- MV.unsafeRead pvec vid
        return $ concat (IM.elems preds)

instance Thawable BiDigraph where
  type MutableGraph BiDigraph = MBiDigraph
  thaw g = do
    vc <- R.newRef (vertexCount g)
    ec <- R.newRef (edgeCount g)
    eidsrc <- R.newRef (edgeIdSrc g)
    pvec <- V.thaw (graphPreds g)
    svec <- V.thaw (graphSuccs g)
    pref <- R.newRef pvec
    sref <- R.newRef svec
    return MBiDigraph { mgraphVertexCount = vc
                      , mgraphEdgeCount = ec
                      , mgraphEdgeIdSrc = eidsrc
                      , mgraphPreds = pref
                      , mgraphSuccs = sref
                      }

instance Graph BiDigraph where
  vertices g = map V [0 .. vertexCount g - 1]
  edges g = concatMap (outEdges g) (vertices g)
  successors g (V v)
    | outOfRange g v = []
    | otherwise = map V $ IM.keys $ V.unsafeIndex (graphSuccs g) v
  outEdges g (V v)
    | outOfRange g v = []
    | otherwise =
      let succs = V.unsafeIndex (graphSuccs g) v
      in concat (IM.elems succs)
  edgeExists g (V src) (V dst)
    | outOfRange g src || outOfRange g dst = False
    | otherwise = IM.member dst (V.unsafeIndex (graphSuccs g) src)
  maxVertexId g = V.length (graphSuccs g) - 1
  isEmpty = (==0) . vertexCount

instance Bidirectional BiDigraph  where
  predecessors g (V v)
    | outOfRange g v = []
    | otherwise = map V $ IM.keys $ V.unsafeIndex (graphPreds g) v
  inEdges g (V v)
    | outOfRange g v = []
    | otherwise =
      let preds = V.unsafeIndex (graphPreds g) v
      in concat (IM.elems preds)

-- Helpers

outOfRange :: BiDigraph -> Int -> Bool
outOfRange g = (>= vertexCount g)

-- | Given a graph, ensure that there is space in the vertex vector
-- for a new vertex.  If there is not, double the capacity.
ensureNodeSpace :: (P.PrimMonad m, R.MonadRef m) => MBiDigraph m -> m ()
ensureNodeSpace g = do
  pvec <- R.readRef (mgraphPreds g)
  svec <- R.readRef (mgraphSuccs g)
  let cap = MV.length pvec
  cnt <- R.readRef (mgraphVertexCount g)
  case cnt < cap of
    True -> return ()
    False -> do
      pvec' <- MV.grow pvec cap
      svec' <- MV.grow svec cap
      R.writeRef (mgraphPreds g) pvec'
      R.writeRef (mgraphSuccs g) svec'

