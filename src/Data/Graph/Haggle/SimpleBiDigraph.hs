{-# LANGUAGE TypeFamilies #-}
-- | This is a simple graph (it does not allow parallel edges).  To support
-- this efficiently, it is less compact than 'Digraph' or 'BiDigraph'.  As
-- a consequence, edge existence tests are efficient (logarithmic in the
-- number of edges leaving the source vertex).
module Data.Graph.Haggle.SimpleBiDigraph (
  MSimpleBiDigraph,
  SimpleBiDigraph,
  newMSimpleBiDigraph,
  newSizedMSimpleBiDigraph
  ) where

import Control.Monad ( when )
import qualified Control.Monad.Primitive as P
import qualified Control.Monad.Ref as R
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V

import Data.Graph.Haggle.Classes
import Data.Graph.Haggle.Internal.Basic

-- type EdgeID = Int

data MSimpleBiDigraph m = -- See Note [Graph Representation]
  MBiDigraph { mgraphVertexCount :: R.Ref m Int
             , mgraphEdgeCount :: R.Ref m Int
             , mgraphPreds :: R.Ref m (MV.MVector (P.PrimState m) (IntMap Edge))
             , mgraphSuccs :: R.Ref m (MV.MVector (P.PrimState m) (IntMap Edge))
             }

data SimpleBiDigraph =
  BiDigraph { vertexCount :: {-# UNPACK #-} !Int
            , edgeCount :: {-# UNPACK #-} !Int
            , graphPreds :: V.Vector (IntMap Edge)
            , graphSuccs :: V.Vector (IntMap Edge)
            }

defaultSize :: Int
defaultSize = 128

newMSimpleBiDigraph :: (P.PrimMonad m, R.MonadRef m) => m (MSimpleBiDigraph m)
newMSimpleBiDigraph = newSizedMSimpleBiDigraph defaultSize 0

newSizedMSimpleBiDigraph :: (P.PrimMonad m, R.MonadRef m) => Int -> Int -> m (MSimpleBiDigraph m)
newSizedMSimpleBiDigraph szNodes _ = do
  when (szNodes < 0) $ error "Negative size (newSized)"
  nn <- R.newRef 0
  en <- R.newRef 0
  pvec <- MV.new szNodes
  svec <- MV.new szNodes
  pref <- R.newRef pvec
  sref <- R.newRef svec
  return $! MBiDigraph { mgraphVertexCount = nn
                       , mgraphEdgeCount = en
                       , mgraphPreds = pref
                       , mgraphSuccs = sref
                       }

instance MGraph MSimpleBiDigraph where
  type ImmutableGraph MSimpleBiDigraph = SimpleBiDigraph
  getVertices g = do
    nVerts <- R.readRef (mgraphVertexCount g)
    return [V v | v <- [0..nVerts - 1]]

  getOutEdges g (V src) = do
    nVerts <- R.readRef (mgraphVertexCount g)
    case src >= nVerts of
      True -> return []
      False -> do
        svec <- R.readRef (mgraphSuccs g)
        succs <- MV.unsafeRead svec src
        return $ IM.elems succs

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
    pvec <- R.readRef (mgraphPreds g)
    svec <- R.readRef (mgraphSuccs g)
    pvec' <- V.freeze (MV.take nVerts pvec)
    svec' <- V.freeze (MV.take nVerts svec)
    return $! BiDigraph { vertexCount = nVerts
                        , edgeCount = nEdges
                        , graphPreds = pvec'
                        , graphSuccs = svec'
                        }

instance MAddVertex MSimpleBiDigraph where
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

instance MAddEdge MSimpleBiDigraph where
  addEdge g v1@(V src) v2@(V dst) = do
    nVerts <- R.readRef (mgraphVertexCount g)
    exists <- checkEdgeExists g v1 v2
    case exists || src >= nVerts || dst >= nVerts of
      True -> return Nothing
      False -> do
        eid <- R.readRef (mgraphEdgeCount g)
        let e = E eid src dst
        R.modifyRef' (mgraphEdgeCount g) (+1)

        pvec <- R.readRef (mgraphPreds g)
        preds <- MV.unsafeRead pvec dst
        MV.unsafeWrite pvec dst (IM.insert src e preds)

        svec <- R.readRef (mgraphSuccs g)
        succs <- MV.unsafeRead svec src
        MV.unsafeWrite svec src (IM.insert dst e succs)

        return $ Just e

instance MBidirectional MSimpleBiDigraph where
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
        return $ IM.elems preds

instance Thawable SimpleBiDigraph where
  type MutableGraph SimpleBiDigraph = MSimpleBiDigraph
  thaw g = do
    vc <- R.newRef (vertexCount g)
    ec <- R.newRef (edgeCount g)
    pvec <- V.thaw (graphPreds g)
    svec <- V.thaw (graphSuccs g)
    pref <- R.newRef pvec
    sref <- R.newRef svec
    return MBiDigraph { mgraphVertexCount = vc
                      , mgraphEdgeCount = ec
                      , mgraphPreds = pref
                      , mgraphSuccs = sref
                      }

instance Graph SimpleBiDigraph where
  -- FIXME: This will be more complicated if we support removing vertices
  vertices g = map V [0 .. vertexCount g - 1]
  edges g = concatMap (outEdges g) (vertices g)
  successors g (V v)
    | outOfRange g v = []
    | otherwise = map V $ IM.keys $ V.unsafeIndex (graphSuccs g) v
  outEdges g (V v)
    | outOfRange g v = []
    | otherwise =
      let succs = V.unsafeIndex (graphSuccs g) v
      in IM.elems succs
  edgeExists g (V src) (V dst)
    | outOfRange g src || outOfRange g dst = False
    | otherwise = IM.member dst (V.unsafeIndex (graphSuccs g) src)
  maxVertexId g = V.length (graphSuccs g) - 1
  isEmpty = (==0) . vertexCount


instance Bidirectional SimpleBiDigraph  where
  predecessors g (V v)
    | outOfRange g v = []
    | otherwise = map V $ IM.keys $ V.unsafeIndex (graphPreds g) v
  inEdges g (V v)
    | outOfRange g v = []
    | otherwise =
      let preds = V.unsafeIndex (graphPreds g) v
      in IM.elems preds

-- Helpers

outOfRange :: SimpleBiDigraph -> Int -> Bool
outOfRange g = (>= vertexCount g)

-- | Given a graph, ensure that there is space in the vertex vector
-- for a new vertex.  If there is not, double the capacity.
ensureNodeSpace :: (P.PrimMonad m, R.MonadRef m) => MSimpleBiDigraph m -> m ()
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


{- Note [Graph Representation]

Each of the IntMaps in the vectors maps the edge *destination* node id to the
*edge id*.  We need to store the edge IDs to reconstruct an Edge.  Other graph
representations use the edge IDs to maintain lists, but here we don't have
that.  The destination is the key of the map for fast edgeExists tests.

-}
