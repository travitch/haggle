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
import Control.Monad.ST
import Data.STRef
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V

import Data.Graph.Haggle
import Data.Graph.Haggle.Internal.Basic

-- type EdgeID = Int

data MSimpleBiDigraph s = -- See Note [Graph Representation]
  MBiDigraph { mgraphVertexCount :: STRef s Int
             , mgraphEdgeCount :: STRef s Int
             , mgraphPreds :: STRef s (MV.STVector s (IntMap Edge))
             , mgraphSuccs :: STRef s (MV.STVector s (IntMap Edge))
             }

data SimpleBiDigraph =
  BiDigraph { vertexCount :: {-# UNPACK #-} !Int
            , edgeCount :: {-# UNPACK #-} !Int
            , graphPreds :: V.Vector (IntMap Edge)
            , graphSuccs :: V.Vector (IntMap Edge)
            }

defaultSize :: Int
defaultSize = 128

newMSimpleBiDigraph :: ST s (MSimpleBiDigraph s)
newMSimpleBiDigraph = newSizedMSimpleBiDigraph defaultSize 0

newSizedMSimpleBiDigraph :: Int -> Int -> ST s (MSimpleBiDigraph s)
newSizedMSimpleBiDigraph szNodes _ = do
  when (szNodes < 0) $ error "Negative size (newSized)"
  nn <- newSTRef 0
  en <- newSTRef 0
  pvec <- MV.new szNodes
  svec <- MV.new szNodes
  pref <- newSTRef pvec
  sref <- newSTRef svec
  return $! MBiDigraph { mgraphVertexCount = nn
                       , mgraphEdgeCount = en
                       , mgraphPreds = pref
                       , mgraphSuccs = sref
                       }

instance MGraph MSimpleBiDigraph where
  type ImmutableGraph MSimpleBiDigraph = SimpleBiDigraph
  getVertices g = do
    nVerts <- readSTRef (mgraphVertexCount g)
    return [V v | v <- [0..nVerts - 1]]

  getOutEdges g (V src) = do
    nVerts <- readSTRef (mgraphVertexCount g)
    case src >= nVerts of
      True -> return []
      False -> do
        svec <- readSTRef (mgraphSuccs g)
        succs <- MV.unsafeRead svec src
        return $ IM.elems succs
--        return $ IM.foldrWithKey (\dst eid acc -> E eid src dst : acc) [] succs

  countVertices = readSTRef . mgraphVertexCount
  countEdges = readSTRef . mgraphEdgeCount

  getSuccessors g (V src) = do
    nVerts <- readSTRef (mgraphVertexCount g)
    case src >= nVerts of
      True -> return []
      False -> do
        svec <- readSTRef (mgraphSuccs g)
        succs <- MV.unsafeRead svec src
        return $ map V $ IM.keys succs

  checkEdgeExists g (V src) (V dst) = do
    nVerts <- readSTRef (mgraphVertexCount g)
    case src >= nVerts || dst >= nVerts of
      True -> return False
      False -> do
        svec <- readSTRef (mgraphSuccs g)
        succs <- MV.unsafeRead svec src
        return $ IM.member dst succs

  freeze g = do
    nVerts <- readSTRef (mgraphVertexCount g)
    nEdges <- readSTRef (mgraphEdgeCount g)
    pvec <- readSTRef (mgraphPreds g)
    svec <- readSTRef (mgraphSuccs g)
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
    vid <- readSTRef r
    modifySTRef' r (+1)
    pvec <- readSTRef (mgraphPreds g)
    svec <- readSTRef (mgraphSuccs g)
    MV.write pvec vid IM.empty
    MV.write svec vid IM.empty
    return (V vid)
    where
      r = mgraphVertexCount g

instance MAddEdge MSimpleBiDigraph where
  addEdge g v1@(V src) v2@(V dst) = do
    nVerts <- readSTRef (mgraphVertexCount g)
    exists <- checkEdgeExists g v1 v2
    case exists || src >= nVerts || dst >= nVerts of
      True -> return Nothing
      False -> do
        eid <- readSTRef (mgraphEdgeCount g)
        let e = E eid src dst
        modifySTRef' (mgraphEdgeCount g) (+1)

        pvec <- readSTRef (mgraphPreds g)
        preds <- MV.unsafeRead pvec dst
        MV.unsafeWrite pvec dst (IM.insert src e preds)

        svec <- readSTRef (mgraphSuccs g)
        succs <- MV.unsafeRead svec src
        MV.unsafeWrite svec src (IM.insert dst e succs)

        return $ Just e

instance MBidirectional MSimpleBiDigraph where
  getPredecessors g (V vid) = do
    nVerts <- readSTRef (mgraphVertexCount g)
    case vid < nVerts of
      False -> return []
      True -> do
        pvec <- readSTRef (mgraphPreds g)
        preds <- MV.unsafeRead pvec vid
        return $ map V $ IM.keys preds

  getInEdges g (V vid) = do
    nVerts <- readSTRef (mgraphVertexCount g)
    case vid < nVerts of
      False -> return []
      True -> do
        pvec <- readSTRef (mgraphPreds g)
        preds <- MV.unsafeRead pvec vid
        return $ IM.elems preds
 --       return $ IM.foldrWithKey' (\src eid acc -> E eid src vid : acc) [] preds

instance Graph SimpleBiDigraph where
  type MutableGraph SimpleBiDigraph = MSimpleBiDigraph
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
--      in IM.foldrWithKey' (\dst eid acc -> E eid v dst : acc) [] succs
  edgeExists g (V src) (V dst)
    | outOfRange g src || outOfRange g dst = False
    | otherwise = IM.member dst (V.unsafeIndex (graphSuccs g) src)
  maxVertexId g = V.length (graphSuccs g) - 1
  isEmpty = (==0) . vertexCount
  thaw g = do
    vc <- newSTRef (vertexCount g)
    ec <- newSTRef (edgeCount g)
    pvec <- V.thaw (graphPreds g)
    svec <- V.thaw (graphSuccs g)
    pref <- newSTRef pvec
    sref <- newSTRef svec
    return MBiDigraph { mgraphVertexCount = vc
                      , mgraphEdgeCount = ec
                      , mgraphPreds = pref
                      , mgraphSuccs = sref
                      }


instance Bidirectional SimpleBiDigraph  where
  predecessors g (V v)
    | outOfRange g v = []
    | otherwise = map V $ IM.keys $ V.unsafeIndex (graphPreds g) v
  inEdges g (V v)
    | outOfRange g v = []
    | otherwise =
      let preds = V.unsafeIndex (graphPreds g) v
      in IM.elems preds
      -- in IM.foldrWithKey' (\src eid acc -> E eid src v : acc) [] preds

-- Helpers

outOfRange :: SimpleBiDigraph -> Int -> Bool
outOfRange g = (>= vertexCount g)

-- | Given a graph, ensure that there is space in the vertex vector
-- for a new vertex.  If there is not, double the capacity.
ensureNodeSpace :: MSimpleBiDigraph s -> ST s ()
ensureNodeSpace g = do
  pvec <- readSTRef (mgraphPreds g)
  svec <- readSTRef (mgraphSuccs g)
  let cap = MV.length pvec
  cnt <- readSTRef (mgraphVertexCount g)
  case cnt < cap of
    True -> return ()
    False -> do
      pvec' <- MV.grow pvec cap
      svec' <- MV.grow svec cap
      writeSTRef (mgraphPreds g) pvec'
      writeSTRef (mgraphSuccs g) svec'


{- Note [Graph Representation]

Each of the IntMaps in the vectors maps the edge *destination* node id to the
*edge id*.  We need to store the edge IDs to reconstruct an Edge.  Other graph
representations use the edge IDs to maintain lists, but here we don't have
that.  The destination is the key of the map for fast edgeExists tests.

-}
