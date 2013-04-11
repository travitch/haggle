{-# LANGUAGE PatternGuards, FlexibleContexts #-}
module Data.Graph.Haggle.VertexMap (
  -- * Pure interface
  VertexMap,
  emptyVertexMap,
  vertexForLabel,
  lookupVertexForLabel,
  -- * Ref interface
  VertexMapRef,
  newVertexMapRef,
  vertexForLabelRef,
  vertexMapFromRef
  ) where

import Control.Monad ( liftM )
import Control.Monad.Primitive
import Data.Map ( Map )
import qualified Data.Map as M
import Data.PrimRef

import Data.Graph.Haggle

newtype VertexMap nl = VM (Map nl Vertex)

emptyVertexMap :: VertexMap nl
emptyVertexMap = VM M.empty

vertexForLabel :: (PrimMonad m, MLabeledVertex g, Ord (MVertexLabel g))
               => g m
               -> VertexMap (MVertexLabel g)
               -> MVertexLabel g
               -> m (Vertex, VertexMap (MVertexLabel g))
vertexForLabel g vm@(VM m) lbl
  | Just v <- M.lookup lbl m = return (v, vm)
  | otherwise = do
    v <- addLabeledVertex g lbl
    let m' = M.insert lbl v m
    return (v, VM m')

lookupVertexForLabel :: (Ord nl) => nl -> VertexMap nl -> Maybe Vertex
lookupVertexForLabel lbl (VM m) = M.lookup lbl m

newtype VertexMapRef nl m = VMR (PrimRef m (VertexMap nl))

vertexMapFromRef :: (PrimMonad m) => VertexMapRef nl m -> m (VertexMap nl)
vertexMapFromRef (VMR ref) = readPrimRef ref

newVertexMapRef :: (PrimMonad m) => m (VertexMapRef nl m)
newVertexMapRef = liftM VMR $ newPrimRef emptyVertexMap

vertexForLabelRef :: (PrimMonad m, MLabeledVertex g, Ord (MVertexLabel g))
                  => g m
                  -> VertexMapRef (MVertexLabel g) m
                  -> MVertexLabel g
                  -> m Vertex
vertexForLabelRef g (VMR ref) lbl = do
  vm <- readPrimRef ref
  (v, vm') <- vertexForLabel g vm lbl
  writePrimRef ref vm'
  return v


