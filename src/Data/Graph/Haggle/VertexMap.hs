{-# LANGUAGE PatternGuards, FlexibleContexts #-}
module Data.Graph.Haggle.VertexMap (
  VertexMap,
  emptyVertexMap,
  vertexForLabel
  ) where

import Control.Monad.Primitive
import Data.Map ( Map )
import qualified Data.Map as M

import Data.Graph.Haggle

newtype VertexMap nl = VM (Map nl Vertex)

emptyVertexMap :: VertexMap nl
emptyVertexMap = VM M.empty

vertexForLabel :: (PrimMonad m, MLabeledVertex g, Ord (MVertexLabel g))
               => g m
               -> MVertexLabel g
               -> VertexMap (MVertexLabel g)
               -> m (Vertex, VertexMap (MVertexLabel g))
vertexForLabel g lbl vm@(VM m)
  | Just v <- M.lookup lbl m = return (v, vm)
  | otherwise = do
    v <- addLabeledVertex g lbl
    let m' = M.insert lbl v m
    return (v, VM m')
