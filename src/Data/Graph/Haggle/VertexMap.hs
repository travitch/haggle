{-# LANGUAGE PatternGuards, FlexibleContexts #-}
-- | This is a simple module to handle a common pattern: constructing graphs
-- where vertex labels map uniquely to vertices.
--
-- The primary functions in this module are 'vertexForLabel' and
-- 'vertexForLabelRef', which take a vertex label and return the 'Vertex' for
-- that label (allocating a new 'Vertex') if necessary.  The first of those
-- functions explicitly threads the mapping as inputs and outputs.  The second
-- manages a mutable ref side-by-side with the underlying graph.
--
-- After the graph is fully constructed, this mapping is often still useful.
module Data.Graph.Haggle.VertexMap (
  -- * Pure interface
  VertexMap,
  emptyVertexMap,
  vertexForLabel,
  lookupVertexForLabel,
  vertexMapFromGraph,
  -- * Ref interface
  VertexMapRef,
  newVertexMapRef,
  vertexForLabelRef,
  vertexMapFromRef ) where

import qualified Control.DeepSeq as DS
import Control.Monad ( liftM )
import qualified Control.Monad.Primitive as P
import qualified Control.Monad.Ref as R
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Tuple ( swap )

import Data.Graph.Haggle.Classes

-- | A simple mapping from labels to their 'Vertex'
newtype VertexMap nl = VM (Map nl Vertex)

instance (DS.NFData nl) => DS.NFData (VertexMap nl) where
  rnf (VM m) = m `DS.deepseq` ()

emptyVertexMap :: VertexMap nl
emptyVertexMap = VM M.empty

-- | > (v, m') <- vertexForLabel g m lbl
--
-- Looks up the 'Vertex' for @lbl@ in @g@.  If no 'Vertex' in @g@ has that
-- label, a new 'Vertex' is allocated and returned.  The updated vertex
-- mapping @m'@ is returned, too.
vertexForLabel :: (MLabeledVertex g, Ord (MVertexLabel g), P.PrimMonad m, R.MonadRef m)
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

-- | A pure lookup to convert a 'Vertex' label into a 'Vertex'.  If the
-- label is not in the graph, returns 'Nothing'.
lookupVertexForLabel :: (Ord nl) => nl -> VertexMap nl -> Maybe Vertex
lookupVertexForLabel lbl (VM m) = M.lookup lbl m

-- | Build a 'VertexMap' from a 'Graph' with 'Vertex' labels.
vertexMapFromGraph :: (HasVertexLabel g, Ord (VertexLabel g))
                   => g -> VertexMap (VertexLabel g)
vertexMapFromGraph = VM . M.fromList . map swap . labeledVertices

-- | A 'VertexMap' wrapped up in a mutable ref for possibly
-- easier access in 'vertexMapFromRef'.
newtype VertexMapRef nl m = VMR (R.Ref m (VertexMap nl))

-- | Extract the pure 'VertexMap' from the mutable ref.  This is useful
-- to retain the mapping after the graph is fully constructed.
vertexMapFromRef :: (P.PrimMonad m, R.MonadRef m) => VertexMapRef nl m -> m (VertexMap nl)
vertexMapFromRef (VMR ref) = R.readRef ref

-- | Allocate a new 'VertexMap' buried in a mutable ref.
newVertexMapRef :: (P.PrimMonad m, R.MonadRef m) => m (VertexMapRef nl m)
newVertexMapRef = liftM VMR $ R.newRef emptyVertexMap

-- | Just like 'vertexForLabel', but holding the mapping in a ref instead
-- of threading it.  Usage is simpler:
--
-- > v <- vertexForLabelRef g m lbl
vertexForLabelRef :: (MLabeledVertex g, Ord (MVertexLabel g), P.PrimMonad m, R.MonadRef m)
                  => g m
                  -> VertexMapRef (MVertexLabel g) m
                  -> MVertexLabel g
                  -> m Vertex
vertexForLabelRef g (VMR ref) lbl = do
  vm <- R.readRef ref
  (v, vm') <- vertexForLabel g vm lbl
  R.writeRef ref vm'
  return v


