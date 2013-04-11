module Data.Graph.Haggle.LabelAdapter (
  -- * Types
  LabeledMGraph,
  LabeledGraph,
  -- * Mutable Graph API
  newLabeledGraph,
  newSizedLabeledGraph,
  -- * Immutable Graph API
  mapEdgeLabel,
  mapVertexLabel,
  fromLabeledEdgeList,
  ) where

import Data.Graph.Haggle.Internal.Adapter
