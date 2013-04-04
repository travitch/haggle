module Data.Graph.Haggle.LabelAdapter (
  LabeledMGraph,
  LabeledGraph,
  newLabeledGraph,
  newSizedLabeledGraph,
  addLabeledVertex,
  addLabeledEdge,
  getVertexLabel,
  getEdgeLabel,
  getSuccessors,
  getOutEdges,
  countVertices,
  countEdges,
  getPredecessors,
  getInEdges,
  freeze
  ) where

import Data.Graph.Haggle.Internal.Adapter
