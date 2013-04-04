module Data.Graph.Haggle.LabelAdapter (
  LabeledMGraph,
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
  ) where

import Data.Graph.Haggle.Internal.Adapter
