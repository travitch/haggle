# Overview
![travis-ci status](https://travis-ci.org/travitch/haggle.svg?branch=master)

Haggle is a graph library for Haskell.  It aims to support large graphs
efficiently and compactly.  It differs from
[fgl](http://hackage.haskell.org/package/fgl) in a few ways:
- There are no partial functions in the API
- There are multiple graph representations included
- Mutable graphs can be constructed in IO or ST
- Labels for both nodes and edges are optional

The API is based on typeclasses to allow programs to be written generically and
support multiple graph representations when possible.  Graphs can be mutable,
immutable, or inductive.

The mutable graph support is designed to allow graphs to be efficiently
constructed in a mutable context and then frozen into a pure immutable form.
There are multiple representations of mutable and immutable graphs to support
different use cases (simple graphs, directed graphs, and bidirectional directed
graphs).  The different representations allow users to make decisions on time
and space tradeoffs based on their algorithmic needs.

Inductive graphs (the fgl style) are also supported, while sharing some of the
API with the compact immutable graphs.

See the haddocks in Data.Graph.Haggle for information and examples on using the
API.

# TODO
- Reach feature parity with the algorithms in fgl
- Explore more graph representations
- Explore graph serialization (the use of the opaque `Vertex` type makes it a bit tricky)
