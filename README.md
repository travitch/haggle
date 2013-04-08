Haggle is another graph library for Haskell.  It aims to
support large graphs efficiently and compactly.  It differs
from [fgl](http://hackage.haskell.org/package/fgl) in that
it does not use inductive graphs.  Instead, graphs are built
using mutation in the IO or ST monads and then frozen into
immutable forms for processing.

This library is more restrictive than fgl, but provides
more compact (and hopefully faster) implementations.  See
the haddocks in Data.Graph.Haggle for much more information
and examples.
