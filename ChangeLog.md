0.2 (2022-05-08)
----------------

- Exported the `vertexId` function to project an `Int` from a `Vertex`, which is useful for conversions to other formats (e.g., graphviz) (@benjaminselfridge)
- Fixed a bug in the `BiDigraph` that prevented parallel edges from being added to the graph (@benjaminselfridge)
- Added `Unbox` instances for `Vertex` and `Edge` (@bielr)
- Added a `Bifunctor` instance for the `PatriciaTree` (@kquick)


0.1.0.0 (2019-12-18)
--------------------

- Initial release
