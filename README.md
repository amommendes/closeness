## Closeness Centrality of a Vertex

The aim of this code is implement a simples algorithm to extract a metric called "closeness centrality" from a social network graph.

Centrality metrics try to approximate a measure of influence of an individual within a social network (see details [here](https://en.wikipedia.org/wiki/Centrality). The distance between any two vertices is their shortest path. The *farness* of a given vertex *v* is the sum of all distances from each vertex to *v*. Finally, the *closeness* of a vertex *v* is the inverse of the *farness*.

Graph is representend by a matrix of edges between nodes. It is a 995x2 matrix, which each line correspond to vertex names separated by
a single space, representing an edge between those two nodes.

