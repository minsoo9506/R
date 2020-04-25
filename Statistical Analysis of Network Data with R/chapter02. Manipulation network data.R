# chapter1 ----
library(sand)
library(igraph)

g = graph.formula(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6, 4-7, 5-6, 6-7)
V(g)
E(g)
print_all(g)
plot(g)

dg = graph.formula(1-+2, 1-+3, 2-+3)
print_all(dg)
plot(dg)

# vertex name
V(dg)$name
V(dg)$name = c("Sam", "Mary", "Tom")
print_all(dg)
plot(dg)

dg = graph.formula(Sam-+Mary, Sam-+Tom, Mary-+Tom)

# Representation for graph
E(dg)               # adjacency list
get.edgelist(dg)    # edgelist
get.adjacency(dg)   # adjacency matrix

# subgraph
sub = induced.subgraph(g, 1:5)
print_all(sub)

sub = g - vertices(c(6,7))
print_all(sub)

#
sub = sub + vertices(c(6,7))
plot(sub)
sub  = sub + edges(c(6,7),c(4,7))
plot(sub)

# deco network graph
V(dg)$name
V(dg)$sex = c("M","F","M") # attribute를 맘대로 만들수 있다 
print_all(dg)

is.weighted(g)
wg = g
E(wg)$weight = runif(ecount(wg))
E(wg)$weight
is.weighted(wg)

# data frame
g.lazega = graph.data.frame(elist.lazega, directed="FALSE", vertices=v.attr.lazega)
vcount(g.lazega)
ecount(g.lazega)
list.vertex.attributes(g.lazega)
plot(g.lazega)

# basic graph
is.simple(g)
not.simple = g + edges(c(2,3)) # multi edges
is.simple(not.simple)
plot(not.simple)

E(not.simple)$weight = 1
simpled = simplify(not.simple) # multi edge가 지워졌다 by weight = 1
plot(simpled)

neighbors(g, 5)

degree(g)
degree(dg, mode="in")
degree(dg, mode="out")

is.connected(g)
is.connected(dg, mode="strong")

# special graph
g.full = graph.full(7)
plot(g.full)
g.ring = graph.ring(7)
plot(g.ring)
g.tree = graph.tree(7, children=2, mode="undirected")
plot(g.tree)
g.ring = graph.ring(7)
plot(g.ring)