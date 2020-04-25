library(igraph)
library(sand)

g.1 = graph.lattice(c(5,5,5)) # lattice

data(aidsblog)
aidsblog = upgrade_graph(aidsblog) # blog network

# Graph layout

# circular layout
igraph.options(vertex.size=3, vertex.label=NA, edge.arrow.size=0.2)
par(mfrow=c(1,2))
plot(g.1, layout=layout.circle) 
title("5*5*5 Lattice")
plot(aidsblog, layout=layout.circle)
title("Blog Network")

# frunchterman.reingold
igraph.options(vertex.size=3, vertex.label=NA, edge.arrow.size=0.1)
par(mfrow=c(1,2))
plot(g.1, layout=layout.fruchterman.reingold)
title("5*5*5 Lattice")
plot(aidsblog, layout=layout.fruchterman.reingold)
title("Blog Network")

# Energy-placement Methods
## kamada.kawai
igraph.options(vertex.size=3, vertex.label=NA, edge.arrow.size=0.1)
par(mfrow=c(1,2))
plot(g.1, layout=layout.kamada.kawai)
title("5*5*5 Lattice")
plot(aidsblog, layout=layout.kamada.kawai)
title("Blog Network")

g.tree = graph.formula(1-+2,1-+3,1-+4,2-+5,2-+6,2-+7,3-+8,3-+9,4-+10)
par(mfrow=c(1,3))
igraph.options(vertex.size=30, vertex.label=NA, edge.arrow.size=0.1)
plot(g.tree, layout=layout.circle)
plot(g.tree, layout=layout.reingold.tilford(g.tree, circular=T))
plot(g.tree, layout=layout.reingold.tilford)

g.bip = upgrade_graph(g.bip)
V(g.bip)$type
par(mfrow=c(1,1))
plot(g.bip, layout=-layout.bipartite(g.bip)[,2:1], vertex.label=V(g.bip)$name,
     vertex.size=30, vertex.shape=ifelse(V(g.bip)$type,"rectangle","circle"),
     vertex.color=ifelse(V(g.bip)$type,"red","cyan"))

# deco graph layouts
library(igraphdata)
data("karate")
set.seed(2014122022)
l = layout.kamada.kawai(karate)
par(mfrow=c(1,2))
plot(karate, layout=l, vertex.label=NA)

V(karate)$label = sub("Actor ","",V(karate)$name)

V(karate)$shape = "circle"
V(karate)[c("Mr Hi", "John A")]$shape = "rectangle"

V(karate)[Faction == 1]$color = "red"
V(karate)[Faction == 2]$color = "dodgerblue"

V(karate)$size = 4*sqrt(graph.strength(karate))
V(karate)$size2 = V(karate)$size * .5

E(karate)$width = E(karate)$weight

F1 = V(karate)[Faction == 1]
F2 = V(karate)[Faction == 2]
E(karate)[F1 %--% F1]$color = "pink"
E(karate)[F2 %--% F2]$color = "lightblue"
E(karate)[F1 %--% F2]$color = "yellow"

V(karate)$label.dist = ifelse(V(karate)$size >= 10, 0, 0.75)

plot(karate, layout=l)

data("lazega")
lazega = upgrade_graph(lazega)
colbar = c("red","dodgerblue","goldenrod")
v.colors = colbar[V(lazega)$Office]
v.shapes = c("ciricle","square")[V(lazega)$Practice]
v.size = 3.5*sqrt(V(lazega)$Years)
set.seed(2014122022)
l = layout.fruchterman.reingold(lazega)
par(mfrow=c(1,1))
plot(lazega, layout=l, vertex.color=v.colors, vertex.shapes=v.shapes, vertex.size=v.size,
     vertex.label=NA)

# Visualizing Large Network
summary(fblog)
fblog = upgrade_graph(fblog)

party.names = sort(unique(V(fblog)$PolParty))
party.names

set.seed(2014122022)
l = layout.kamada.kawai(fblog)
party.nums.f = as.factor(V(fblog)$PolParty)
party.nums = as.numeric(party.nums.f)
plot(fblog, layout=l, vertex.label=NA, vertex.color=party.nums, vertex.size=3)

set.seed(2014122022)
l = layout.drl(fblog)
plot(fblog, layout=l, vertex.size=5, vertex.label=NA, vertex.color=party.nums)

fblog.c = contract.vertices(fblog, party.nums)
E(fblog.c)$weight = 1
fblog.c = simplify(fblog.c)
party.size = as.vector(table(V(fblog)$PolParty))
plot(fblog.c, vertex.size=5*sqrt(party.size), vertex.label=party.names,
     vertex.color=V(fblog.c), edge.width=sqrt(E(fblog.c)$weight),
     vertex.label.dist=1.5, edge.arrow.size=0)

k.nbhds = graph.neighborhood(karate, order=1)
sapply(k.nbhds, vcount)

k.nbhds

k.1 = k.nbhds[[1]]
k.34 = k.nbhds[[34]]
par(mfrow=c(1,2))
plot(k.1, vertex.label=NA, vertex.color=c("red", rep("lightblue", 16)))
plot(k.34, vertex.label=NA, vertex.color=c(rep("lightblue", 17), "red"))
