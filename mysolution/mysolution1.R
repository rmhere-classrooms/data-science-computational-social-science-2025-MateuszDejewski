library(igraph)

g <- erdos.renyi.game(n = 100, p.or.m = 0.05)


summary(g) #graph not weighted IGRAPH 38abe70 U---

#listing nodes and edges
V(g)
E(g)

E(g)$weight <- runif(length(E(g)), 0.01, 1)

summary(g)  #graph weighted IGRAPH 38abe70 U-W- ( W in "code" indicates it also has attr:  weight (e/n))

##printing and drawing Nodes degree
deg <- degree(g)
deg
hist(deg, xlab = "Degree", col = "lightblue")

#getting  number od clusters
cl <- clusters(g)
cl$no 

#ploting graph
pr <- page.rank(g)$vector

plot(g, vertex.size = pr * 500,      
  vertex.label = NA, edge.arrow.size = 0.2,
)