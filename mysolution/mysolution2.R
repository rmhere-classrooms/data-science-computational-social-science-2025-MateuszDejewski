library(igraph)


g <- barabasi.game(1000)


plot(g, layout = layout.fruchterman.reingold(g), vertex.size = 2, vertex.label = NA)


b <- betweenness(g)
central_node <- which.max(b)
central_node


diam <- diameter(g)
diam



# Grafy  Barabási-Albert zazwyczaj dodają nowe węzły do tych z największą liczbą połączeń, przez co tworzą się "huby"
# Widać to na wygenerowanym grafie połączenia zbiegają do środka do kilu węzłów i dopiero te się łączą. 
# Natomiast w grafach Erdős-Rényi jest to w pełni losowe i tworzy się "siatka" z wiloma połączeniami między węzłami. 