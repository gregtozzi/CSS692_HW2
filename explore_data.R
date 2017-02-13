require(igraph)
require(magrittr)
source('helper_functions.R')

Attiro <- read_graph('Attiro.graphml',
                     format = 'graphml')

Attiro <- as.directed(Attiro)

Attiro_Grouping <- read.csv('Attiro_Grouping.csv')
V(Attiro)$label <- Attiro_Grouping$Label %>% as.character
Group <- Attiro_Grouping$group
V(Attiro)$group <- Group
Palette <- character()
for(i in 1:length(unique(Group))) {
  Palette[which(Group == unique(Group)[i])] <- colors_ok_go[i]
}
V(Attiro)$color <- unlist(Palette)
E(Attiro)$type <- E(Attiro)$weight
delete_edge_attr(Attiro, 'weight')

plot(Attiro,
     vertex.label = V(Attiro)$label,
     edge.color = E(Attiro)$type,
     edge.arrow.size = 0.3,
     vertex.label.family = "Futura Medium",
     vertex.label.color = 'white',
     vertex.frame.color = 'white')

GN <- cluster_edge_betweenness(Attiro, directed = FALSE)

Attiro_undirected <- as.undirected(Attiro, mode = 'collapse')
FG <- cluster_fast_greedy(Attiro_undirected)
FG_mem <- membership(FG)
