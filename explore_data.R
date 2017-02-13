require(igraph)
require(magrittr)

Attiro <- read_graph('Attiro.net',
                     format = 'pajek')
Attiro_Grouping <- read.csv('Attiro_Grouping.csv')
V(Attiro)$label <- Attiro_Grouping$Label %>% as.character
V(Attiro)$group <- Attiro_Grouping$group

plot(Attiro,
     vertex.color = V(Attiro)$group,
     vertex.label = V(Attiro)$label,
     edge.color = E(Attiro)$weight)

GN <- cluster_edge_betweenness(Attiro,
                               weights = NULL)

write_graph(Attiro, 'newAttiro', format = 'pajek')