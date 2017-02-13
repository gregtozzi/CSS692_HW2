colors_ok_go <- list(rgb(2/255, 66/255, 249/255),
                     rgb(232/255, 46/255, 20/255),
                     rgb(61/255, 182/255, 98/255),
                     rgb(252/255, 209/255, 10/255),
                     rgb(0/255, 214/255, 251/255),
                     rgb(247/255, 71/255, 13/255),
                     rgb(119/255, 22/255, 255/255),
                     "#B7CB96", "#AF9979", "#827711", 'black',
                     "#F29525", "#B9177D", "brown")

colors_edges <- c(rgb(119/255, 22/255, 255/255),
                     "#FF2851", "#8E8E93")

assign_colors <- function(membership, Palette) {
  sapply(membership, function(x) Palette[[x]])
}

plot_Attiro <- function(graph, vertexColors) {
  plot(graph,
       vertex.label = V(graph)$label,
       vertex.color = vertexColors,
       edge.arrow.size = 0.3,
       vertex.label.family = "Futura Medium",
       vertex.label.color = 'white',
       vertex.frame.color = 'white',
       vertex.label.cex = 0.5)
}