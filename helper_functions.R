# Define palettes for use in plots

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
  # Assigns colors from a palette based on membership
  sapply(membership, function(x) Palette[[x]])
}

plot_Attiro <- function(graph, vertexColors) {
  # Automates repetitive plotting tasks
  plot(graph,
       vertex.label = V(graph)$label,
       vertex.color = vertexColors,
       edge.arrow.size = 0.3,
       vertex.label.family = "Futura Medium",
       vertex.label.color = 'white',
       vertex.frame.color = 'white',
       vertex.label.cex = 0.5)
}

Modularity <- function(graph, Group) {
  # A hand-check of the modularity calcuations in igraph.
  # Currently on works if membership is exclusive (i.e. no
  # node is a member of more than one group).
  
  require(psych) # Need this for the trace function
  
  # Force the graph to be undirected
  undirectedGraph <- as.undirected(graph, mode = 'collapse')
  
  # Compute the constants - number of rows, number of columns,
  # and number of groups
  n <- undirectedGraph %>% V %>% length
  m <- undirectedGraph %>% E %>% length
  r <- Group %>% unique %>% length
  
  # Compute the adjacency matrix and the degree matricies
  A <- as_adj(undirectedGraph)
  k <- degree(undirectedGraph)
  kv <- k %>% rep(n); dim(kv) <- c(n, n)
  kw <- t(kv)
  
  # Compute the modularity matrix and membership matrix
  B <- A - kv * kw / 2 / m
  S <- numeric(n * r); dim(S) <- c(n, r)
  CG <- clean_group(Group)
  for(node in 1:n) {
    S[node, CG[node]] <- 1
  }
  
  # Compute modularity
  matrixProduct <- t(S) %*% B %*% S %>% as.matrix
  1 / 2 / m * tr(matrixProduct)
}

clean_group <- function(Group) {
  # Converts non-sequential group labels into sequential,
  # 1-indexed integers
  
  uniqueGroup <- Group %>% unique
  sapply(Group, function(x) which(x == unique(Group)))
}