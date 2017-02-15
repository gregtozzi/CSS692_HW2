import_attiro <- function(file) {
  newGraph <- read_graph(file, format = 'Pajek')
  newAdjMat <- newGraph %>% as_adj %>% as.matrix > 0
  newAdjMat <- newAdjMat * 1
  undirectedGraph <- graph_from_adjacency_matrix(newAdjMat) %>% as.undirected(mode = 'collapse')
  return(undirectedGraph)
}

# Define palettes for use in plots
colors_ok_go <- c("#EF4862", "#F275A1", "#F7A08D",
                     "#E84524", "#EAE613", "#8E8FC7",
                     "#6C6E9E", "#3D58A7", "#2ABFC3",
                     "#A0D28B", "#35B44B", "#050608",
                     "#656565", "#D5D5D5", "#14BBE8")

colors_edges <- c(rgb(119/255, 22/255, 255/255),
                     "#FF2851", "#8E8E93")

assign_colors <- function(membership, Palette) {
  # Assigns colors from a palette based on membership
  newMembership <- clean_group(membership)
  Palette[newMembership]
}

plot_Attiro <- function(graph, membership, Palette) {
  # Automates repetitive plotting tasks
  l <-layout.kamada.kawai(graph)
  vertexColors <- assign_colors(membership, Palette)
  plot(graph,
       vertex.label = V(graph)$label,
       vertex.color = vertexColors,
       edge.arrow.size = 0.3,
       vertex.label.family = "Futura Medium",
       vertex.label.color = 'white',
       vertex.frame.color = 'white',
       vertex.label.cex = 0.5,
       layout = l)
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