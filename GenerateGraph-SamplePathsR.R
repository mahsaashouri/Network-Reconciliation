

library(igraph)

# Create a simple directed graph with one edge between each pair of nodes (no self-loops)
nodes <- 1:10
edges <- data.frame(from = sample(nodes, size = 20, replace = TRUE),
                    to = sample(nodes, size = 20, replace = TRUE))
edges <- edges[edges$from != edges$to, ] # Remove self-loops
edges <- unique(edges)  # Remove duplicate edges
g <- graph_from_data_frame(edges, directed = TRUE)

# Add a source node
source_node <- max(V(g)) + 1
g <- add_vertices(g, nv = 1, name = "other")

# Connect the source node to selected existing nodes with directed edges
target_nodes <- sample(nodes, size = 5)  # Choose 5 random nodes to connect to
for (target_node in target_nodes) {
  g <- add_edges(g, c(source_node, target_node))
}
plot(g, edge.arrow.size=0.5)
# Simulate random paths from selected nodes (excluding the source node)
random_paths <- lapply(1:10, function(v) {
  if (runif(1) < 0.5) {
    # Start from the source node
    random_walk(g, start = source_node, steps  = sample(2:10, 1), mode = "out")
  } else {
    # Start from a randomly selected node (excluding the source node)
    v <- sample(nodes, size = 1)
    random_walk(g, start = v, steps  = sample(2:10, 1), mode = "out")
  }
})


