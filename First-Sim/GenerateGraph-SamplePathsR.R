
library(igraph)
set.seed(154)
generate_random_paths <- function(g, source_node, nodes, n_paths) {
  random_paths <- lapply(1:n_paths, function(v) {
    path <- NULL
    while (is.null(path) || length(path) != 2) {
      if (runif(1) < 0.7) {
        # Start from the source node
        path <- random_walk(g, start = source_node, steps = 1, mode = "out")
      } else {
        # choosing by different probabilities
        #probabilities <- numeric(length(nodes))
        ## while using 10000 nodes
        #probabilities[1:3000] <- 0.8
        #probabilities[3001:5000] <- 0.1
        #probabilities[5001:length(nodes)] <- 0.4
        ## while using 5000 nodes
        #probabilities[1:1000] <- 0.8
        #probabilities[1001:3000] <- 0.1
        #probabilities[3001:length(nodes)] <- 0.4
        # Normalize the probabilities so that they sum up to 1
        #probabilities <- probabilities / sum(probabilities)
        #selected_node <- sample(nodes, size = 1, prob = probabilities)
        # choosing by same probability
        selected_node <- sample(nodes, size = 1)
        path <- random_walk(g, start = selected_node, steps = 1, mode = "out")
      }
    }
    path
  })
  random_paths
}


# Create a simple directed graph with one edge between each pair of nodes (no self-loops)
n <- 10000 # or 5000
nodes <- 1:n
edges <- data.frame(from = sample(nodes, size = 2*n, replace = TRUE),
                    to = sample(nodes, size = 2*n, replace = TRUE))
edges <- edges[edges$from != edges$to, ] # Remove self-loops
edges <- unique(edges)  # Remove duplicate edges
g <- graph_from_data_frame(edges, directed = TRUE)

# Add a source node
source_node <- max(V(g)) + 1
g <- add_vertices(g, nv = 1, name = "other")

# Connect the source node to selected existing nodes with directed edges
target_nodes <- sample(1:max(V(g)), size = n/2, replace = FALSE)  # Choose half random nodes to connect to
for (target_node in target_nodes) {
  g <- add_edges(g, c(source_node, target_node))
}

# Create data frames for each month
start_date <- as.Date("2012-01-01")  # Start date
end_date <- as.Date("2024-01-01")    # End date
dates <- seq(start_date, end_date, by = "month")  # Monthly dates

# Initialize a list to store data frames for each month

path_df_list <- list()
# Loop over each month
for (i in seq_along(dates)) {
  random_paths <- generate_random_paths(g, source_node, 1:max(V(g)), 20000)
  random_paths_names <- c(names(unlist( random_paths)))
  # Combine elements two by two
  combined_pairs <- paste(random_paths_names[seq(1, length(random_paths_names), by = 2)], random_paths_names[seq(2, length(random_paths_names), by = 2)], sep = "::")
  # Count the combined pairs
  path_counts <-table(combined_pairs)
  path_df <- data.frame(path = names(path_counts), frequency = as.numeric(path_counts))
  # Add the date column
  path_df$date <- dates[i]
  # Append the path_df dataframe to the list
  path_df_list[[i]] <- path_df
}

# Stack the data frames for each month into one large data frame
combined_df <- do.call(rbind, path_df_list)
# Load required libraries
library(tidyr)
library(dplyr)

# Convert 'date' column to Date type
combined_df$date <- as.Date(combined_df$date)

# Create a full combination of paths and dates
full_combination <- expand.grid(path = unique(combined_df$path), 
                                date = seq(min(combined_df$date), max(combined_df$date), by = "month"))

# Merge the full combination with the original dataset
complete_df <- full_combination %>%
  left_join(combined_df, by = c("path", "date")) %>%
  mutate(frequency = ifelse(is.na(frequency), 0, frequency))%>% # Replace missing values with 0
  arrange(path) # Order by path ID


complete_df <- complete_df %>%
  group_by(path) %>%
  filter(mean(frequency == 0) <= 0.8)



