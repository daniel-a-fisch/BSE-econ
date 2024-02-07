#  Network Measures (Facebook100)
#  26.01.2024
# 
#  Daniel Fisch
# 
#  In this script I analyse the small world phenomenon in the Facebook100 data set.
#  I do this by comparing diameter and mean distances within the network as of
#  2005 for different universities.


#install.packages("igraph")
#install.packages("R.matlab")
#install.packages("tidyverse")

library(igraph)
library(R.matlab)
library(ggplot2)

# Replace "path/to/facebook100" with the actual path to your "facebook100" folder
folder_path <- "C:/.../facebook100"

# Get a list of all .mat files in the folder
mat_files <- list.files(folder_path, pattern = "\\.mat$", full.names = TRUE)
mat_files <- mat_files

# Initialize empty arrays for diameter, mean distance, network size and giant comp (GC) size
diameters <- numeric(length(mat_files))
mean_distances <- numeric(length(mat_files))
net_size <- numeric(length(mat_files))
GC_size <- numeric(length(mat_files))

# Insert loop later
for (i in seq_along(mat_files)) {

# Read in data
mat_file_path <- mat_files[i]
mat_data <- readMat(mat_file_path)

# Define adjecancy matrix and additional information matrix
A <- mat_data$A
info <- mat_data$local.info

# Create an igraph graph from the adjacency matrix
graph <- graph.adjacency(A, mode = "undirected")

# Find the connected components in the graph, identify the largest connected component
# Get the vertices of the largest connected component and create the largest connected component subgraph

# Find the connected components
components <- components(graph)

# Create subgraph of the larges
largest_component_index <- which.max(components$csize)
GC = induced_subgraph(graph, which(components$membership == largest_component_index))


# Compute the diameter and mean distance of the largest connected component
diameters[i] <- diameter(GC)
mean_distances[i] <- mean_distance(GC)

# Print the results - check
#cat("Diameter of the largest component:", diameters[i], "\n")
#cat("Mean distance in the largest component:", mean_distances[i], "\n")

# Store size of GC and network
GC_size[i] <- max(components$csize)
net_size[i] <- length(graph)

}


df <- data.frame(n = net_size, n_GC = GC_size, diameter = diameters, mean_distance = mean_distances)

# Plot GC diameter vs network size
ggplot(df, aes(x = n, y = diameter)) +
  geom_point() +
  #geom_line() +
  labs(title = "Diameters vs network size", x = "Number of nodes in network", y = "Diameter of giant component")
# Plot mean distances vs size GC
ggplot(df, aes(x = n_GC, y = mean_distance)) +
  geom_point() +
  #geom_line() +
  labs(title = "Mean distance vs size giant component", x = "Number of nodes in giant component", y = "Mean distance in giant component")
