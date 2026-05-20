# DBSCAN Clustering in R

Here's a complete example of implementing DBSCAN clustering in R using the `fpc` and `dbscan` packages:

```r
# Install and load required packages
install.packages(c("fpc", "dbscan", "ggplot2", "cluster"))
library(fpc)
library(dbscan)
library(ggplot2)
library(cluster)

# Generate sample data
set.seed(123)
# Create 3 clusters with some noise
cluster1 <- data.frame(x = rnorm(50, 2, 0.5), y = rnorm(50, 2, 0.5))
cluster2 <- data.frame(x = rnorm(50, 6, 0.5), y = rnorm(50, 6, 0.5))
cluster3 <- data.frame(x = rnorm(50, 2, 0.5), y = rnorm(50, 6, 0.5))
noise <- data.frame(x = runif(20, 0, 8), y = runif(20, 0, 8))

# Combine all data
data <- rbind(cluster1, cluster2, cluster3, noise)
head(data)

# Visualize the raw data
ggplot(data, aes(x = x, y = y)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "Original Data with Clusters", x = "X", y = "Y") +
  theme_minimal()

# Apply DBSCAN clustering
# First, we need to determine appropriate eps and minPts values
# Let's try different parameter combinations

# Method 1: Using the kNN distance plot to find eps
kNNdistplot(data, k = 4)  # k=4 because we expect 3 clusters + 1 for noise
abline(h = 0.5, lty = 2)   # This suggests eps = 0.5

# Apply DBSCAN with eps = 0.5 and minPts = 4
dbscan_result <- dbscan(data, eps = 0.5, minPts = 4)

# View the clustering results
print(dbscan_result)

# Plot the clustering results
plot(dbscan_result, data, main = "DBSCAN Clustering Results")
points(dbscan_result$cluster, col = c("black", "red", "blue", "green", "orange"))

# Alternative approach using the fpc package
# Calculate the optimal eps using the elbow method
# This is more complex but gives better results

# Using the fpc package for more detailed analysis
# Convert data to matrix format
data_matrix <- as.matrix(data)

# Perform DBSCAN using fpc
# Note: fpc uses different parameter names
dbscan_fpc <- dbscan(data_matrix, eps = 0.5, MinPts = 4)

# Visualize results with colors
cluster_colors <- c("black", "red", "blue", "green", "orange", "purple")
plot(data_matrix, col = cluster_colors[dbscan_fpc$cluster + 1], 
     pch = 19, cex = 0.8,
     main = "DBSCAN Clustering - fpc package",
     xlab = "X", ylab = "Y")

# Add cluster centers (if needed)
# Get unique cluster IDs
unique_clusters <- unique(dbscan_fpc$cluster)
for(i in unique_clusters[unique_clusters >= 0]) {  # Skip noise points
  cluster_points <- data_matrix[dbscan_fpc$cluster == i, ]
  center <- colMeans(cluster_points)
  points(center[1], center[2], col = cluster_colors[i + 1], pch = 4, cex = 2, lwd = 2)
}

# Detailed summary of clusters
cat("Number of clusters found:", max(dbscan_fpc$cluster) + 1, "\n")
cat("Number of noise points:", sum(dbscan_fpc$cluster == -1), "\n")

# Create a more detailed visualization with ggplot2
data_with_clusters <- data
data_with_clusters$cluster <- dbscan_fpc$cluster

ggplot(data_with_clusters, aes(x = x, y = y, color = factor(cluster))) +
  geom_point(size = 2) +
  labs(title = "DBSCAN Clustering Results", 
       x = "X", y = "Y", 
       color = "Cluster") +
  theme_minimal() +
  scale_color_manual(values = c("black", "red", "blue", "green", "orange", "purple"))

# Evaluate clustering quality (if ground truth is available)
# For this example, we'll just show the cluster assignments
cat("Cluster assignments:\n")
table(dbscan_fpc$cluster)

# Calculate some basic statistics
cat("\nCluster sizes:\n")
cluster_sizes <- table(dbscan_fpc$cluster)
print(cluster_sizes)
```

## Key Parameters Explained:

- **eps (ε)**: The maximum distance between two points for them to be considered as in the same neighborhood
- **minPts**: The number of points in a neighborhood for a point to be considered as a core point

## Key Features of DBSCAN:

1. **Noise Detection**: Automatically identifies outliers as noise points (cluster = -1)
2. **Arbitrary Shape**: Can find clusters of any shape, not just spherical
3. **No Pre-specified Number of Clusters**: Unlike k-means, you don't need to specify the number of clusters beforehand
4. **Robust to Outliers**: Handles noise well in the data

## Output Interpretation:

- **Cluster = 0, 1, 2, ...**: Points belonging to different clusters
- **Cluster = -1**: Noise points (outliers)
- The algorithm will automatically determine the number of clusters based on the data structure

This example demonstrates how DBSCAN can effectively identify clusters in data with varying densities and automatically detect noise points.

