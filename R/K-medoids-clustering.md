# K-Medoids Clustering in R

Here's a complete example of implementing K-Medoids clustering using the `cluster` package in R:

```r
# Install and load required packages
install.packages(c("cluster", "factoextra", "ggplot2"))
library(cluster)
library(factoextra)
library(ggplot2)

# Generate sample data
set.seed(123)
data <- data.frame(
  x = c(rnorm(30, 2, 0.5), rnorm(30, 6, 0.8), rnorm(30, 3, 1)),
  y = c(rnorm(30, 2, 0.5), rnorm(30, 6, 0.8), rnorm(30, 3, 1))
)

# View the first few rows of data
head(data)

# Perform K-Medoids clustering
# Using the PAM (Partitioning Around Medoids) algorithm
k <- 3
medoids_result <- pam(data, k, metric = "euclidean")

# Display clustering results
print(medoids_result)

# View cluster assignments
print(medoids_result$clustering)

# View the medoids (representative objects)
print(medoids_result$medoids)

# Visualize the clustering results
fviz_cluster(medoids_result, 
             data = data,
             ellipse = TRUE,
             palette = c("#2E9FDF", "#00AFBB", "#FC4E07"),
             ggtheme = theme_bw(),
             main = "K-Medoids Clustering Results")

# Alternative visualization using base R
plot(data$x, data$y, 
     col = medoids_result$clustering, 
     pch = 19, 
     xlab = "X", 
     ylab = "Y",
     main = "K-Medoids Clustering")

# Add medoids to the plot
points(medoids_result$medoids, col = 1:3, pch = 8, cex = 2)

# Calculate clustering quality metrics
# Silhouette analysis
silhouette_scores <- silhouette(medoids_result$clustering, dist(data))
avg_silhouette <- mean(silhouette_scores[, 3])
cat("Average Silhouette Score:", round(avg_silhouette, 3), "\n")

# Compare different numbers of clusters using the elbow method
# For this example, we'll use the Calinski-Harabasz index
ch_scores <- c()
for(i in 2:6) {
  pam_result <- pam(data, i)
  ch_scores[i] <- pam_result$cs
}

# Plot CH scores to find optimal k
plot(2:6, ch_scores, type = "b", 
     xlab = "Number of Clusters (k)", 
     ylab = "Calinski-Harabasz Index",
     main = "Optimal Number of Clusters")

# Print CH scores
cat("Calinski-Harabasz scores for k=2:6\n")
for(i in 2:6) {
  cat("k =", i, ":", round(ch_scores[i], 2), "\n")
}

# Summary of the clustering
cat("\nClustering Summary:\n")
cat("Number of clusters:", k, "\n")
cat("Number of observations:", length(medoids_result$clustering), "\n")
cat("Within-cluster sum of squares:", round(medoids_result$tot.withinss, 2), "\n")
cat("Between-cluster sum of squares:", round(medoids_result$betweenss, 2), "\n")
```

## Key Features of This Implementation:

1. **Data Generation**: Creates sample 2D data with 3 natural clusters
2. **K-Medoids Algorithm**: Uses PAM (Partitioning Around Medoids) method
3. **Visualization**: Shows both base R and factoextra plots
4. **Quality Assessment**: Includes silhouette analysis and CH index
5. **Optimal K Selection**: Demonstrates how to find the optimal number of clusters

## Output Interpretation:

- **Medoids**: The actual data points that represent each cluster
- **Clustering**: Assignment of each data point to a cluster
- **Silhouette Score**: Measures how similar objects are to their own cluster compared to other clusters
- **Calinski-Harabasz Index**: Higher values indicate better clustering

The K-Medoids algorithm is particularly useful because it's more robust to outliers than K-Means since it uses actual data points as medoids rather than calculating means.

