# K-Medoids Clustering in R

Here's a complete example of implementing K-medoids clustering using the `cluster` package in R:

```r
# Install and load required packages
install.packages(c("cluster", "factoextra"))
library(cluster)
library(factoextra)

# Generate sample data
set.seed(123)
data <- data.frame(
  x = c(rnorm(30, 2, 0.5), rnorm(30, 6, 0.8), rnorm(30, 3, 0.6)),
  y = c(rnorm(30, 2, 0.5), rnorm(30, 6, 0.8), rnorm(30, 3, 0.6))
)

# View the first few rows of data
head(data)

# Perform K-medoids clustering
# Using PAM (Partitioning Around Medoids) algorithm
k <- 3  # number of clusters
medoids_result <- pam(data, k, metric = "euclidean")

# Display results
print(medoids_result)

# View cluster assignments
print(medoids_result$clustering)

# View the medoids (representative objects)
print(medoids_result$medoids)

# Visualize the clusters
fviz_cluster(medoids_result, 
             data = data,
             ellipse = TRUE,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
             ggtheme = theme_bw(),
             main = "K-Medoids Clustering Results")

# Calculate cluster statistics
summary(medoids_result)

# Compare with K-means for comparison
kmeans_result <- kmeans(data, centers = 3)
fviz_cluster(kmeans_result, 
             data = data,
             ellipse = TRUE,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
             ggtheme = theme_bw(),
             main = "K-Means Clustering Results")
```

## Key Features of K-Medoids:

1. **Robust to outliers**: Unlike K-means, K-medoids uses actual data points as medoids
2. **Handles non-Euclidean distances**: Can work with various distance metrics
3. **More stable**: Less sensitive to noise and outliers in the data

## Output Explanation:

- **medoids**: The actual data points that represent each cluster
- **clustering**: Assignment of each data point to a cluster
- **silinfo**: Silhouette information for cluster evaluation
- **objective**: Objective function value (sum of distances to medoids)

## Alternative Distance Metrics:

```r
# Using different distance metrics
medoids_manhattan <- pam(data, k, metric = "manhattan")
medoids_cosine <- pam(data, k, metric = "cosine")

# Compare clustering quality
print(medoids_result$silinfo)
print(medoids_manhattan$silinfo)
```

This example demonstrates how to implement K-medoids clustering in R, visualize the results, and compare it with K-means clustering.

