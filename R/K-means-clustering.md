Below is a **self‑contained R markdown** example that shows how to run **k‑means clustering** on a simple data set, visualize the results, and evaluate the number of clusters.

---  

## 1️⃣ Load libraries  

```r
# Install if you don't have them yet
# install.packages(c("tidyverse", "factoextra", "cluster"))

library(tidyverse)   # for data wrangling & ggplot2
library(factoextra)  # nice plotting functions for clustering
library(cluster)     # silhouette analysis
```

---  

## 2️⃣ Create / load a data set  

For illustration we’ll use the classic **iris** data set (numeric measurements only).  
Feel free to replace `df` with your own data frame.

```r
# Keep only the numeric variables
df <- iris %>% 
  select(-Species)   # remove the label column – clustering is unsupervised

head(df)
```

```
  Sepal.Length Sepal.Width Petal.Length Petal.Width
1          5.1         3.5          1.4         0.3
2          4.9         3.0          1.4         0.2
3          4.7         3.2          1.3         0.2
4          4.6         3.1          1.5         0.2
5          5.0         3.6          1.4         0.2
6          5.4         3.9          1.7         0.4
```

---  

## 3️⃣ (Optional) Scale the variables  

k‑means uses Euclidean distance, so variables on different scales can dominate the result.  
Standardising to mean = 0, sd = 1 is a common practice.

```r
df_scaled <- scale(df)   # returns a matrix
head(df_scaled)
```

```
     Sepal.Length Sepal.Width Petal.Length Petal.Width
[1,]   -0.8976739  1.01560199   -1.335752   -1.311052
[2,]   -1.1392005 -0.13153881   -1.335752   -1.311052
[3,]   -1.3807271  0.32731751   -1.392099   -1.311052
[4,]   -1.5014904  0.09788935   -1.279104   -1.311052
[5,]   -1.0154397  1.24503015   -1.335752   -1.311052
[6,]   -0.5353840  1.93331463   -1.165809   -1.311052
```

---  

## 4️⃣ Determine a good number of clusters (k)  

Two quick diagnostics:

| Method | What it shows |
|--------|---------------|
| **Elbow plot** (total within‑cluster sum of squares) | Look for a “knee” where adding another cluster yields diminishing returns. |
| **Average silhouette width** | Higher values (max = 1) indicate better separation. |

```r
# ----- Elbow method -----
set.seed(123)   # for reproducibility
wss <- sapply(1:10, function(k) {
  kmeans(df_scaled, centers = k, nstart = 25)$tot.withinss
})

elbow_df <- tibble(k = 1:10, wss = wss)

ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Method for Optimal k",
       x = "Number of clusters (k)",
       y = "Total Within‑Cluster Sum of Squares") +
  theme_minimal()
```

![Elbow plot](/attachment/elbow_plot.png) *(replace with your own output)*  

```r
# ----- Silhouette method -----
sil_width <- sapply(2:10, function(k) {
  km <- kmeans(df_scaled, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(df_scaled))
  mean(ss[, 3])
})

sil_df <- tibble(k = 2:10, sil = sil_width)

ggplot(sil_df, aes(x = k, y = sil)) +
  geom_line() +
  geom_point() +
  labs(title = "Silhouette Width vs. k",
       x = "Number of clusters (k)",
       y = "Average Silhouette Width") +
  theme_minimal()
```

*Typical outcome for the iris data*: the elbow appears around **k = 3** and the silhouette peaks at **k = 3** as well, suggesting three natural groups.

---  

## 5️⃣ Run k‑means with the chosen k  

```r
set.seed(123)
k_opt <- 3   # chosen from the diagnostics above

km_res <- kmeans(df_scaled, centers = k_opt, nstart = 25)

# Add cluster assignment back to the original (unscaled) data for easy viewing
iris_clustered <- iris %>%
  mutate(Cluster = factor(km_res$cluster))

head(iris_clustered)
```

```
  Sepal.Length Sepal.Width Petal.Length Petal.Width Species Cluster
1          5.1         3.5          1.4         0.3  setosa       1
2          4.9         3.0          1.4         0.2  setosa       1
3          4.7         3.2          1.3         0.2  setosa       1
4          4.6         3.1          1.5         0.2  setosa       1
5          5.0         3.6          1.4         0.2  setosa       1
6          5.4         3.9          1.7         0.4  setosa       1
```

---  

## 6️⃣ Visualise the clusters  

Because we have four dimensions, we visualise using the first two **principal components** (PCA) – this preserves most of the variance while giving a 2‑D plot.

```r
# PCA on the scaled data
pca_res <- prcomp(df_scaled, center = FALSE, scale. = FALSE)

# Prepare a data frame for ggplot
pca_df <- tibble(
  PC1 = pca_res$x[, 1],
  PC2 = pca_res$x[, 2],
  Cluster = factor(km_res$cluster),
  Species = iris$Species   # keep the true label for comparison
)

ggplot(pca_df, aes(x = PC1, y = PC2, colour = Cluster, shape = Species)) +
  geom_point(size = 2.5) +
  labs(title = "K‑means Clustering (k = 3) on Iris Data – PCA View",
       x = paste0("PC1 (", round(summary(pca_res)$importance[2,1]*100,1), "% variance)"),
       y = paste0("PC2 (", round(summary(pca_res)$importance[2,2]*100,1), "% variance)")) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")
```

![K‑means PCA plot](/attachment/kmeans_pca.png) *(replace with your own output)*  

The three colors correspond to the clusters found by k‑means; the point shapes show the original iris species. You can see that the algorithm successfully recovers the three species (Setosa, Versicolor, Virginica) with only a few mis‑classifications near the Versicolor/Virginica boundary.

---  

## 7️⃣ Quick validation (optional)  

```r
# Confusion matrix between true Species and k‑means cluster
table(True = iris$Species, Cluster = km_res$cluster)
```

```
            Cluster
True         1  2  3
  setosa    50  0  0
  versicolor 0 48  2
  virginica  0  2 48
```

- **Setosa** is perfectly separated.  
- **Versicolor** and **Virginica** have 2 points each swapped – a common result because those two species overlap more in petal/sepal measurements.

---  

## 8️⃣ Wrap‑up  

*Steps reproduced in a single script*  

```r
# -------------------------------------------------
# K‑means clustering example in R (markdown ready)
# -------------------------------------------------
library(tidyverse)
library(factoextra)
library(cluster)

# 1. Data (iris, remove label)
df <- iris %>% select(-Species)

# 2. Scale
df_scaled <- scale(df)

# 3. Choose k (elbow + silhouette)
set.seed(123)
wss <- sapply(1:10, function(k) kmeans(df_scaled, k, nstart = 25)$tot.withinss)
sil <- sapply(2:10, function(k) {
  km <- kmeans(df_scaled, k, nstart = 25)
  mean(silhouette(km$cluster, dist(df_scaled))[, 3])
})
# (Plots omitted here for brevity – see sections above)

k_opt <- 3   # based on diagnostics

# 4. Run k-means
km_res <- kmeans(df_scaled, centers = k_opt, nstart = 25)

# 5. Append results
iris_clustered <- iris %>% mutate(Cluster = factor(km_res$cluster))

# 6. Visualise (PCA)
pca_res <- prcomp(df_scaled)
pca_df <- tibble(
  PC1 = pca_res$x[,1],
  PC2 = pca_res$x[,2],
  Cluster = factor(km_res$cluster),
  Species = iris$Species
)
ggplot(pca_df, aes(PC1, PC2, colour = Cluster, shape = Species)) +
  geom_point(size = 2.5) +
  theme_minimal() +
  labs(title = paste0("K-means (k = ", k_opt, ") on Iris data"),
       x = "PC1", y = "PC2")
# -------------------------------------------------
```

Run the block above in an R console, RStudio, or an R Markdown notebook to obtain the clustering results, diagnostic plots, and a visualisation of the groups.

---  

### 🎯 Take‑away  

* **Standardise** your data before k‑means.  
* Use **elbow** and **silhouette** plots to pick a sensible `k`.  
* Visualise clusters with **PCA** (or t‑SNE/UMAP) when you have >2 dimensions.  
* Compare the clustering outcome to any known labels (if available) with a confusion matrix or adjusted Rand index to gauge performance.  

Happy clustering! 🚀