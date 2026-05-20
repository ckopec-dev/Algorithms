# Principal Component Analysis (PCA) in R

Here's a complete example of PCA using the R programming language:

## 1. Load Required Libraries

```r
# Load required libraries
library(ggplot2)
library(factoextra)
```

## 2. Create Sample Data

```r
# Create sample dataset
set.seed(123)
n <- 100

# Generate correlated variables
x1 <- rnorm(n, mean = 5, sd = 2)
x2 <- 0.8 * x1 + rnorm(n, mean = 0, sd = 1.5)
x3 <- 0.6 * x1 + 0.4 * x2 + rnorm(n, mean = 0, sd = 1)
x4 <- 0.3 * x2 + rnorm(n, mean = 0, sd = 2)

# Create data frame
data <- data.frame(
  var1 = x1,
  var2 = x2,
  var3 = x3,
  var4 = x4
)

# View first few rows
head(data)
```

## 3. Perform PCA

```r
# Perform PCA
pca_result <- prcomp(data, scale. = TRUE)

# View PCA summary
summary(pca_result)
```

## 4. Examine Results

```r
# Access principal components
pca_result$components

# Access standard deviations
pca_result$sdev

# Access rotation matrix
pca_result$rotation
```

## 5. Visualize Results

```r
# Scree plot (variance explained)
fviz_eigenvalue(pca_result, addlabels = TRUE, ylim = c(0, 5))

# Biplot
fviz_pca_biplot(pca_result, 
                repel = TRUE,
                col.var = "blue",
                col.ind = "black")

# Plot of first two principal components
fviz_pca_ind(pca_result, 
             col.ind = "cos2", 
             gradient.cols = c("white", "blue"),
             repel = TRUE)
```

## 6. Extract and Interpret Components

```r
# Get principal component scores
pc_scores <- predict(pca_result, data)

# View first few scores
head(pc_scores)

# Create a data frame with original data and PC scores
results <- data.frame(
  original_data = data,
  PC1 = pc_scores[,1],
  PC2 = pc_scores[,2],
  PC3 = pc_scores[,3],
  PC4 = pc_scores[,4]
)

# View results
head(results)
```

## 7. Calculate Proportion of Variance Explained

```r
# Calculate proportion of variance explained by each component
variance_explained <- (pca_result$sdev^2) / sum(pca_result$sdev^2)

# Create variance table
variance_table <- data.frame(
  Component = 1:length(variance_explained),
  Variance_Explained = variance_explained,
  Cumulative_Variance = cumsum(variance_explained)
)

print(variance_table)
```

## 8. Complete Example with Real Dataset

```r
# Using built-in iris dataset
data(iris)

# Perform PCA on iris dataset
iris_pca <- prcomp(iris[,1:4], scale. = TRUE)

# Summary
summary(iris_pca)

# Plot results
fviz_pca_biplot(iris_pca, 
                col.var = "red",
                col.ind = iris$Species,
                addEllipses = TRUE,
                legend.title = "Species")
```

## Key Points:

- **`scale. = TRUE`**: Standardizes variables to have mean = 0 and sd = 1
- **`prcomp()`**: Function for performing PCA in R
- **`summary()`**: Shows variance explained by each component
- **`predict()`**: Used to get component scores for new data
- **`factoextra`**: Provides enhanced visualization functions for PCA

This example demonstrates the complete workflow of PCA in R, from data preparation to visualization and interpretation of results.

