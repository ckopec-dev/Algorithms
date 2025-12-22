# Principal Component Analysis (PCA) in R

Here's a complete example of PCA using the R programming language:

```r
# Load required libraries
library(ggplot2)
library(factoextra)

# Example 1: Using built-in mtcars dataset
# Load the dataset
data(mtcars)
head(mtcars)

# Perform PCA
pca_result <- prcomp(mtcars, scale. = TRUE)

# View summary of PCA
summary(pca_result)

# Extract principal components
pca_components <- pca_result$x
head(pca_components)

# Plot PCA biplot
fviz_pca_biplot(pca_result, 
                repel = TRUE,
                col.var = "blue",
                col.ind = "red",
                legend.title = "Variables")

# Scree plot to show variance explained
fviz_eig(pca_result, addlabels = TRUE)

# Example 2: Using custom data
# Create sample dataset
set.seed(123)
n <- 100
x1 <- rnorm(n, 0, 1)
x2 <- x1 + rnorm(n, 0, 0.5)
x3 <- x1 * 0.5 + x2 * 0.3 + rnorm(n, 0, 1)
x4 <- x2 + rnorm(n, 0, 0.8)

custom_data <- data.frame(x1, x2, x3, x4)

# Perform PCA on custom data
custom_pca <- prcomp(custom_data, scale. = TRUE)

# Summary
summary(custom_pca)

# View loadings (how original variables contribute to PCs)
loadings <- custom_pca$rotation
print(loadings)

# Plot results
fviz_pca_biplot(custom_pca, 
                repel = TRUE,
                col.var = "darkblue",
                col.ind = "red",
                legend.title = "Individuals")

# Extract principal component scores
pc_scores <- custom_pca$x
head(pc_scores)

# Variance explained by each component
variance_explained <- summary(custom_pca)$importance[2,]
print(variance_explained)
```

## Key Functions and Output:

### Main Functions Used:
- `prcomp()`: Performs PCA analysis
- `summary()`: Shows variance explained by each component
- `fviz_pca_biplot()`: Creates biplot visualization
- `fviz_eig()`: Creates scree plot

### Key Outputs:
1. **Principal Components**: Transformed data in new coordinate system
2. **Loadings**: How original variables contribute to each PC
3. **Variance Explained**: Percentage of total variance captured by each component
4. **Biplot**: Visualization showing both individuals and variables

### Important Parameters:
- `scale. = TRUE`: Standardizes variables (recommended when variables have different units)
- `center = TRUE`: Centers variables (default behavior)

This example demonstrates how to perform PCA, interpret results, and visualize the principal components effectively.

