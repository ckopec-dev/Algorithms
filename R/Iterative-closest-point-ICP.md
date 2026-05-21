# Iterative Closest Point (ICP) Algorithm in R

Here's a complete example of implementing the ICP algorithm in R:

```r
# Load required libraries
library(plotly)
library(dplyr)

# Function to calculate Euclidean distance between two points
euclidean_distance <- function(p1, p2) {
  sqrt(sum((p1 - p2)^2))
}

# Function to find closest points between two point sets
find_closest_points <- function(source_points, target_points) {
  closest_pairs <- list()
  
  for (i in 1:nrow(source_points)) {
    min_dist <- Inf
    closest_idx <- 1
    
    for (j in 1:nrow(target_points)) {
      dist <- euclidean_distance(source_points[i,], target_points[j,])
      if (dist < min_dist) {
        min_dist <- dist
        closest_idx <- j
      }
    }
    
    closest_pairs[[i]] <- list(
      source = source_points[i,],
      target = target_points[closest_idx,],
      distance = min_dist
    )
  }
  
  return(closest_pairs)
}

# Function to calculate transformation matrix (rotation + translation)
calculate_transformation <- function(source_points, target_points) {
  # Calculate centroids
  source_centroid <- colMeans(source_points)
  target_centroid <- colMeans(target_points)
  
  # Center the points
  source_centered <- source_points - rep(source_centroid, each = nrow(source_points))
  target_centered <- target_points - rep(target_centroid, each = nrow(target_points))
  
  # Calculate covariance matrix
  H <- t(source_centered) %*% target_centered
  
  # Perform SVD
  svd_result <- svd(H)
  
  # Calculate rotation matrix
  R <- svd_result$u %*% diag(c(1, det(svd_result$u %*% svd_result$v))) %*% t(svd_result$v)
  
  # Calculate translation vector
  t <- target_centroid - R %*% source_centroid
  
  return(list(rotation = R, translation = t))
}

# Function to apply transformation to points
apply_transformation <- function(points, rotation, translation) {
  transformed_points <- points %*% t(rotation) + rep(translation, each = nrow(points))
  return(transformed_points)
}

# Main ICP algorithm function
icp <- function(source_points, target_points, max_iterations = 50, tolerance = 1e-6) {
  current_points <- source_points
  transformations <- list()
  
  for (i in 1:max_iterations) {
    # Find closest points
    closest_pairs <- find_closest_points(current_points, target_points)
    
    # Extract source and target points for transformation calculation
    source_for_transform <- matrix(0, nrow = length(closest_pairs), ncol = 2)
    target_for_transform <- matrix(0, nrow = length(closest_pairs), ncol = 2)
    
    for (j in 1:length(closest_pairs)) {
      source_for_transform[j,] <- closest_pairs[[j]]$source
      target_for_transform[j,] <- closest_pairs[[j]]$target
    }
    
    # Calculate transformation
    transform <- calculate_transformation(source_for_transform, target_for_transform)
    
    # Apply transformation
    current_points <- apply_transformation(current_points, transform$rotation, transform$translation)
    
    # Store transformation
    transformations[[i]] <- transform
    
    # Check convergence (optional)
    if (i > 1) {
      # Calculate change in transformation
      diff <- sum((transform$translation - transformations[[i-1]]$translation)^2)
      if (diff < tolerance) {
        cat("Converged after", i, "iterations\n")
        break
      }
    }
  }
  
  return(list(
    final_points = current_points,
    transformations = transformations,
    iterations = i
  ))
}

# Generate sample data for demonstration
set.seed(123)

# Create original shape (a triangle)
original_shape <- matrix(c(0, 0, 1, 0, 0.5, 1), ncol = 2, byrow = TRUE)

# Create a transformed version (rotated and translated)
# Original triangle rotated 30 degrees and translated by (2, 1)
rotation_matrix <- matrix(c(cos(pi/6), -sin(pi/6), sin(pi/6), cos(pi/6)), ncol = 2, byrow = TRUE)
translation_vector <- c(2, 1)

# Apply transformation to create target shape
target_shape <- original_shape %*% t(rotation_matrix) + rep(translation_vector, each = nrow(original_shape))

# Add some noise to make it more realistic
noisy_target <- target_shape + matrix(rnorm(6, 0, 0.1), ncol = 2)

# Run ICP algorithm
icp_result <- icp(original_shape, noisy_target, max_iterations = 20)

# Visualize results
plot_data <- data.frame(
  x = c(original_shape[,1], noisy_target[,1], icp_result$final_points[,1]),
  y = c(original_shape[,2], noisy_target[,2], icp_result$final_points[,2]),
  group = rep(c("Original", "Noisy Target", "ICP Result"), 
              c(nrow(original_shape), nrow(noisy_target), nrow(icp_result$final_points)))
)

# Create interactive plot
p <- plot_ly(plot_data, x = ~x, y = ~y, color = ~group, 
             type = 'scatter', mode = 'markers', 
             sizes = c(20, 40, 60),
             marker = list(opacity = 0.7)) %>%
  layout(title = "ICP Algorithm Visualization",
         xaxis = list(title = "X Coordinate"),
         yaxis = list(title = "Y Coordinate"))

p

# Print results
cat("ICP Algorithm Results:\n")
cat("Iterations completed:", icp_result$iterations, "\n")
cat("Final transformation matrix:\n")
print(icp_result$transformations[[icp_result$iterations]]$rotation)
cat("Final translation vector:\n")
print(icp_result$transformations[[icp_result$iterations]]$translation)
cat("Final aligned points:\n")
print(icp_result$final_points)
```

## Key Components Explained:

### 1. **Distance Calculation**
- `euclidean_distance()`: Calculates the Euclidean distance between two 2D points

### 2. **Closest Point Matching**
- `find_closest_points()`: For each point in the source set, finds the closest point in the target set

### 3. **Transformation Calculation**
- `calculate_transformation()`: Uses Singular Value Decomposition (SVD) to calculate optimal rotation and translation

### 4. **Point Transformation**
- `apply_transformation()`: Applies the calculated rotation and translation to the points

### 5. **Main ICP Loop**
- Iteratively matches points, calculates transformations, and applies them until convergence

## How ICP Works:

1. **Initialization**: Start with source and target point sets
2. **Point Matching**: Find the closest points between source and target
3. **Transformation**: Calculate optimal rotation and translation using SVD
4. **Apply Transformation**: Transform source points using calculated parameters
5. **Repeat**: Continue until convergence or maximum iterations reached

## Output Features:

- **Convergence Detection**: Stops when transformation changes are below tolerance
- **Visualization**: Interactive plot showing original, noisy target, and final aligned points
- **Transformation Details**: Returns rotation matrix and translation vector
- **Iteration Count**: Shows how many iterations were needed for convergence

This implementation demonstrates the core principles of ICP algorithm for point cloud registration and alignment.

