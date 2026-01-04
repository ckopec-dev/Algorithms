# Iterative Closest Point (ICP) Algorithm in R

Here's a complete example of implementing the ICP algorithm in R to align two 2D point sets:

```r
# Load required libraries
library(graphics)
library(stats)

# Function to compute Euclidean distance between two points
euclidean_distance <- function(p1, p2) {
  return(sqrt(sum((p1 - p2)^2)))
}

# Function to find closest points between two point sets
find_closest_points <- function(source_points, target_points) {
  n <- nrow(source_points)
  closest_indices <- rep(0, n)
  distances <- rep(0, n)
  
  for (i in 1:n) {
    min_dist <- Inf
    closest_idx <- 1
    
    for (j in 1:nrow(target_points)) {
      dist <- euclidean_distance(source_points[i,], target_points[j,])
      if (dist < min_dist) {
        min_dist <- dist
        closest_idx <- j
      }
    }
    
    closest_indices[i] <- closest_idx
    distances[i] <- min_dist
  }
  
  return(list(indices = closest_indices, distances = distances))
}

# Function to compute transformation matrix (rotation + translation)
compute_transformation <- function(source_points, target_points) {
  # Compute centroids
  source_centroid <- colMeans(source_points)
  target_centroid <- colMeans(target_points)
  
  # Center the points
  source_centered <- source_points - rep(source_centroid, each = nrow(source_points))
  target_centered <- target_points - rep(target_centroid, each = nrow(target_points))
  
  # Compute covariance matrix
  H <- t(source_centered) %*% target_centered
  
  # Singular Value Decomposition
  svd_result <- svd(H)
  
  # Compute rotation matrix
  R <- svd_result$u %*% diag(c(1, det(svd_result$u %*% svd_result$v))) %*% t(svd_result$v)
  
  # Compute translation
  t <- target_centroid - R %*% source_centroid
  
  return(list(rotation = R, translation = t))
}

# Function to apply transformation to points
apply_transformation <- function(points, rotation, translation) {
  return(t(rotation %*% t(points) + rep(translation, each = nrow(points))))
}

# Main ICP algorithm
icp <- function(source_points, target_points, max_iterations = 50, tolerance = 1e-6) {
  current_source <- source_points
  total_transform <- list(rotation = diag(2), translation = c(0, 0))
  
  for (iter in 1:max_iterations) {
    # Find closest points
    closest <- find_closest_points(current_source, target_points)
    
    # Extract corresponding target points
    corresponding_target <- target_points[closest$indices, ]
    
    # Compute transformation
    transform <- compute_transformation(current_source, corresponding_target)
    
    # Apply transformation to source points
    current_source <- apply_transformation(current_source, transform$rotation, transform$translation)
    
    # Update total transformation
    total_transform$rotation <- transform$rotation %*% total_transform$rotation
    total_transform$translation <- transform$rotation %*% total_transform$translation + transform$translation
    
    # Check convergence
    if (iter > 1) {
      # Calculate change in transformation
      if (max(abs(transform$rotation - diag(2))) < tolerance && 
          max(abs(transform$translation)) < tolerance) {
        cat("Converged after", iter, "iterations\n")
        break
      }
    }
  }
  
  return(list(transformed_points = current_source, 
              final_transform = total_transform))
}

# Example usage
set.seed(123)  # For reproducible results

# Create a source point set (original)
source_points <- matrix(c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5,
                          1, 2, 3, 4, 5, 2, 3, 4, 5, 6), 
                        ncol = 2, byrow = TRUE)

# Create a target point set (transformed version of source)
target_points <- matrix(c(2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
                          3, 4, 5, 6, 7, 4, 5, 6, 7, 8), 
                        ncol = 2, byrow = TRUE)

# Run ICP algorithm
result <- icp(source_points, target_points, max_iterations = 30)

# Display results
cat("Original source points:\n")
print(source_points)

cat("\nTarget points:\n")
print(target_points)

cat("\nTransformed source points:\n")
print(result$transformed_points)

cat("\nFinal transformation matrix:\n")
print(result$final_transform$rotation)

cat("\nFinal translation vector:\n")
print(result$final_transform$translation)

# Visualization
par(mfrow = c(1, 2))

# Plot original and target points
plot(source_points[,1], source_points[,2], 
     col = "blue", pch = 19, cex = 1.2, 
     main = "Before ICP Alignment", 
     xlab = "X", ylab = "Y")
points(target_points[,1], target_points[,2], 
       col = "red", pch = 19, cex = 1.2)
legend("topleft", c("Source", "Target"), col = c("blue", "red"), pch = 19)

# Plot aligned points
plot(result$transformed_points[,1], result$transformed_points[,2], 
     col = "green", pch = 19, cex = 1.2, 
     main = "After ICP Alignment", 
     xlab = "X", ylab = "Y")
points(target_points[,1], target_points[,2], 
       col = "red", pch = 19, cex = 1.2)
legend("topleft", c("Aligned Source", "Target"), col = c("green", "red"), pch = 19)

# Calculate RMS error after alignment
rms_error <- sqrt(mean((result$transformed_points - target_points)^2))
cat("\nRoot Mean Square Error:", rms_error, "\n")
```

## Key Components Explained:

### 1. **Distance Calculation**
- `euclidean_distance()`: Computes the Euclidean distance between two 2D points

### 2. **Closest Point Matching**
- `find_closest_points()`: For each source point, finds the closest target point

### 3. **Transformation Computation**
- `compute_transformation()`: Uses SVD to compute optimal rotation and translation
- Implements the Horn's method for 2D point set registration

### 4. **Transformation Application**
- `apply_transformation()`: Applies rotation and translation to point sets

### 5. **Main ICP Loop**
- Iteratively matches points, computes transformations, and applies them
- Converges when transformation changes are below tolerance

## Expected Output:
The algorithm will show:
- Original source and target point sets
- Transformed source points after alignment
- Final transformation matrix and translation vector
- RMS error indicating alignment quality
- Visual comparison before and after alignment

This implementation demonstrates the core concepts of ICP for 2D point set registration and can be extended for 3D applications.

