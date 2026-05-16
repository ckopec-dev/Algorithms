# Jarvis March Algorithm in R

The Jarvis March (also known as the Gift Wrapping Algorithm) is a simple algorithm for computing the convex hull of a set of points in 2D space.

```r
# Jarvis March Algorithm Implementation in R
jarvis_march <- function(points) {
  # Input validation
  if (nrow(points) < 3) {
    return(points)
  }
  
  # Find the leftmost point (with smallest x-coordinate)
  leftmost <- which.min(points[,1])
  
  # Initialize result with the leftmost point
  hull <- matrix(points[leftmost, ], nrow = 1)
  current <- leftmost
  
  repeat {
    # Find the next point in the hull
    next_point <- 1
    
    # Check all other points to find the one that makes the leftmost turn
    for (i in 1:nrow(points)) {
      if (i == current) next
      
      # Calculate cross product to determine orientation
      cross_product <- cross_product_2d(
        points[current, ], 
        points[next_point, ], 
        points[i, ]
      )
      
      # If we found a more counterclockwise point, update
      if (cross_product > 0 || 
          (cross_product == 0 && 
           distance(points[current, ], points[i, ]) > 
           distance(points[current, ], points[next_point, ]))) {
        next_point <- i
      }
    }
    
    # If we've wrapped around to the starting point, we're done
    if (next_point == leftmost) {
      break
    }
    
    # Add the next point to hull
    hull <- rbind(hull, points[next_point, ])
    current <- next_point
  }
  
  return(hull)
}

# Helper function to calculate cross product of three points
cross_product_2d <- function(p1, p2, p3) {
  # Cross product of vectors (p2-p1) and (p3-p1)
  return((p2[1] - p1[1]) * (p3[2] - p1[2]) - (p2[2] - p1[2]) * (p3[1] - p1[1]))
}

# Helper function to calculate distance between two points
distance <- function(p1, p2) {
  return(sqrt(sum((p1 - p2)^2)))
}

# Example usage
set.seed(123)
# Generate random points
points <- matrix(runif(20, 0, 10), nrow = 10, ncol = 2)

# Display original points
cat("Original points:\n")
print(points)

# Compute convex hull
hull <- jarvis_march(points)

# Display hull points
cat("\nConvex hull points:\n")
print(hull)

# Visualize the results
plot(points[,1], points[,2], 
     xlim = c(0, 10), ylim = c(0, 10),
     xlab = "X coordinate", ylab = "Y coordinate",
     main = "Jarvis March Convex Hull",
     pch = 19, col = "blue", cex = 1.2)

# Plot hull points in red
points(hull[,1], hull[,2], col = "red", pch = 19, cex = 1.5)

# Connect hull points to form the convex hull
lines(hull[,1], hull[,2], col = "red", lwd = 2)
lines(hull[nrow(hull),1], hull[nrow(hull),2], 
      hull[1,1], hull[1,2], col = "red", lwd = 2)

# Add labels for original points
text(points[,1], points[,2], 
     labels = 1:nrow(points), 
     cex = 0.7, col = "blue")
```

## Algorithm Explanation

The Jarvis March algorithm works as follows:

1. **Find the starting point**: Identify the leftmost point (or the point with the smallest x-coordinate)
2. **Build the hull**: Starting from this point, find the next point in the hull by checking which point makes the leftmost turn
3. **Continue**: Repeat until we return to the starting point

## Key Features

- **Time Complexity**: O(nh) where n is the number of points and h is the number of points on the hull
- **Space Complexity**: O(h) for storing the hull points
- **Works for**: Any set of 2D points
- **Handles**: Collinear points correctly

## Example Output

The algorithm will return the vertices of the convex hull in order, forming a polygon that encloses all input points. The visualization shows:
- Blue points: Original input points
- Red points: Vertices of the convex hull
- Red lines: The convex hull connecting the vertices

This implementation correctly handles edge cases and provides a clear visualization of the convex hull computation process.

