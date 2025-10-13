# Convex Hull Algorithm in R

Here's an example of implementing a convex hull algorithm in R using the `grDevices` package:

```r
# Load required library
library(grDevices)

# Generate random points
set.seed(123)
n <- 50
x <- runif(n, 0, 10)
y <- runif(n, 0, 10)
points <- data.frame(x = x, y = y)

# Create the convex hull
hull <- chull(points)

# Plot the points and convex hull
plot(points$x, points$y, 
     xlim = c(0, 10), ylim = c(0, 10),
     xlab = "X coordinate", ylab = "Y coordinate",
     main = "Convex Hull Example",
     pch = 19, col = "blue", cex = 0.8)

# Add the convex hull polygon
polygon(points$x[hull], points$y[hull], 
        border = "red", lwd = 2, col = rgb(1, 0, 0, 0.2))

# Add points to the plot
points(points$x, points$y, pch = 19, col = "blue", cex = 0.8)

# Add the hull points in a different color
points(points$x[hull], points$y[hull], pch = 19, col = "red", cex = 1.2)

# Add legend
legend("topright", 
       legend = c("Data points", "Convex hull", "Hull vertices"),
       col = c("blue", "red", "red"), 
       pch = c(19, 19, 19), 
       pt.cex = c(0.8, 1.2, 1.2),
       cex = 0.8)
```

## Alternative implementation using the `sp` package:

```r
# Alternative approach using sp package
library(sp)

# Create spatial points
coordinates(points) <- ~x + y
sp_points <- SpatialPoints(points)

# Create convex hull
hull_sp <- ConvexHull(sp_points)

# Plot the result
plot(sp_points, pch = 19, col = "blue", cex = 0.8)
plot(hull_sp, add = TRUE, border = "red", lwd = 2)
```

## Function to compute and visualize convex hull:

```r
# Function to compute convex hull with visualization
compute_convex_hull <- function(x, y, plot = TRUE) {
  # Create data frame
  points <- data.frame(x = x, y = y)
  
  # Compute convex hull
  hull_indices <- chull(points)
  
  # Extract hull points
  hull_points <- points[hull_indices, ]
  
  if (plot) {
    # Plot
    plot(points$x, points$y, 
         xlim = range(points$x), ylim = range(points$y),
         xlab = "X coordinate", ylab = "Y coordinate",
         main = "Convex Hull",
         pch = 19, col = "blue", cex = 0.8)
    
    # Add convex hull
    polygon(points$x[hull_indices], points$y[hull_indices], 
            border = "red", lwd = 2, col = rgb(1, 0, 0, 0.2))
    
    # Add points
    points(points$x, points$y, pch = 19, col = "blue", cex = 0.8)
    
    # Return hull indices
    return(hull_indices)
  } else {
    return(hull_indices)
  }
}

# Example usage
x <- c(1, 2, 3, 4, 5, 2, 3, 4)
y <- c(1, 3, 2, 4, 1, 5, 3, 2)
hull_indices <- compute_convex_hull(x, y)
print(paste("Hull points indices:", paste(hull_indices, collapse = ", ")))
```

## Key Functions Used:

- `chull()`: Computes the convex hull of a set of points
- `polygon()`: Draws the convex hull polygon
- `plot()`: Creates the visualization
- `points()`: Adds individual points to the plot

The convex hull is the smallest convex polygon that contains all the points, with the hull vertices being the points that form the boundary of this polygon.

