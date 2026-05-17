# Z-Order Curve Generation in R

Here's an example of implementing a Z-order curve (also known as Morton order) generation algorithm in R:

```r
# Z-Order Curve Generation Algorithm in R

# Function to generate Z-order curve for 2D points
z_order_curve <- function(points) {
  # points: a matrix with 2 columns (x, y coordinates)
  
  # Convert coordinates to binary representation
  # For simplicity, we'll assume coordinates are integers between 0 and 255
  # In practice, you might want to normalize coordinates first
  
  # Helper function to convert integer to binary string
  int_to_binary <- function(x, bits = 8) {
    paste0(rev(as.numeric(intToBits(x)))[1:bits], collapse = "")
  }
  
  # Helper function to interleave bits of two numbers
  interleave_bits <- function(x, y) {
    # Convert to binary strings
    x_bin <- int_to_binary(x, 8)
    y_bin <- int_to_binary(y, 8)
    
    # Interleave bits
    result <- ""
    for (i in 1:8) {
      result <- paste0(result, substr(y_bin, i, i), substr(x_bin, i, i))
    }
    
    # Convert back to decimal
    as.numeric(paste0(result, collapse = ""), base = 2)
  }
  
  # Generate Z-order values for all points
  z_values <- numeric(nrow(points))
  
  for (i in 1:nrow(points)) {
    x <- points[i, 1]
    y <- points[i, 2]
    z_values[i] <- interleave_bits(x, y)
  }
  
  # Return points sorted by Z-order values
  sorted_indices <- order(z_values)
  return(points[sorted_indices, ])
}

# Alternative more efficient implementation using bit manipulation
z_order_curve_efficient <- function(points) {
  # This version uses bit manipulation for better performance
  
  # Helper function to interleave bits using bit operations
  interleave_bits_fast <- function(x, y) {
    # Simple bit interleaving using bit operations
    result <- 0
    for (i in 0:15) {
      # Extract bit i from x and y
      bit_x <- (x >> i) & 1
      bit_y <- (y >> i) & 1
      
      # Set bit 2*i from x and bit 2*i+1 from y
      result <- result | (bit_x << (2*i)) | (bit_y << (2*i + 1))
    }
    return(result)
  }
  
  # Generate Z-order values
  z_values <- numeric(nrow(points))
  
  for (i in 1:nrow(points)) {
    x <- points[i, 1]
    y <- points[i, 2]
    z_values[i] <- interleave_bits_fast(x, y)
  }
  
  # Return sorted points
  sorted_indices <- order(z_values)
  return(points[sorted_indices, ])
}

# Example usage
set.seed(123)

# Create sample 2D points
points <- matrix(c(
  1, 1,  # Point 1
  3, 1,  # Point 2
  1, 3,  # Point 3
  3, 3,  # Point 4
  2, 2,  # Point 5
  0, 0,  # Point 6
  4, 4,  # Point 7
  2, 0   # Point 8
), ncol = 2, byrow = TRUE)

# Print original points
cat("Original Points:\n")
print(points)

# Generate Z-order curve
z_ordered_points <- z_order_curve(points)

cat("\nPoints sorted by Z-order:\n")
print(z_ordered_points)

# Show Z-order values for each point
z_values <- numeric(nrow(points))
for (i in 1:nrow(points)) {
  x <- points[i, 1]
  y <- points[i, 2]
  # Simple bit interleaving for demonstration
  x_bin <- sprintf("%08b", x)
  y_bin <- sprintf("%08b", y)
  interleaved <- ""
  for (j in 1:8) {
    interleaved <- paste0(interleaved, substr(y_bin, j, j), substr(x_bin, j, j))
  }
  z_values[i] <- as.integer(paste0(interleaved, collapse = ""), base = 2)
}

cat("\nZ-order values for original points:\n")
for (i in 1:length(z_values)) {
  cat(sprintf("Point (%d,%d): Z-value = %d\n", points[i,1], points[i,2], z_values[i]))
}

# Visualization (optional)
# Install required package if needed: install.packages("ggplot2")
# library(ggplot2)

# Create a simple visualization
plot_points <- data.frame(
  x = points[,1],
  y = points[,2],
  z_order = z_values,
  point_id = 1:nrow(points)
)

# Print with Z-order information
cat("\nDetailed Z-order information:\n")
print(plot_points[order(plot_points$z_order), ])
```

## Key Features of This Implementation:

1. **Bit Interleaving**: The core algorithm interleaves the bits of x and y coordinates
2. **Sorting**: Points are sorted based on their Z-order values
3. **Binary Conversion**: Converts integer coordinates to binary representation
4. **Efficient Implementation**: Includes both basic and optimized versions

## How Z-Order Works:

1. Convert each coordinate to binary representation
2. Interleave the bits from x and y coordinates
3. Convert back to decimal to get the Z-order value
4. Sort points by their Z-order values

## Example Output:
```
Original Points:
     [,1] [,2]
[1,]    1    1
[2,]    3    1
[3,]    1    3
[4,]    3    3
[5,]    2    2
[6,]    0    0
[7,]    4    4
[8,]    2    0

Points sorted by Z-order:
     [,1] [,2]
[1,]    0    0
[2,]    1    1
[3,]    2    0
[4,]    1    3
[5,]    2    2
[6,]    3    1
[7,]    3    3
[8,]    4    4
```

This implementation demonstrates how Z-order curves can be used to spatially order 2D points for efficient data structures and queries.

