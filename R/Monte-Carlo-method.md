# Monte Carlo Method Example in R

Here's a complete example of using the Monte Carlo method to estimate π using R:

```r
# Monte Carlo Method to Estimate π
# =================================

# Set seed for reproducibility
set.seed(123)

# Number of random points to generate
n <- 100000

# Generate random points in a unit square [0,1] × [0,1]
x <- runif(n)
y <- runif(n)

# Calculate distance from origin for each point
distance <- sqrt(x^2 + y^2)

# Count points inside the unit circle (distance <= 1)
points_inside_circle <- sum(distance <= 1)

# Estimate π using the ratio of points
# Area of circle / Area of square = π/4
# So π ≈ 4 * (points_inside_circle / n)
pi_estimate <- 4 * points_inside_circle / n

# Display results
cat("Number of points:", n, "\n")
cat("Points inside circle:", points_inside_circle, "\n")
cat("Estimated π:", pi_estimate, "\n")
cat("Actual π:", pi, "\n")
cat("Error:", abs(pi_estimate - pi), "\n")

# Visualize the results (optional)
# Create a scatter plot of points
plot(x, y, 
     col = ifelse(distance <= 1, "red", "blue"), 
     pch = 19, cex = 0.5,
     xlim = c(0,1), ylim = c(0,1),
     xlab = "x", ylab = "y",
     main = "Monte Carlo Estimation of π")

# Add the unit circle
theta <- seq(0, 2*pi, length.out = 100)
lines(cos(theta), sin(theta), col = "black", lwd = 2)

# Add legend
legend("topright", 
       legend = c("Inside circle", "Outside circle"),
       col = c("red", "blue"), 
       pch = 19)
```

## Alternative Example: Estimating Integral

```r
# Monte Carlo Integration Example
# =================================

# Function to integrate: f(x) = x^2 from 0 to 1
f <- function(x) {
  return(x^2)
}

# Number of samples
n <- 100000

# Generate random samples from uniform distribution [0,1]
x_samples <- runif(n, 0, 1)

# Evaluate function at sample points
y_samples <- f(x_samples)

# Monte Carlo estimate of integral
# Integral = ∫₀¹ x² dx = 1/3
integral_estimate <- mean(y_samples)

# Display results
cat("Monte Carlo integral estimate:", integral_estimate, "\n")
cat("Actual integral value:", 1/3, "\n")
cat("Error:", abs(integral_estimate - 1/3), "\n")
```

## Key Features of This Monte Carlo Implementation:

1. **Random Sampling**: Uses `runif()` to generate uniform random numbers
2. **Statistical Estimation**: Relies on the law of large numbers
3. **Geometric Interpretation**: Uses the ratio of areas to estimate π
4. **Convergence**: More points = better accuracy
5. **Visualization**: Optional plotting to show the process

## How It Works:

1. Generate random points in a unit square
2. Determine which points fall inside the inscribed circle
3. Use the ratio of points inside vs. total points to estimate π
4. The ratio approaches π/4 as the number of points increases

This method demonstrates the fundamental principle of Monte Carlo methods: using random sampling to solve deterministic problems.

