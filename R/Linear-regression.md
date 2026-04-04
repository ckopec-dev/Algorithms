# Linear Regression in R

Here's a complete example of implementing linear regression in R using the built-in `mtcars` dataset:

```r
# Load the mtcars dataset
data(mtcars)

# View the first few rows of the dataset
head(mtcars)

# Basic structure of the data
str(mtcars)

# Simple linear regression: mpg ~ wt (weight)
model <- lm(mpg ~ wt, data = mtcars)

# View the model summary
summary(model)

# Plot the regression line
plot(mtcars$wt, mtcars$mpg, 
     xlab = "Weight (1000 lbs)", 
     ylab = "Miles per Gallon",
     main = "Linear Regression: MPG vs Weight")

# Add the regression line
abline(model, col = "red", lwd = 2)

# Make predictions
new_data <- data.frame(wt = c(2.5, 3.0, 3.5))
predictions <- predict(model, new_data)
predictions

# Calculate R-squared
summary(model)$r.squared

# View model coefficients
coefficients(model)

# Residuals analysis
plot(model, which = 1)  # Residuals vs Fitted plot
```

## Output Explanation

The `summary(model)` output will show:
- **Coefficients**: Intercept and slope values
- **R-squared**: Proportion of variance explained (0-1 scale)
- **p-values**: Statistical significance of coefficients
- **F-statistic**: Overall model significance

## Multiple Linear Regression Example

```r
# Multiple linear regression: mpg ~ wt + hp + cyl
model2 <- lm(mpg ~ wt + hp + cyl, data = mtcars)
summary(model2)

# Compare models
anova(model, model2)  # Analysis of variance to compare models
```

## Key Functions Used

- `lm()`: Linear model function
- `summary()`: Model statistics
- `predict()`: Make predictions
- `plot()`: Visualize results
- `coefficients()`: Extract model coefficients

This example demonstrates the fundamental steps of linear regression analysis in R, from model fitting to interpretation and visualization.

