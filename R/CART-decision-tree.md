# CART Decision Tree in R

Here's a complete example of implementing a CART (Classification and Regression Trees) decision tree using the `rpart` package in R:

```r
# Load required libraries
library(rpart)
library(rpart.plot)

# Load built-in dataset
data(iris)

# Split data into training and testing sets
set.seed(123)
train_index <- sample(1:nrow(iris), 0.7 * nrow(iris))
train_data <- iris[train_index, ]
test_data <- iris[-train_index, ]

# Build CART decision tree model
cart_model <- rpart(Species ~ ., 
                   data = train_data,
                   method = "class",
                   control = rpart.control(cp = 0.01, minsplit = 20))

# Display the model
print(cart_model)

# Visualize the decision tree
rpart.plot(cart_model, 
           type = 3,
           extra = 102,
           fallen.leaves = TRUE,
           main = "CART Decision Tree - Iris Dataset")

# Make predictions on test data
predictions <- predict(cart_model, test_data, type = "class")

# Create confusion matrix
confusion_matrix <- table(Predicted = predictions, Actual = test_data$Species)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", round(accuracy * 100, 2), "%\n")

# Variable importance
importance <- cart_model$variable.importance
print(importance)

# Plot variable importance
barplot(importance, main = "Variable Importance", las = 2)
```

## Key Components Explained:

### 1. **Model Building Parameters:**
- `method = "class"` - For classification problems
- `cp = 0.01` - Complexity parameter (pruning threshold)
- `minsplit = 20` - Minimum observations required to attempt a split

### 2. **Formula Notation:**
- `Species ~ .` - Predict Species using all other variables

### 3. **Model Evaluation:**
- Confusion matrix for classification accuracy
- Variable importance measures

### 4. **Visualization:**
- `rpart.plot()` creates a readable tree diagram
- Shows splits, node numbers, and class probabilities

## Expected Output:
The model will show:
- Tree structure with decision rules
- Prediction accuracy (typically >90% for iris dataset)
- Variable importance (petal length and width usually most important)
- Visual tree diagram showing the classification process

This example demonstrates the complete workflow for building, visualizing, and evaluating a CART decision tree model in R.

