# Decision Tree Learning in R

Here's a complete example of implementing a decision tree learning algorithm in R using the `rpart` package:

```r
# Load required libraries
library(rpart)
library(rpart.plot)

# Create sample dataset
set.seed(123)
data <- data.frame(
  age = c(25, 35, 45, 23, 35, 31, 26, 36, 40, 28, 33, 41, 29, 37, 42),
  income = c(50000, 80000, 120000, 45000, 75000, 65000, 55000, 85000, 110000, 60000, 70000, 115000, 62000, 88000, 125000),
  education = c("Bachelor", "Master", "PhD", "Bachelor", "Master", "Bachelor", "Master", "PhD", "PhD", "Bachelor", "Master", "PhD", "Bachelor", "Master", "PhD"),
  credit_risk = c("Low", "Medium", "High", "Low", "Medium", "Low", "Medium", "High", "High", "Low", "Medium", "High", "Low", "Medium", "High")
)

# View the dataset
print(data)

# Convert categorical variables to factors
data$education <- as.factor(data$education)
data$credit_risk <- as.factor(data$credit_risk)

# Build decision tree model
tree_model <- rpart(credit_risk ~ age + income + education, 
                   data = data,
                   method = "class",
                   control = rpart.control(cp = 0.01))

# Display the decision tree
print(tree_model)

# Plot the decision tree
rpart.plot(tree_model, 
           type = 3,
           extra = 102,
           fallen.leaves = TRUE,
           main = "Credit Risk Decision Tree")

# Make predictions on the training data
predictions <- predict(tree_model, data, type = "class")
print(predictions)

# Create confusion matrix
conf_matrix <- table(Predicted = predictions, Actual = data$credit_risk)
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", round(accuracy * 100, 2), "%\n")

# Predict on new data
new_data <- data.frame(
  age = c(30, 45),
  income = c(70000, 130000),
  education = c("Master", "PhD")
)

# Convert education to factor with same levels as training data
new_data$education <- factor(new_data$education, levels = levels(data$education))

# Make predictions on new data
new_predictions <- predict(tree_model, new_data, type = "class")
print(new_predictions)

# Display tree rules
print(summary(tree_model))
```

## Key Components Explained:

### 1. **Data Preparation**
- Created a sample dataset with age, income, education level, and credit risk
- Converted categorical variables to factors

### 2. **Model Building**
- Used `rpart()` function to build the decision tree
- Specified `method = "class"` for classification
- Set `cp = 0.01` to control tree complexity

### 3. **Visualization**
- Used `rpart.plot()` to create a visual representation
- Shows decision rules and splits

### 4. **Evaluation**
- Confusion matrix to assess performance
- Accuracy calculation
- Predictions on new data

## Expected Output:
The code will display:
- The decision tree structure
- A visual plot of the tree
- Prediction results
- Accuracy metrics
- New predictions

This example demonstrates the complete workflow of decision tree learning in R, from data preparation to model evaluation and prediction.

