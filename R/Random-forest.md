# Random Forest Example in R

Here's a complete example of implementing Random Forest algorithm in R using the `randomForest` package:

```r
# Install and load required packages
install.packages(c("randomForest", "caret"))
library(randomForest)
library(caret)

# Load built-in dataset
data(iris)

# View the structure of the dataset
str(iris)
head(iris)

# Split data into training and testing sets (70% train, 30% test)
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
train_data <- iris[trainIndex, ]
test_data <- iris[-trainIndex, ]

# Build Random Forest model
rf_model <- randomForest(Species ~ ., 
                        data = train_data,
                        ntree = 500,        # Number of trees
                        mtry = 2,           # Number of variables at each split
                        importance = TRUE,  # Calculate variable importance
                        proximity = TRUE)   # Calculate proximity matrix

# View model results
print(rf_model)

# Make predictions
predictions <- predict(rf_model, test_data)

# Create confusion matrix
confusion_matrix <- confusionMatrix(predictions, test_data$Species)
print(confusion_matrix)

# View variable importance
importance(rf_model)
varImpPlot(rf_model)

# Plot model performance
plot(rf_model)

# Cross-validation to assess model performance
set.seed(123)
cv_model <- train(Species ~ ., 
                  data = train_data,
                  method = "rf",
                  trControl = trainControl(method = "cv", number = 10),
                  tuneLength = 5)

print(cv_model)
```

## Key Parameters Explained:

- **`ntree`**: Number of trees to grow (default is 500)
- **`mtry`**: Number of variables randomly sampled as candidates at each split
- **`importance`**: If TRUE, calculates variable importance measures
- **`proximity`**: If TRUE, calculates proximity matrix

## Output Interpretation:

The Random Forest model will provide:
- **OOB (Out-of-Bag) error rate**: Estimate of model performance
- **Variable importance**: How much each variable contributes to the model
- **Confusion matrix**: Performance metrics for classification
- **Predictions**: Class predictions for test data

This example demonstrates how to build, train, and evaluate a Random Forest classifier for the classic iris dataset.

