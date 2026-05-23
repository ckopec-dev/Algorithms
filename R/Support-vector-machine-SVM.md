# Support Vector Machine (SVM) in R

Here's a complete example of implementing SVM using the `e1071` package in R:

```r
# Install and load required packages
install.packages(c("e1071", "caret", "ggplot2"))
library(e1071)
library(caret)
library(ggplot2)

# Load built-in dataset
data(iris)

# Explore the dataset
head(iris)
str(iris)
summary(iris)

# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
train_data <- iris[trainIndex, ]
test_data <- iris[-trainIndex, ]

# Build SVM model
svm_model <- svm(Species ~ ., 
                 data = train_data,
                 kernel = "radial",  # radial (RBF) kernel
                 cost = 1,           # complexity parameter
                 gamma = 0.5)        # kernel coefficient

# Display model summary
print(svm_model)

# Make predictions
predictions <- predict(svm_model, test_data)

# Create confusion matrix
conf_matrix <- confusionMatrix(predictions, test_data$Species)
print(conf_matrix)

# Visualize results
# Plot Sepal Length vs Sepal Width
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 3) +
  labs(title = "Iris Dataset - Sepal Dimensions",
       x = "Sepal Length", 
       y = "Sepal Width") +
  theme_minimal()

# Cross-validation for model evaluation
svm_cv <- train(Species ~ ., 
                data = train_data,
                method = "svmRadial",
                trControl = trainControl(method = "cv", number = 5),
                tuneLength = 5)

# Display cross-validation results
print(svm_cv)

# Plot cross-validation results
plot(svm_cv)

# Tune hyperparameters
tuned_svm <- tune(svm, Species ~ ., 
                  data = train_data,
                  kernel = "radial",
                  ranges = list(cost = c(0.1, 1, 10, 100),
                               gamma = c(0.01, 0.1, 1, 10)))

# Display best parameters
print(tuned_svm$best.parameters)
print(tuned_svm$best.model)

# Final model with tuned parameters
final_svm <- svm(Species ~ ., 
                 data = train_data,
                 kernel = "radial",
                 cost = 1,
                 gamma = 0.1)

# Final predictions and evaluation
final_predictions <- predict(final_svm, test_data)
final_conf_matrix <- confusionMatrix(final_predictions, test_data$Species)
print(final_conf_matrix)
```

## Key Components Explained:

### 1. **Model Parameters:**
- `kernel`: Type of kernel function ("linear", "polynomial", "radial", "sigmoid")
- `cost`: Regularization parameter (higher values = less regularization)
- `gamma`: Kernel coefficient (affects the influence of single training examples)

### 2. **Model Evaluation:**
- Confusion matrix for accuracy assessment
- Cross-validation for robust performance estimation
- Hyperparameter tuning for optimal performance

### 3. **Important Functions:**
- `svm()`: Main function to build SVM model
- `predict()`: Make predictions on new data
- `confusionMatrix()`: Evaluate model performance
- `train()`: Cross-validation with `caret` package
- `tune()`: Hyperparameter optimization

This example demonstrates a complete SVM workflow including data preparation, model building, evaluation, and optimization for the classic Iris dataset classification problem.

