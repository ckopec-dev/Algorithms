# ID3 Decision Tree Algorithm in R

Here's a complete example of implementing the ID3 decision tree algorithm in R:

```r
# Load required libraries
library(rpart)
library(rpart.plot)

# Create sample dataset
data <- data.frame(
  Outlook = c("Sunny", "Sunny", "Overcast", "Rain", "Rain", "Rain", "Overcast", 
              "Sunny", "Sunny", "Rain", "Sunny", "Overcast", "Overcast", "Rain"),
  Temperature = c("Hot", "Hot", "Hot", "Mild", "Cool", "Cool", "Cool", 
                  "Mild", "Cool", "Mild", "Mild", "Mild", "Hot", "Mild"),
  Humidity = c("High", "High", "High", "High", "Normal", "Normal", "Normal", 
               "High", "Normal", "Normal", "Normal", "High", "Normal", "High"),
  Wind = c("Weak", "Strong", "Weak", "Weak", "Weak", "Strong", "Strong", 
           "Weak", "Weak", "Weak", "Strong", "Strong", "Weak", "Strong"),
  PlayTennis = c("No", "No", "Yes", "Yes", "Yes", "No", "Yes", "No", 
                 "Yes", "Yes", "Yes", "Yes", "Yes", "No")
)

# Convert to factors
data$Outlook <- as.factor(data$Outlook)
data$Temperature <- as.factor(data$Temperature)
data$Humidity <- as.factor(data$Humidity)
data$Wind <- as.factor(data$Wind)
data$PlayTennis <- as.factor(data$PlayTennis)

# Display the dataset
print("Dataset:")
print(data)

# Build ID3 decision tree using rpart with ID3-like parameters
# Note: rpart doesn't directly implement ID3, but we can approximate it
tree_model <- rpart(PlayTennis ~ ., 
                   data = data,
                   method = "class",
                   control = rpart.control(
                     cp = 0.0,           # No pruning
                     minsplit = 2,       # Minimum observations for split
                     minbucket = 1,      # Minimum observations in leaf
                     maxdepth = 30       # Maximum depth
                   ))

# Display the tree
print("Decision Tree:")
print(tree_model)

# Plot the decision tree
rpart.plot(tree_model, 
           type = 3,
           extra = 102,
           fallen.leaves = TRUE,
           main = "ID3 Decision Tree - Play Tennis")

# Make predictions on the training data
predictions <- predict(tree_model, data, type = "class")
print("Predictions:")
print(predictions)

# Calculate accuracy
accuracy <- sum(predictions == data$PlayTennis) / length(data$PlayTennis)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

# Create a more detailed implementation of ID3 algorithm
# This is a simplified version of ID3 implementation

# Function to calculate entropy
entropy <- function(y) {
  if (length(y) == 0) return(0)
  
  # Calculate proportions
  probs <- table(y) / length(y)
  
  # Calculate entropy
  entropy_val <- -sum(probs * log2(probs))
  return(entropy_val)
}

# Function to calculate information gain
information_gain <- function(data, attribute, target) {
  # Calculate total entropy
  total_entropy <- entropy(data[[target]])
  
  # Calculate weighted entropy after split
  weighted_entropy <- 0
  n <- nrow(data)
  
  # Get unique values of the attribute
  values <- unique(data[[attribute]])
  
  for (value in values) {
    subset <- data[data[[attribute]] == value, ]
    weight <- nrow(subset) / n
    weighted_entropy <- weighted_entropy + weight * entropy(subset[[target]])
  }
  
  # Information gain is the difference
  gain <- total_entropy - weighted_entropy
  return(gain)
}

# Function to find best attribute for splitting
best_attribute <- function(data, target) {
  attributes <- names(data)[names(data) != target]
  gains <- sapply(attributes, function(attr) information_gain(data, attr, target))
  return(names(which.max(gains)))
}

# Example usage of custom ID3 functions
print("Custom ID3 Implementation:")
print(paste("Best attribute for splitting:", best_attribute(data, "PlayTennis")))

# Show the dataset structure
print("Dataset Structure:")
str(data)
```

## Expected Output:

```
Dataset:
   Outlook Temperature Humidity Wind PlayTennis
1    Sunny         Hot     High Weak         No
2    Sunny         Hot     High Strong      No
3 Overcast         Hot     High Weak        Yes
4     Rain       Mild     High Weak        Yes
5     Rain       Cool   Normal Weak        Yes
6     Rain       Cool   Normal Strong       No
7 Overcast       Cool   Normal Strong        Yes
8    Sunny       Mild     High Weak         No
9    Sunny       Cool   Normal Weak        Yes
10    Rain       Cool   Normal Weak        Yes
11   Sunny       Mild   Normal Strong        Yes
12 Overcast       Mild     High Strong        Yes
13 Overcast         Hot   Normal Weak        Yes
14    Rain       Mild     High Strong       No

Accuracy: 100 %
```

## Key Features of This Implementation:

1. **Data Preparation**: Creates a sample dataset with weather conditions and tennis playing decisions
2. **Tree Building**: Uses `rpart` package to build a decision tree (approximating ID3 behavior)
3. **Visualization**: Plots the decision tree using `rpart.plot`
4. **Evaluation**: Calculates accuracy of predictions
5. **Custom Functions**: Implements entropy and information gain calculations
6. **Attribute Selection**: Identifies the best attribute for splitting using ID3 criteria

## How ID3 Works:

1. **Calculate Entropy**: Measure the impurity of the dataset
2. **Calculate Information Gain**: For each attribute, calculate how much information is gained by splitting on that attribute
3. **Select Best Attribute**: Choose the attribute with maximum information gain
4. **Split Dataset**: Divide the dataset based on the selected attribute
5. **Repeat**: Recursively apply the process to each subset until all instances are pure or no more attributes remain

This implementation demonstrates both the practical application using R's built-in packages and the theoretical foundations of the ID3 algorithm.

