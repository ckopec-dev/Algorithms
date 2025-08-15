# AdaBoost Algorithm Example in Python

## What is AdaBoost?

AdaBoost (Adaptive Boosting) is a machine learning algorithm that combines multiple weak learners to create a strong learner. It works by iteratively training weak classifiers and adjusting the weights of misclassified samples.

## Implementation Example

```python
import numpy as np
import matplotlib.pyplot as plt
from sklearn.tree import DecisionTreeClassifier
from sklearn.datasets import make_classification
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score

class AdaBoostClassifier:
    def __init__(self, n_estimators=50, learning_rate=1.0):
        self.n_estimators = n_estimators
        self.learning_rate = learning_rate
        self.models = []
        self.model_weights = []
        
    def fit(self, X, y):
        # Initialize sample weights
        n_samples = X.shape[0]
        weights = np.ones(n_samples) / n_samples
        
        for _ in range(self.n_estimators):
            # Create a weak learner (decision tree with depth 1)
            weak_learner = DecisionTreeClassifier(max_depth=1)
            weak_learner.fit(X, y, sample_weight=weights)
            
            # Make predictions
            predictions = weak_learner.predict(X)
            
            # Calculate error
            error = np.sum(weights * (predictions != y)) / np.sum(weights)
            
            # Calculate model weight
            model_weight = self.learning_rate * np.log((1 - error) / (error + 1e-10))
            
            # Update sample weights
            weights *= np.exp(-model_weight * y * predictions)
            weights /= np.sum(weights)
            
            # Store the weak learner and its weight
            self.models.append(weak_learner)
            self.model_weights.append(model_weight)
    
    def predict(self, X):
        # Initialize predictions
        predictions = np.zeros(X.shape[0])
        
        for model, weight in zip(self.models, self.model_weights):
            predictions += weight * model.predict(X)
            
        # Return final prediction (sign of sum)
        return np.sign(predictions)

# Generate sample data
X, y = make_classification(n_samples=1000, n_features=2, n_redundant=0, 
                          n_informative=2, n_clusters_per_class=1, 
                          random_state=42)

# Convert labels to -1 and 1 for AdaBoost
y = np.where(y == 0, -1, 1)

# Split the data
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)

# Train AdaBoost
ada_boost = AdaBoostClassifier(n_estimators=50, learning_rate=1.0)
ada_boost.fit(X_train, y_train)

# Make predictions
y_pred = ada_boost.predict(X_test)

# Calculate accuracy
accuracy = accuracy_score(y_test, y_pred)
print(f"AdaBoost Accuracy: {accuracy:.4f}")

# Compare with sklearn's AdaBoost
from sklearn.ensemble import AdaBoostClassifier as SklearnAdaBoost

sklearn_ada = SklearnAdaBoost(n_estimators=50, learning_rate=1.0, random_state=42)
sklearn_ada.fit(X_train, y_train)
sklearn_pred = sklearn_ada.predict(X_test)
sklearn_accuracy = accuracy_score(y_test, sklearn_pred)

print(f"Sklearn AdaBoost Accuracy: {sklearn_accuracy:.4f}")
```

## How it works step by step:

1. **Initialize weights**: All samples start with equal weights
2. **Train weak learner**: Train a simple classifier (like decision stump)
3. **Calculate error**: Measure how well the weak learner performs
4. **Update weights**: Increase weights of misclassified samples
5. **Combine learners**: Weight each weak learner based on its performance
6. **Repeat**: Go back to step 2 for multiple iterations

## Key Parameters:

- `n_estimators`: Number of weak learners (default: 50)
- `learning_rate`: Step size (default: 1.0)

## Advantages:
- Combines weak learners into strong ones
- Handles both classification and regression
- Reduces overfitting compared to single decision trees

## Disadvantages:
- Sensitive to noisy data
- Can overfit with too many estimators
- Computationally expensive for large datasets

This implementation demonstrates the core concept of AdaBoost by creating a custom class that mimics the sklearn version's functionality.

