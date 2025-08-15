# AdaBoost Algorithm Example in Python

Here's a complete implementation of the AdaBoost algorithm from scratch in Python:

```python
import numpy as np
import matplotlib.pyplot as plt
from sklearn.datasets import make_classification
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import accuracy_score

class AdaBoostClassifier:
    def __init__(self, n_estimators=50, learning_rate=1.0):
        self.n_estimators = n_estimators
        self.learning_rate = learning_rate
        self.stumps = []
        self.alphas = []
        
    def fit(self, X, y):
        # Initialize sample weights
        n_samples = X.shape[0]
        weights = np.ones(n_samples) / n_samples
        
        # For each boosting iteration
        for _ in range(self.n_estimators):
            # Create a weak learner (decision stump)
            stump = DecisionTreeClassifier(max_depth=1)
            stump.fit(X, y, sample_weight=weights)
            
            # Make predictions
            predictions = stump.predict(X)
            
            # Calculate error
            error = np.sum(weights * (predictions != y)) / np.sum(weights)
            
            # Calculate alpha (classifier weight)
            alpha = self.learning_rate * (np.log((1 - error) / (error + 1e-10)))
            
            # Update sample weights
            weights *= np.exp(-alpha * y * predictions)
            weights /= np.sum(weights)
            
            # Store the stump and its weight
            self.stumps.append(stump)
            self.alphas.append(alpha)
    
    def predict(self, X):
        # Initialize predictions
        predictions = np.zeros(X.shape[0])
        
        # Sum weighted predictions from all stumps
        for alpha, stump in zip(self.alphas, self.stumps):
            predictions += alpha * stump.predict(X)
            
        # Return final prediction (sign of sum)
        return np.sign(predictions)

# Generate sample dataset
X, y = make_classification(n_samples=1000, n_features=2, n_redundant=0, 
                          n_informative=2, n_clusters_per_class=1, 
                          random_state=42)

# Convert labels to -1 and 1
y = np.where(y == 0, -1, 1)

# Split data
split_idx = int(0.8 * len(X))
X_train, X_test = X[:split_idx], X[split_idx:]
y_train, y_test = y[:split_idx], y[split_idx:]

# Train AdaBoost
ada_boost = AdaBoostClassifier(n_estimators=50, learning_rate=1.0)
ada_boost.fit(X_train, y_train)

# Make predictions
predictions = ada_boost.predict(X_test)

# Calculate accuracy
accuracy = accuracy_score(y_test, predictions)
print(f"AdaBoost Accuracy: {accuracy:.4f}")

# Visualize results
plt.figure(figsize=(12, 5))

# Plot 1: Training data
plt.subplot(1, 2, 1)
plt.scatter(X_train[y_train == -1, 0], X_train[y_train == -1, 1], 
           c='red', marker='o', label='Class -1', alpha=0.7)
plt.scatter(X_train[y_train == 1, 0], X_train[y_train == 1, 1], 
           c='blue', marker='s', label='Class 1', alpha=0.7)
plt.title('Training Data')
plt.xlabel('Feature 1')
plt.ylabel('Feature 2')
plt.legend()

# Plot 2: Test predictions
plt.subplot(1, 2, 2)
test_predictions = ada_boost.predict(X_test)
plt.scatter(X_test[test_predictions == -1, 0], X_test[test_predictions == -1, 1], 
           c='red', marker='o', label='Predicted -1', alpha=0.7)
plt.scatter(X_test[test_predictions == 1, 0], X_test[test_predictions == 1, 1], 
           c='blue', marker='s', label='Predicted 1', alpha=0.7)
plt.title('AdaBoost Predictions')
plt.xlabel('Feature 1')
plt.ylabel('Feature 2')
plt.legend()

plt.tight_layout()
plt.show()

# Compare with sklearn's AdaBoost
from sklearn.ensemble import AdaBoostClassifier as SklearnAdaBoost

sklearn_ada = SklearnAdaBoost(n_estimators=50, learning_rate=1.0, random_state=42)
sklearn_ada.fit(X_train, y_train)
sklearn_predictions = sklearn_ada.predict(X_test)
sklearn_accuracy = accuracy_score(y_test, sklearn_predictions)

print(f"\nComparison:")
print(f"Custom AdaBoost Accuracy: {accuracy:.4f}")
print(f"Sklearn AdaBoost Accuracy: {sklearn_accuracy:.4f}")
```

## Key Components Explained:

1. **Weak Learners**: Uses decision stumps (decision trees with depth=1)
2. **Weighted Sampling**: Each sample has a weight that gets updated after each iteration
3. **Alpha Calculation**: Determines the importance of each weak learner based on its error
4. **Prediction Aggregation**: Final prediction is a weighted sum of all weak learners

## How AdaBoost Works:

1. Initialize equal weights for all training samples
2. For each iteration:
   - Train a weak learner on weighted data
   - Calculate the learner's error rate
   - Compute the learner's weight (alpha)
   - Update sample weights based on prediction correctness
3. Combine all learners with their respective weights

## Advantages:
- Combines multiple weak learners to create a strong learner
- Handles both binary and multiclass classification
- Robust to overfitting when using shallow decision trees

## Disadvantages:
- Sensitive to noisy data and outliers
- Can be computationally expensive for large datasets
- May overfit with too many estimators

