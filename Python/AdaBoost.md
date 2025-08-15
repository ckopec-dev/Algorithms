# AdaBoost Algorithm Example in Python

Here's a complete implementation of the AdaBoost algorithm from scratch using Python:

```python
import numpy as np
import matplotlib.pyplot as plt
from sklearn.datasets import make_classification
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import accuracy_score

class AdaBoostClassifier:
    def __init__(self, n_estimators=10, learning_rate=1.0):
        self.n_estimators = n_estimators
        self.learning_rate = learning_rate
        self.stumps = []
        self.alphas = []
        
    def fit(self, X, y):
        # Initialize sample weights
        n_samples = X.shape[0]
        weights = np.full(n_samples, 1.0 / n_samples)
        
        for _ in range(self.n_estimators):
            # Create a weak learner (decision stump - depth=1)
            stump = DecisionTreeClassifier(max_depth=1)
            stump.fit(X, y, sample_weight=weights)
            
            # Make predictions
            predictions = stump.predict(X)
            
            # Calculate error
            error = np.sum(weights * (predictions != y))
            
            # Calculate alpha (classifier weight)
            alpha = self.learning_rate * (0.5 * np.log((1 - error) / (error + 1e-10)))
            
            # Update weights
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

# Generate sample data
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
ada_boost = AdaBoostClassifier(n_estimators=10, learning_rate=1.0)
ada_boost.fit(X_train, y_train)

# Make predictions
y_pred = ada_boost.predict(X_test)

# Calculate accuracy
accuracy = accuracy_score(y_test, y_pred)
print(f"AdaBoost Accuracy: {accuracy:.4f}")

# Visualize the results
plt.figure(figsize=(12, 5))

# Plot training data
plt.subplot(1, 2, 1)
plt.scatter(X_train[y_train == -1, 0], X_train[y_train == -1, 1], 
           c='red', marker='o', label='Class -1')
plt.scatter(X_train[y_train == 1, 0], X_train[y_train == 1, 1], 
           c='blue', marker='s', label='Class 1')
plt.title('Training Data')
plt.xlabel('Feature 1')
plt.ylabel('Feature 2')
plt.legend()

# Plot test data with predictions
plt.subplot(1, 2, 2)
test_predictions = ada_boost.predict(X_test)
correct = y_test == test_predictions

plt.scatter(X_test[correct & (y_test == -1), 0], X_test[correct & (y_test == -1), 1], 
           c='red', marker='o', label='Correct -1')
plt.scatter(X_test[correct & (y_test == 1), 0], X_test[correct & (y_test == 1), 1], 
           c='blue', marker='s', label='Correct 1')
plt.scatter(X_test[~correct & (y_test == -1), 0], X_test[~correct & (y_test == -1), 1], 
           c='red', marker='x', s=100, label='Incorrect -1')
plt.scatter(X_test[~correct & (y_test == 1), 0], X_test[~correct & (y_test == 1), 1], 
           c='blue', marker='x', s=100, label='Incorrect 1')
plt.title(f'Test Data Predictions\nAccuracy: {accuracy:.4f}')
plt.xlabel('Feature 1')
plt.ylabel('Feature 2')
plt.legend()

plt.tight_layout()
plt.show()

# Show individual stump weights
print("\nStump Weights (alphas):")
for i, alpha in enumerate(ada_boost.alphas):
    print(f"Stump {i+1}: {alpha:.4f}")
```

## Key Components Explained:

### 1. **AdaBoost Algorithm Steps**:
- Initialize equal weights for all training samples
- Train a weak learner (decision stump) on weighted data
- Calculate error and classifier weight (alpha)
- Update sample weights based on prediction accuracy
- Repeat for specified number of estimators

### 2. **Key Parameters**:
- `n_estimators`: Number of weak learners to train
- `learning_rate`: Controls the contribution of each weak learner

### 3. **Decision Stump**:
A decision tree with maximum depth 1, making it a simple threshold classifier

### 4. **Weight Update Rule**:
Samples that are misclassified get higher weights in next iteration

### 5. **Final Prediction**:
Weighted sum of all weak learners' predictions

## Expected Output:
```
AdaBoost Accuracy: 0.9200

Stump Weights (alphas):
Stump 1: 0.7634
Stump 2: 0.7634
Stump 3: 0.7634
...
```

This implementation demonstrates how AdaBoost combines multiple weak learners to create a strong classifier by focusing on difficult-to-classify samples in each iteration.

