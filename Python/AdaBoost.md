# AdaBoost Algorithm Example in Python

Here's a complete implementation of the AdaBoost algorithm from scratch in Python:

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
        w = np.full(n_samples, (1.0 / n_samples))
        
        # For each boosting iteration
        for _ in range(self.n_estimators):
            # Create a weak learner (decision stump)
            stump = DecisionTreeClassifier(max_depth=1)
            stump.fit(X, y, sample_weight=w)
            
            # Make predictions
            predictions = stump.predict(X)
            
            # Calculate error
            error = np.sum(w * (predictions != y)) / np.sum(w)
            
            # Calculate alpha (classifier weight)
            alpha = self.learning_rate * (np.log((1.0 - error) / (error + 1e-10)) / 2.0)
            
            # Update sample weights
            w *= np.exp(-alpha * y * predictions)
            w /= np.sum(w)
            
            # Store the stump and its weight
            self.stumps.append(stump)
            self.alphas.append(alpha)
    
    def predict(self, X):
        # Initialize predictions
        predictions = np.zeros(X.shape[0])
        
        # Combine all weak learners
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

# Train AdaBoost classifier
ada_boost = AdaBoostClassifier(n_estimators=10, learning_rate=1.0)
ada_boost.fit(X_train, y_train)

# Make predictions
y_pred = ada_boost.predict(X_test)

# Calculate accuracy
accuracy = accuracy_score(y_test, y_pred)
print(f"Accuracy: {accuracy:.4f}")

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
plt.scatter(X_test[y_test == -1, 0], X_test[y_test == -1, 1], 
           c='red', marker='o', label='True -1')
plt.scatter(X_test[y_test == 1, 0], X_test[y_test == 1, 1], 
           c='blue', marker='s', label='True 1')

# Plot predictions
y_pred_test = ada_boost.predict(X_test)
plt.scatter(X_test[y_pred_test == -1, 0], X_test[y_pred_test == -1, 1], 
           c='red', marker='x', s=50, label='Pred -1')
plt.scatter(X_test[y_pred_test == 1, 0], X_test[y_pred_test == 1, 1], 
           c='blue', marker='x', s=50, label='Pred 1')

plt.title('AdaBoost Predictions')
plt.xlabel('Feature 1')
plt.ylabel('Feature 2')
plt.legend()

plt.tight_layout()
plt.show()
```

## How AdaBoost Works:

1. **Initialize weights**: All training samples start with equal weights
2. **Train weak learner**: Train a simple classifier (decision stump) on weighted data
3. **Calculate error**: Measure how well the weak learner performs
4. **Compute alpha**: Determine the weight of this weak learner based on its performance
5. **Update weights**: Increase weights of misclassified samples, decrease weights of correctly classified ones
6. **Repeat**: Continue for specified number of iterations

## Key Components:

- **Weak Learners**: Simple classifiers (typically decision stumps with max_depth=1)
- **Alpha values**: Weights that determine how much each weak learner contributes
- **Sample weighting**: Adjusts the importance of samples in subsequent rounds

## Advantages:
- Combines multiple weak learners to create a strong learner
- Handles both binary and multiclass classification
- Robust to overfitting when using simple weak learners

## Disadvantages:
- Sensitive to noisy data and outliers
- Can overfit if too many estimators are used
- Computationally expensive for large datasets

This implementation demonstrates the core principles of AdaBoost while being fully self-contained in Python without external dependencies beyond numpy and scikit-learn's DecisionTreeClassifier for creating weak learners.

