# CART Decision Tree Algorithm Example

Here's a complete example of implementing a CART (Classification and Regression Trees) decision tree algorithm in Python:

```python
import numpy as np
import pandas as pd
from collections import Counter
import matplotlib.pyplot as plt
from sklearn.datasets import load_iris
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import accuracy_score, classification_report

# Load sample dataset
iris = load_iris()
X = iris.data
y = iris.target
feature_names = iris.feature_names
target_names = iris.target_names

# Split the dataset
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)

# Create CART decision tree classifier
cart_model = DecisionTreeClassifier(
    criterion='gini',        # 'gini' or 'entropy'
    max_depth=3,            # Maximum depth of the tree
    min_samples_split=2,    # Minimum samples required to split
    min_samples_leaf=1,     # Minimum samples required at leaf node
    random_state=42
)

# Train the model
cart_model.fit(X_train, y_train)

# Make predictions
y_pred = cart_model.predict(X_test)

# Calculate accuracy
accuracy = accuracy_score(y_test, y_pred)
print(f"Accuracy: {accuracy:.4f}")
print("\nClassification Report:")
print(classification_report(y_test, y_pred, target_names=target_names))

# Feature importance
feature_importance = cart_model.feature_importances_
print("\nFeature Importance:")
for i, importance in enumerate(feature_importance):
    print(f"{feature_names[i]}: {importance:.4f}")

# Visualize the decision tree (optional)
from sklearn.tree import plot_tree

plt.figure(figsize=(12, 8))
plot_tree(cart_model, 
          feature_names=feature_names,
          class_names=target_names,
          filled=True,
          rounded=True,
          fontsize=10)
plt.title("CART Decision Tree")
plt.show()

# Manual implementation of CART algorithm (simplified version)
class SimpleCART:
    def __init__(self, max_depth=3, min_samples_split=2):
        self.max_depth = max_depth
        self.min_samples_split = min_samples_split
        self.tree = None
    
    def gini_impurity(self, y):
        """Calculate Gini impurity"""
        if len(y) == 0:
            return 0
        counts = Counter(y)
        probabilities = [count/len(y) for count in counts.values()]
        return 1 - sum(p**2 for p in probabilities)
    
    def best_split(self, X, y):
        """Find the best split point"""
        best_gini = float('inf')
        best_feature = None
        best_threshold = None
        
        n_features = X.shape[1]
        
        for feature in range(n_features):
            thresholds = np.unique(X[:, feature])
            
            for threshold in thresholds:
                left_mask = X[:, feature] <= threshold
                right_mask = ~left_mask
                
                if np.sum(left_mask) == 0 or np.sum(right_mask) == 0:
                    continue
                
                left_y = y[left_mask]
                right_y = y[right_mask]
                
                weighted_gini = (len(left_y)/len(y)) * self.gini_impurity(left_y) + \
                               (len(right_y)/len(y)) * self.gini_impurity(right_y)
                
                if weighted_gini < best_gini:
                    best_gini = weighted_gini
                    best_feature = feature
                    best_threshold = threshold
        
        return best_feature, best_threshold
    
    def build_tree(self, X, y, depth=0):
        """Recursively build the decision tree"""
        n_samples, n_features = X.shape
        n_classes = len(np.unique(y))
        
        # Stopping criteria
        if (depth >= self.max_depth or 
            n_classes == 1 or 
            n_samples < self.min_samples_split):
            # Return leaf node with most common class
            most_common = Counter(y).most_common(1)[0][0]
            return {'class': most_common}
        
        # Find best split
        feature, threshold = self.best_split(X, y)
        
        if feature is None:
            # Return leaf node with most common class
            most_common = Counter(y).most_common(1)[0][0]
            return {'class': most_common}
        
        # Split the data
        left_mask = X[:, feature] <= threshold
        right_mask = ~left_mask
        
        left_tree = self.build_tree(X[left_mask], y[left_mask], depth + 1)
        right_tree = self.build_tree(X[right_mask], y[right_mask], depth + 1)
        
        return {
            'feature': feature,
            'threshold': threshold,
            'left': left_tree,
            'right': right_tree
        }
    
    def fit(self, X, y):
        """Train the CART model"""
        self.tree = self.build_tree(X, y)
    
    def predict_sample(self, sample, tree):
        """Predict a single sample"""
        if 'class' in tree:
            return tree['class']
        
        if sample[tree['feature']] <= tree['threshold']:
            return self.predict_sample(sample, tree['left'])
        else:
            return self.predict_sample(sample, tree['right'])
    
    def predict(self, X):
        """Predict multiple samples"""
        return [self.predict_sample(sample, self.tree) for sample in X]

# Example usage of manual implementation
print("\n" + "="*50)
print("Manual CART Implementation Example")
print("="*50)

# Create and train manual CART model
manual_cart = SimpleCART(max_depth=3, min_samples_split=2)
manual_cart.fit(X_train, y_train)

# Make predictions
manual_predictions = manual_cart.predict(X_test)
manual_accuracy = accuracy_score(y_test, manual_predictions)
print(f"Manual CART Accuracy: {manual_accuracy:.4f}")

# Compare with sklearn implementation
print(f"Sklearn CART Accuracy: {accuracy:.4f}")
```

## Key Features of CART Algorithm:

### 1. **Gini Impurity**: Measures the likelihood of incorrect classification
### 2. **Binary Splits**: Each node splits into exactly two child nodes
### 3. **Pruning**: Prevents overfitting through maximum depth and sample thresholds
### 4. **Feature Selection**: Chooses best feature and threshold for splitting

### 5. **Parameters**:
- `criterion`: 'gini' or 'entropy' for impurity measure
- `max_depth`: Maximum depth of the tree
- `min_samples_split`: Minimum samples required to split a node
- `min_samples_leaf`: Minimum samples required at leaf node

### 6. **Advantages**:
- Easy to understand and interpret
- No need for feature scaling
- Handles both classification and regression
- Automatically performs feature selection

### 7. **Disadvantages**:
- Prone to overfitting
- Can be unstable with small changes in data
- May not perform well with complex datasets

This example demonstrates both the scikit-learn implementation and a simplified manual implementation of the CART algorithm.

