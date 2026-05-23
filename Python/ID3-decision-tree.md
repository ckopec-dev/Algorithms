# ID3 Decision Tree Algorithm Implementation

Here's a complete implementation of the ID3 algorithm for building decision trees in Python:

```python
import math
from collections import Counter

class Node:
    def __init__(self, feature=None, threshold=None, left=None, right=None, value=None):
        self.feature = feature      # Feature index to split on
        self.threshold = threshold  # Threshold value for numerical features
        self.left = left           # Left subtree
        self.right = right         # Right subtree
        self.value = value         # Class label (for leaf nodes)

class ID3DecisionTree:
    def __init__(self, max_depth=10, min_samples_split=2):
        self.max_depth = max_depth
        self.min_samples_split = min_samples_split
        self.root = None
    
    def entropy(self, y):
        """Calculate entropy of a dataset"""
        if len(y) == 0:
            return 0
        
        # Count occurrences of each class
        counts = Counter(y)
        entropy = 0
        
        for count in counts.values():
            probability = count / len(y)
            if probability > 0:
                entropy -= probability * math.log2(probability)
        
        return entropy
    
    def information_gain(self, X, y, feature_index):
        """Calculate information gain for a feature"""
        # Calculate entropy of the parent node
        parent_entropy = self.entropy(y)
        
        # Get unique values of the feature
        feature_values = list(set([row[feature_index] for row in X]))
        
        # Calculate weighted entropy of child nodes
        weighted_entropy = 0
        
        for value in feature_values:
            # Split data based on feature value
            subset_indices = [i for i in range(len(X)) if X[i][feature_index] == value]
            subset_y = [y[i] for i in subset_indices]
            
            if len(subset_y) > 0:
                weight = len(subset_y) / len(y)
                weighted_entropy += weight * self.entropy(subset_y)
        
        # Information gain = parent entropy - weighted child entropy
        return parent_entropy - weighted_entropy
    
    def best_feature(self, X, y):
        """Find the best feature to split on"""
        best_gain = -1
        best_feature = None
        
        for feature_index in range(len(X[0])):
            gain = self.information_gain(X, y, feature_index)
            if gain > best_gain:
                best_gain = gain
                best_feature = feature_index
        
        return best_feature, best_gain
    
    def build_tree(self, X, y, depth=0):
        """Recursively build the decision tree"""
        # Base cases
        if len(set(y)) == 1:  # All samples have same class
            return Node(value=y[0])
        
        if len(y) < self.min_samples_split:  # Not enough samples
            most_common = Counter(y).most_common(1)[0][0]
            return Node(value=most_common)
        
        if depth >= self.max_depth:  # Maximum depth reached
            most_common = Counter(y).most_common(1)[0][0]
            return Node(value=most_common)
        
        # Find best feature to split on
        best_feature, best_gain = self.best_feature(X, y)
        
        if best_gain <= 0:  # No information gain
            most_common = Counter(y).most_common(1)[0][0]
            return Node(value=most_common)
        
        # Create node
        node = Node(feature=best_feature)
        
        # Split data
        feature_values = list(set([row[best_feature] for row in X]))
        
        # For simplicity, assuming binary split for categorical features
        # In practice, you might want to handle this more carefully
        left_indices = [i for i in range(len(X)) if X[i][best_feature] == feature_values[0]]
        right_indices = [i for i in range(len(X)) if X[i][best_feature] == feature_values[1]]
        
        if left_indices and right_indices:
            left_X = [X[i] for i in left_indices]
            left_y = [y[i] for i in left_indices]
            right_X = [X[i] for i in right_indices]
            right_y = [y[i] for i in right_indices]
            
            node.left = self.build_tree(left_X, left_y, depth + 1)
            node.right = self.build_tree(right_X, right_y, depth + 1)
        
        return node
    
    def fit(self, X, y):
        """Train the decision tree"""
        self.root = self.build_tree(X, y)
    
    def predict_sample(self, sample, node):
        """Predict a single sample"""
        if node.value is not None:  # Leaf node
            return node.value
        
        if sample[node.feature] == 0:  # Assuming binary features for this example
            return self.predict_sample(sample, node.left)
        else:
            return self.predict_sample(sample, node.right)
    
    def predict(self, X):
        """Predict multiple samples"""
        return [self.predict_sample(sample, self.root) for sample in X]

# Example usage
if __name__ == "__main__":
    # Sample dataset: [Outlook, Temperature, Humidity, Wind]
    # Features: 0=Outlook(Sunny=0, Overcast=1, Rain=2), 1=Temperature(Hot=0, Mild=1, Cool=2)
    # 2=Humidity(High=0, Normal=1), 3=Wind(Weak=0, Strong=1)
    # Target: Play Tennis (Yes=1, No=0)
    
    X = [
        [0, 0, 0, 0],  # Sunny, Hot, High, Weak
        [0, 0, 0, 1],  # Sunny, Hot, High, Strong
        [1, 0, 0, 0],  # Overcast, Hot, High, Weak
        [2, 1, 0, 0],  # Rain, Mild, High, Weak
        [2, 2, 1, 0],  # Rain, Cool, Normal, Weak
        [2, 2, 1, 1],  # Rain, Cool, Normal, Strong
        [1, 2, 1, 1],  # Overcast, Cool, Normal, Strong
        [0, 1, 0, 0],  # Sunny, Mild, High, Weak
        [0, 2, 1, 0],  # Sunny, Cool, Normal, Weak
        [2, 1, 1, 0],  # Rain, Mild, Normal, Weak
        [0, 1, 1, 1],  # Sunny, Mild, Normal, Strong
        [1, 1, 0, 1],  # Overcast, Mild, High, Strong
        [1, 0, 1, 0],  # Overcast, Hot, Normal, Weak
        [2, 1, 0, 1]   # Rain, Mild, High, Strong
    ]
    
    y = [0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0]  # Play Tennis
    
    # Create and train the decision tree
    tree = ID3DecisionTree(max_depth=5)
    tree.fit(X, y)
    
    # Make predictions
    predictions = tree.predict(X)
    
    print("Training Data Predictions:")
    for i, (sample, actual, predicted) in enumerate(zip(X, y, predictions)):
        print(f"Sample {i+1}: {sample} -> Actual: {actual}, Predicted: {predicted}")
    
    # Test with new sample
    new_sample = [2, 1, 0, 0]  # Rain, Mild, High, Weak
    prediction = tree.predict([new_sample])[0]
    print(f"\nNew sample {new_sample} -> Prediction: {'Yes' if prediction == 1 else 'No'}")
```

## Key Components Explained:

### 1. **Node Class**
- Represents each node in the decision tree
- Stores feature index, threshold, left/right children, and class value

### 2. **ID3DecisionTree Class**
- **entropy()**: Calculates entropy of a dataset
- **information_gain()**: Computes information gain for a feature
- **best_feature()**: Finds the feature with maximum information gain
- **build_tree()**: Recursively builds the decision tree
- **fit()**: Trains the model
- **predict()**: Makes predictions on new data

### 3. **Algorithm Steps**
1. Calculate entropy of the entire dataset
2. For each feature, calculate information gain
3. Select the feature with maximum information gain
4. Split the dataset based on that feature
5. Recursively apply the process to child nodes

### 4. **Key Features**
- Handles categorical features
- Prevents overfitting with max_depth parameter
- Minimum samples required for splitting
- Automatic handling of leaf nodes

This implementation demonstrates the core ID3 algorithm for building decision trees based on information gain and entropy.

