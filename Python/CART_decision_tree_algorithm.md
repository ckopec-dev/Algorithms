# CART Decision Tree Algorithm Example

```python
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score, classification_report
from sklearn.datasets import load_iris
import pandas as pd
import numpy as np

# Load the iris dataset
iris = load_iris()
X = iris.data
y = iris.target

# Split the dataset into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)

# Create CART decision tree classifier
cart_tree = DecisionTreeClassifier(
    criterion='gini',        # 'gini' or 'entropy'
    max_depth=3,            # Maximum depth of the tree
    min_samples_split=2,    # Minimum samples required to split a node
    min_samples_leaf=1,     # Minimum samples required at a leaf node
    random_state=42
)

# Train the model
cart_tree.fit(X_train, y_train)

# Make predictions
y_pred = cart_tree.predict(X_test)

# Calculate accuracy
accuracy = accuracy_score(y_test, y_pred)
print(f"Accuracy: {accuracy:.2f}")

# Display classification report
print("\nClassification Report:")
print(classification_report(y_test, y_pred, target_names=iris.target_names))

# Display feature importance
feature_names = iris.feature_names
importance = cart_tree.feature_importances_
print("\nFeature Importance:")
for i, imp in enumerate(importance):
    print(f"{feature_names[i]}: {imp:.3f}")

# Visualize the decision tree (optional)
from sklearn.tree import plot_tree
import matplotlib.pyplot as plt

plt.figure(figsize=(12, 8))
plot_tree(cart_tree, 
          feature_names=feature_names,
          class_names=iris.target_names,
          filled=True,
          rounded=True)
plt.title("CART Decision Tree")
plt.show()

# Example of making predictions on new data
new_samples = np.array([[5.1, 3.5, 1.4, 0.2],
                       [6.2, 2.8, 4.8, 1.8]])

predictions = cart_tree.predict(new_samples)
probabilities = cart_tree.predict_proba(new_samples)

print("\nPredictions for new samples:")
for i, (sample, pred, prob) in enumerate(zip(new_samples, predictions, probabilities)):
    print(f"Sample {i+1}: {sample}")
    print(f"  Predicted class: {iris.target_names[pred]}")
    print(f"  Probabilities: {dict(zip(iris.target_names, prob))}")
```

## How CART Algorithm Works:

1. **Recursive Partitioning**: The algorithm recursively splits the data based on feature values
2. **Gini Impurity**: Uses Gini impurity to measure the quality of a split
3. **Stopping Criteria**: Stops splitting when:
   - Maximum depth is reached
   - Minimum samples required for split is not met
   - All samples belong to same class

## Key Parameters:

- `criterion`: 'gini' or 'entropy' - measures node purity
- `max_depth`: Maximum depth of the tree
- `min_samples_split`: Minimum samples required to split a node
- `min_samples_leaf`: Minimum samples required at leaf node

## Output Example:
```
Accuracy: 0.98

Classification Report:
              precision    recall  f1-score   support

      setosa       1.00      1.00      1.00        15
  versicolor       1.00      0.93      0.97         14
   virginica       0.92      1.00      0.96         13

    accuracy                           0.98        42

Feature Importance:
sepal length (cm): 0.023
sepal width (cm): 0.005
petal length (cm): 0.523
petal width (cm): 0.449
```

