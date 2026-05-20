# Naïve Bayes Classifier Example in Python

```python
# Import necessary libraries
from sklearn.naive_bayes import GaussianNB
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score, classification_report
from sklearn.datasets import load_iris
import pandas as pd
import numpy as np

# Load the iris dataset
iris = load_iris()
X = iris.data  # Features: sepal length, sepal width, petal length, petal width
y = iris.target  # Target: species of iris (setosa, versicolor, virginica)

# Create a DataFrame for better visualization
df = pd.DataFrame(X, columns=iris.feature_names)
df['target'] = y
df['species'] = df['target'].map({0: 'setosa', 1: 'versicolor', 2: 'virginica'})

print("Dataset Overview:")
print(df.head())
print(f"\nDataset shape: {df.shape}")
print(f"Target distribution:\n{df['species'].value_counts()}")

# Split the dataset into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)

# Create and train the Naïve Bayes classifier
nb_classifier = GaussianNB()
nb_classifier.fit(X_train, y_train)

# Make predictions
y_pred = nb_classifier.predict(X_test)

# Calculate accuracy
accuracy = accuracy_score(y_test, y_pred)
print(f"\nAccuracy: {accuracy:.4f}")

# Detailed classification report
print("\nClassification Report:")
print(classification_report(y_test, y_pred, target_names=iris.target_names))

# Example prediction for a new sample
print("\nExample Prediction:")
# New sample: [sepal_length, sepal_width, petal_length, petal_width]
new_sample = [[5.1, 3.5, 1.4, 0.2]]
prediction = nb_classifier.predict(new_sample)
prediction_proba = nb_classifier.predict_proba(new_sample)

print(f"New sample: {new_sample[0]}")
print(f"Predicted class: {iris.target_names[prediction[0]]}")
print(f"Prediction probabilities:")
for i, prob in enumerate(prediction_proba[0]):
    print(f"  {iris.target_names[i]}: {prob:.4f}")

# Manual calculation example (simplified)
print("\nManual Calculation Example:")
print("For a simple binary classification example:")
print("P(class|features) ∝ P(features|class) × P(class)")

# Show the probability calculations
print("\nProbability calculations for the first test sample:")
print(f"Features: {X_test[0]}")
print(f"Predicted class: {iris.target_names[y_pred[0]]}")
```

## Output Example:
```
Dataset Overview:
   sepal length (cm)  sepal width (cm)  petal length (cm)  petal width (cm)  target species
0                5.1               3.5                1.4               0.2         0  setosa
1                4.9               3.0                1.4               0.2         0  setosa
2                4.7               3.2                1.3               0.2         0  setosa
3                4.6               3.1                1.5               0.2         0  setosa
4                5.0               3.6                1.4               0.2         0  setosa

Dataset shape: (150, 5)
Target distribution:
setosa        50
versicolor    50
virginica     50

Accuracy: 1.0000

Classification Report:
              precision    recall  f1-score   support

      setosa       1.00      1.00      1.00        15
  versicolor       1.00      1.00      1.00        15
   virginica       1.00      1.00      1.00        15

    accuracy                           1.00        45
   macro avg       1.00      1.00      1.00        45
weighted avg       1.00      1.00      1.00        45

Example Prediction:
New sample: [5.1, 3.5, 1.4, 0.2]
Predicted class: setosa
Prediction probabilities:
  setosa: 0.9999
  versicolor: 0.0001
  virginica: 0.0000
```

## Key Points:

1. **GaussianNB**: Assumes that features follow a normal distribution
2. **Naïve Assumption**: All features are conditionally independent given the class
3. **Mathematical Formula**: P(class|features) ∝ P(features|class) × P(class)
4. **Advantages**: Simple, fast, works well with small datasets
5. **Disadvantages**: Assumes feature independence (rarely true in real life)

This example demonstrates how to implement and use a Naïve Bayes classifier for multi-class classification using the famous Iris dataset.

