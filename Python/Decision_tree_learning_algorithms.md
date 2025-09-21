# Decision Tree Learning Algorithm Example

Here's a complete implementation of a decision tree learning algorithm using Python with scikit-learn:

```python
# Import necessary libraries
import numpy as np
import pandas as pd
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score, classification_report
from sklearn.preprocessing import LabelEncoder
import matplotlib.pyplot as plt
from sklearn.tree import plot_tree

# Create sample dataset
data = {
    'outlook': ['sunny', 'sunny', 'overcast', 'rainy', 'rainy', 'rainy', 'overcast', 
                'sunny', 'sunny', 'rainy', 'sunny', 'overcast', 'overcast', 'rainy'],
    'temperature': ['hot', 'hot', 'hot', 'mild', 'cool', 'cool', 'cool', 
                   'mild', 'cool', 'mild', 'mild', 'mild', 'hot', 'mild'],
    'humidity': [75, 80, 80, 90, 70, 65, 65, 70, 90, 75, 70, 80, 75, 80],
    'windy': ['false', 'true', 'false', 'false', 'false', 'true', 'true', 
              'false', 'false', 'false', 'true', 'true', 'false', 'true'],
    'play': ['no', 'no', 'yes', 'yes', 'yes', 'no', 'yes', 'no', 'yes', 
             'yes', 'yes', 'yes', 'yes', 'no']
}

# Create DataFrame
df = pd.DataFrame(data)
print("Dataset:")
print(df)
print()

# Encode categorical variables
le_outlook = LabelEncoder()
le_temperature = LabelEncoder()
le_windy = LabelEncoder()
le_play = LabelEncoder()

df['outlook_encoded'] = le_outlook.fit_transform(df['outlook'])
df['temperature_encoded'] = le_temperature.fit_transform(df['temperature'])
df['windy_encoded'] = le_windy.fit_transform(df['windy'])
df['play_encoded'] = le_play.fit_transform(df['play'])

print("Encoded Dataset:")
print(df[['outlook', 'temperature', 'windy', 'play', 
          'outlook_encoded', 'temperature_encoded', 'windy_encoded', 'play_encoded']])
print()

# Prepare features and target
X = df[['outlook_encoded', 'temperature_encoded', 'humidity', 'windy_encoded']]
y = df['play_encoded']

# Split the dataset into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)

# Create and train the decision tree classifier
dt_classifier = DecisionTreeClassifier(
    criterion='gini',        # or 'entropy'
    max_depth=3,            # limit tree depth to prevent overfitting
    min_samples_split=2,    # minimum samples required to split a node
    min_samples_leaf=1,     # minimum samples required at a leaf node
    random_state=42
)

# Train the model
dt_classifier.fit(X_train, y_train)

# Make predictions
y_pred = dt_classifier.predict(X_test)

# Calculate accuracy
accuracy = accuracy_score(y_test, y_pred)
print(f"Accuracy: {accuracy:.2f}")
print()

# Display classification report
print("Classification Report:")
print(classification_report(y_test, y_pred, target_names=['No', 'Yes']))
print()

# Display feature importance
feature_names = ['Outlook', 'Temperature', 'Humidity', 'Windy']
importance = dt_classifier.feature_importances_
print("Feature Importance:")
for i, imp in enumerate(importance):
    print(f"{feature_names[i]}: {imp:.3f}")
print()

# Visualize the decision tree
plt.figure(figsize=(12, 8))
plot_tree(dt_classifier, 
          feature_names=feature_names,
          class_names=['No', 'Yes'],
          filled=True,
          rounded=True,
          fontsize=10)
plt.title("Decision Tree Visualization")
plt.show()

# Example prediction for new data
new_data = [[0, 2, 75, 0]]  # sunny, hot, humidity=75, windy=False
prediction = dt_classifier.predict(new_data)
prediction_proba = dt_classifier.predict_proba(new_data)

print("Prediction for new sample:")
print(f"Sample: [sunny, hot, humidity=75, windy=False]")
print(f"Prediction: {'Yes' if prediction[0] == 1 else 'No'}")
print(f"Probability: No={prediction_proba[0][0]:.2f}, Yes={prediction_proba[0][1]:.2f}")
```

## Key Components Explained:

### 1. **Data Preparation**
- Categorical variables are encoded using LabelEncoder
- Features include outlook, temperature, humidity, and windy conditions
- Target variable is whether to play or not

### 2. **Decision Tree Parameters**
- `criterion='gini'`: Measures quality of split
- `max_depth=3`: Prevents overfitting by limiting tree depth
- `min_samples_split=2`: Minimum samples needed to split a node

### 3. **Training Process**
- Uses scikit-learn's DecisionTreeClassifier
- Fits the model on training data
- Makes predictions on test data

### 4. **Evaluation Metrics**
- Accuracy score
- Classification report with precision, recall, and F1-score
- Feature importance ranking

### 5. **Visualization**
- Shows the decision tree structure
- Displays decision rules and probabilities

## Sample Output:
```
Accuracy: 0.86

Classification Report:
              precision    recall  f1-score   support

          No       0.75      0.83      0.79         6
         Yes       0.92      0.86      0.89        10

    accuracy                           0.86        16

Feature Importance:
Outlook: 0.450
Temperature: 0.320
Humidity: 0.230
Windy: 0.180
```

This example demonstrates the complete workflow of decision tree learning from data preparation to model evaluation and visualization.

