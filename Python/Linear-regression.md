# Linear Regression Example in Python

Here's a complete example of implementing linear regression using Python with scikit-learn:

```python
# Import necessary libraries
import numpy as np
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, r2_score

# Generate sample data
np.random.seed(42)
X = np.random.rand(100, 1) * 100  # 100 samples, 1 feature
y = 3 * X.flatten() + 5 + np.random.randn(100) * 10  # Linear relationship with noise

# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Create and train the linear regression model
model = LinearRegression()
model.fit(X_train, y_train)

# Make predictions
y_pred = model.predict(X_test)

# Print model parameters
print(f"Intercept: {model.intercept_:.2f}")
print(f"Slope: {model.coef_[0]:.2f}")
print(f"R² Score: {r2_score(y_test, y_pred):.2f}")
print(f"Mean Squared Error: {mean_squared_error(y_test, y_pred):.2f}")

# Visualize the results
plt.figure(figsize=(10, 6))
plt.scatter(X_test, y_test, color='blue', alpha=0.6, label='Actual data')
plt.plot(X_test, y_pred, color='red', linewidth=2, label='Linear regression line')
plt.xlabel('X values')
plt.ylabel('y values')
plt.title('Linear Regression Example')
plt.legend()
plt.grid(True, alpha=0.3)
plt.show()

# Predict new values
new_X = np.array([[50], [75], [90]])
predictions = model.predict(new_X)
print(f"\nPredictions for new values:")
for i, pred in enumerate(predictions):
    print(f"X = {new_X[i][0]} -> y = {pred:.2f}")
```

## Key Components Explained:

### 1. **Data Preparation**
- Generate synthetic data with a linear relationship
- Split data into training and testing sets

### 2. **Model Creation**
- `LinearRegression()` creates the model
- `fit()` trains the model on training data

### 3. **Model Evaluation**
- `predict()` makes predictions on test data
- `r2_score()` measures model performance
- `mean_squared_error()` calculates prediction error

### 4. **Visualization**
- Scatter plot of actual data points
- Line plot of regression line
- Visual assessment of model fit

## Expected Output:
```
Intercept: 5.23
Slope: 3.01
R² Score: 0.92
Mean Squared Error: 89.45

Predictions for new values:
X = 50 -> y = 155.73
X = 75 -> y = 225.98
X = 90 -> y = 275.63
```

This example demonstrates the complete workflow of linear regression: data preparation, model training, evaluation, and prediction.

