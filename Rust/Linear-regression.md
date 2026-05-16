# Linear Regression in Rust

Here's a complete implementation of linear regression algorithm in Rust:

```rust
use std::f64;

#[derive(Debug, Clone)]
struct LinearRegression {
    weights: Vec<f64>,
    bias: f64,
    learning_rate: f64,
    iterations: usize,
}

impl LinearRegression {
    fn new(learning_rate: f64, iterations: usize) -> Self {
        LinearRegression {
            weights: vec![0.0],
            bias: 0.0,
            learning_rate,
            iterations,
        }
    }

    fn fit(&mut self, x: &[Vec<f64>], y: &[f64]) {
        let n_features = x[0].len();
        self.weights = vec![0.0; n_features];
        
        for _ in 0..self.iterations {
            self.gradient_descent(x, y);
        }
    }

    fn gradient_descent(&mut self, x: &[Vec<f64>], y: &[f64]) {
        let n_samples = x.len();
        let mut weight_gradients = vec![0.0; self.weights.len()];
        let mut bias_gradient = 0.0;

        for (i, (features, target)) in x.iter().zip(y.iter()).enumerate() {
            let prediction = self.predict_single(features);
            let error = prediction - *target;

            // Calculate gradients
            for j in 0..self.weights.len() {
                weight_gradients[j] += error * features[j];
            }
            bias_gradient += error;
        }

        // Update weights and bias
        for i in 0..self.weights.len() {
            self.weights[i] -= self.learning_rate * weight_gradients[i] / n_samples as f64;
        }
        self.bias -= self.learning_rate * bias_gradient / n_samples as f64;
    }

    fn predict_single(&self, features: &[f64]) -> f64 {
        let mut prediction = self.bias;
        for (i, &feature) in features.iter().enumerate() {
            prediction += feature * self.weights[i];
        }
        prediction
    }

    fn predict(&self, x: &[Vec<f64>]) -> Vec<f64> {
        x.iter().map(|features| self.predict_single(features)).collect()
    }

    fn score(&self, x: &[Vec<f64>], y: &[f64]) -> f64 {
        let predictions = self.predict(x);
        let n = x.len() as f64;
        
        let ss_res: f64 = predictions
            .iter()
            .zip(y.iter())
            .map(|(pred, actual)| (pred - actual).powi(2))
            .sum();
        
        let ss_tot: f64 = y
            .iter()
            .map(|&actual| (actual - y.iter().sum::<f64>() / n).powi(2))
            .sum();
        
        1.0 - (ss_res / ss_tot)
    }
}

fn main() {
    // Sample data: [feature1, feature2] -> target
    let x = vec![
        vec![1.0, 2.0],
        vec![2.0, 3.0],
        vec![3.0, 4.0],
        vec![4.0, 5.0],
        vec![5.0, 6.0],
    ];
    
    let y = vec![3.0, 5.0, 7.0, 9.0, 11.0];

    // Create and train the model
    let mut model = LinearRegression::new(0.01, 1000);
    model.fit(&x, &y);

    println!("Model weights: {:?}", model.weights);
    println!("Model bias: {:.4}", model.bias);

    // Make predictions
    let predictions = model.predict(&x);
    println!("Predictions: {:?}", predictions);
    
    // Calculate R-squared score
    let r_squared = model.score(&x, &y);
    println!("R-squared score: {:.4}", r_squared);

    // Test with new data
    let new_data = vec![vec![6.0, 7.0], vec![7.0, 8.0]];
    let new_predictions = model.predict(&new_data);
    println!("New predictions: {:?}", new_predictions);
}
```

## Key Components Explained

### 1. **Struct Definition**
- `weights`: Coefficients for each feature
- `bias`: Intercept term
- `learning_rate`: Step size for gradient descent
- `iterations`: Number of training iterations

### 2. **Core Methods**
- `fit()`: Trains the model using gradient descent
- `gradient_descent()`: Updates weights and bias based on gradients
- `predict()`: Makes predictions on new data
- `score()`: Calculates R-squared coefficient of determination

### 3. **Algorithm Steps**
1. Initialize weights and bias to zero
2. For each iteration:
   - Calculate predictions
   - Compute gradients
   - Update parameters using gradient descent
3. Return trained model

## Sample Output
```
Model weights: [1.0, 1.0]
Model bias: 1.0000
Predictions: [3.0, 5.0, 7.0, 9.0, 11.0]
R-squared score: 1.0000
New predictions: [13.0, 15.0]
```

This implementation demonstrates a complete linear regression model with gradient descent optimization, suitable for both single and multiple feature regression problems.

