# Perceptron Learning Algorithm in Rust

Here's a complete implementation of the Perceptron learning algorithm in Rust:

```rust
#[derive(Debug, Clone)]
pub struct Perceptron {
    weights: Vec<f64>,
    bias: f64,
    learning_rate: f64,
}

impl Perceptron {
    /// Create a new perceptron with random weights
    pub fn new(input_size: usize, learning_rate: f64) -> Self {
        let mut weights = Vec::with_capacity(input_size);
        // Initialize weights with small random values
        for _ in 0..input_size {
            weights.push((rand::random::<f64>() - 0.5) * 0.1);
        }
        
        Perceptron {
            weights,
            bias: (rand::random::<f64>() - 0.5) * 0.1,
            learning_rate,
        }
    }

    /// Activation function (step function)
    fn activation(&self, input: f64) -> i8 {
        if input >= 0.0 {
            1
        } else {
            -1
        }
    }

    /// Predict the output for given inputs
    pub fn predict(&self, inputs: &[f64]) -> i8 {
        let mut sum = self.bias;
        for (i, &weight) in self.weights.iter().enumerate() {
            sum += weight * inputs[i];
        }
        self.activation(sum)
    }

    /// Train the perceptron on a single example
    pub fn train_single(&mut self, inputs: &[f64], target: i8) {
        let prediction = self.predict(inputs);
        let error = target - prediction;
        
        // Update bias
        self.bias += self.learning_rate * error as f64;
        
        // Update weights
        for (i, &weight) in self.weights.iter().enumerate() {
            self.weights[i] = weight + self.learning_rate * error as f64 * inputs[i];
        }
    }

    /// Train the perceptron on multiple examples for multiple epochs
    pub fn train(&mut self, training_data: &[(Vec<f64>, i8)], epochs: usize) {
        for _ in 0..epochs {
            for (inputs, target) in training_data {
                self.train_single(inputs, *target);
            }
        }
    }

    /// Get current weights and bias
    pub fn get_weights(&self) -> &[f64] {
        &self.weights
    }

    pub fn get_bias(&self) -> f64 {
        self.bias
    }
}

/// Example usage
fn main() {
    // Create training data for AND gate
    let training_data = vec![
        (vec![0.0, 0.0], -1), // 0 AND 0 = 0 -> -1
        (vec![0.0, 1.0], -1), // 0 AND 1 = 0 -> -1
        (vec![1.0, 0.0], -1), // 1 AND 0 = 0 -> -1
        (vec![1.0, 1.0], 1),  // 1 AND 1 = 1 -> 1
    ];

    // Create perceptron with 2 inputs and learning rate 0.1
    let mut perceptron = Perceptron::new(2, 0.1);
    
    println!("Initial weights: {:?}", perceptron.get_weights());
    println!("Initial bias: {:.4}", perceptron.get_bias());
    
    // Train the perceptron
    println!("\nTraining the perceptron...");
    perceptron.train(&training_data, 100);
    
    println!("Final weights: {:?}", perceptron.get_weights());
    println!("Final bias: {:.4}", perceptron.get_bias());
    
    // Test the trained perceptron
    println!("\nTesting the trained perceptron:");
    for (inputs, target) in &training_data {
        let prediction = perceptron.predict(inputs);
        println!("Input: {:?} -> Predicted: {}, Expected: {}", 
                 inputs, prediction, target);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_perceptron_training() {
        let training_data = vec![
            (vec![0.0, 0.0], -1),
            (vec![0.0, 1.0], -1),
            (vec![1.0, 0.0], -1),
            (vec![1.0, 1.0], 1),
        ];

        let mut perceptron = Perceptron::new(2, 0.1);
        perceptron.train(&training_data, 100);
        
        // Test that it learned the AND function
        assert_eq!(perceptron.predict(&[0.0, 0.0]), -1);
        assert_eq!(perceptron.predict(&[0.0, 1.0]), -1);
        assert_eq!(perceptron.predict(&[1.0, 0.0]), -1);
        assert_eq!(perceptron.predict(&[1.0, 1.0]), 1);
    }
}
```

## Key Features of this Implementation:

1. **Perceptron Structure**: 
   - Weights vector for each input
   - Bias term
   - Learning rate parameter

2. **Core Methods**:
   - `new()`: Initialize with random weights
   - `predict()`: Make predictions using step activation function
   - `train_single()`: Update weights for one training example
   - `train()`: Train for multiple epochs

3. **Activation Function**: 
   - Step function that returns -1 or 1

4. **Example Usage**:
   - Trains on AND gate logic
   - Demonstrates the learning process

## To Run This Code:

Add this to your `Cargo.toml`:

```toml
[dependencies]
rand = "0.8"
```

The perceptron will learn to classify the AND logic function, demonstrating how the weights and bias are adjusted during training to minimize prediction errors.

