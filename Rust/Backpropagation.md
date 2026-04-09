# Backpropagation Algorithm in Rust

Here's a complete implementation of a neural network with backpropagation using Rust:

```rust
use std::f64::consts::E;
use std::ops::{Add, Mul, Sub};

// Sigmoid activation function
fn sigmoid(x: f64) -> f64 {
    1.0 / (1.0 + E.powf(-x))
}

// Derivative of sigmoid function
fn sigmoid_derivative(x: f64) -> f64 {
    x * (1.0 - x)
}

// Matrix structure for neural network weights
#[derive(Debug, Clone)]
struct Matrix {
    data: Vec<Vec<f64>>,
    rows: usize,
    cols: usize,
}

impl Matrix {
    fn new(rows: usize, cols: usize) -> Self {
        Matrix {
            data: vec![vec![0.0; cols]; rows],
            rows,
            cols,
        }
    }

    fn new_random(rows: usize, cols: usize, min: f64, max: f64) -> Self {
        let mut matrix = Matrix::new(rows, cols);
        for i in 0..rows {
            for j in 0..cols {
                matrix.data[i][j] = min + (max - min) * rand::random::<f64>();
            }
        }
        matrix
    }

    fn get(&self, row: usize, col: usize) -> f64 {
        self.data[row][col]
    }

    fn set(&mut self, row: usize, col: usize, value: f64) {
        self.data[row][col] = value;
    }

    fn multiply(&self, other: &Matrix) -> Matrix {
        let mut result = Matrix::new(self.rows, other.cols);
        for i in 0..self.rows {
            for j in 0..other.cols {
                let mut sum = 0.0;
                for k in 0..self.cols {
                    sum += self.data[i][k] * other.data[k][j];
                }
                result.data[i][j] = sum;
            }
        }
        result
    }

    fn add(&self, other: &Matrix) -> Matrix {
        let mut result = Matrix::new(self.rows, self.cols);
        for i in 0..self.rows {
            for j in 0..self.cols {
                result.data[i][j] = self.data[i][j] + other.data[i][j];
            }
        }
        result
    }

    fn subtract(&self, other: &Matrix) -> Matrix {
        let mut result = Matrix::new(self.rows, self.cols);
        for i in 0..self.rows {
            for j in 0..self.cols {
                result.data[i][j] = self.data[i][j] - other.data[i][j];
            }
        }
        result
    }

    fn transpose(&self) -> Matrix {
        let mut result = Matrix::new(self.cols, self.rows);
        for i in 0..self.rows {
            for j in 0..self.cols {
                result.data[j][i] = self.data[i][j];
            }
        }
        result
    }

    fn apply_function(&self, func: fn(f64) -> f64) -> Matrix {
        let mut result = Matrix::new(self.rows, self.cols);
        for i in 0..self.rows {
            for j in 0..self.cols {
                result.data[i][j] = func(self.data[i][j]);
            }
        }
        result
    }
}

// Neural Network implementation
struct NeuralNetwork {
    weights_input_hidden: Matrix,
    weights_hidden_output: Matrix,
    bias_hidden: Matrix,
    bias_output: Matrix,
    learning_rate: f64,
}

impl NeuralNetwork {
    fn new(input_size: usize, hidden_size: usize, output_size: usize, learning_rate: f64) -> Self {
        // Initialize weights with random values
        let weights_input_hidden = Matrix::new_random(hidden_size, input_size, -1.0, 1.0);
        let weights_hidden_output = Matrix::new_random(output_size, hidden_size, -1.0, 1.0);
        
        // Initialize biases with zeros
        let bias_hidden = Matrix::new(hidden_size, 1);
        let bias_output = Matrix::new(output_size, 1);
        
        NeuralNetwork {
            weights_input_hidden,
            weights_hidden_output,
            bias_hidden,
            bias_output,
            learning_rate,
        }
    }

    fn forward(&self, input: &Matrix) -> (Matrix, Matrix, Matrix, Matrix) {
        // Forward propagation
        let hidden_input = self.weights_input_hidden.multiply(input);
        let hidden_input_with_bias = hidden_input.add(&self.bias_hidden);
        let hidden_output = hidden_input_with_bias.apply_function(sigmoid);
        
        let output_input = self.weights_hidden_output.multiply(&hidden_output);
        let output_input_with_bias = output_input.add(&self.bias_output);
        let output = output_input_with_bias.apply_function(sigmoid);
        
        (hidden_input_with_bias, hidden_output, output_input_with_bias, output)
    }

    fn backward(&mut self, input: &Matrix, target: &Matrix, output: &Matrix, hidden_output: &Matrix) {
        // Calculate errors
        let output_error = target.subtract(output);
        let output_delta = output_error.multiply(&output.apply_function(sigmoid_derivative));
        
        let hidden_error = self.weights_hidden_output.transpose().multiply(&output_delta);
        let hidden_delta = hidden_error.multiply(&hidden_output.apply_function(sigmoid_derivative));
        
        // Update weights and biases
        let hidden_output_transpose = hidden_output.transpose();
        let weight_hidden_output_delta = output_delta.multiply(&hidden_output_transpose);
        self.weights_hidden_output = self.weights_hidden_output.add(
            &weight_hidden_output_delta.multiply(&Matrix::new(1, 1).set(0, 0, self.learning_rate))
        );
        
        let input_transpose = input.transpose();
        let weight_input_hidden_delta = hidden_delta.multiply(&input_transpose);
        self.weights_input_hidden = self.weights_input_hidden.add(
            &weight_input_hidden_delta.multiply(&Matrix::new(1, 1).set(0, 0, self.learning_rate))
        );
        
        // Update biases
        self.bias_output = self.bias_output.add(&output_delta.multiply(&Matrix::new(1, 1).set(0, 0, self.learning_rate)));
        self.bias_hidden = self.bias_hidden.add(&hidden_delta.multiply(&Matrix::new(1, 1).set(0, 0, self.learning_rate)));
    }

    fn train(&mut self, inputs: &Vec<Matrix>, targets: &Vec<Matrix>, epochs: usize) {
        for epoch in 0..epochs {
            let mut total_error = 0.0;
            
            for (input, target) in inputs.iter().zip(targets.iter()) {
                // Forward pass
                let (hidden_input, hidden_output, output_input, output) = self.forward(input);
                
                // Backward pass
                self.backward(input, target, &output, &hidden_output);
                
                // Calculate error
                let error = target.subtract(&output);
                let error_squared = error.multiply(&error);
                total_error += error_squared.data.iter().flatten().sum::<f64>();
            }
            
            if epoch % 100 == 0 {
                println!("Epoch {}: Average Error = {:.6}", epoch, total_error / (inputs.len() as f64));
            }
        }
    }

    fn predict(&self, input: &Matrix) -> Matrix {
        let (_, _, _, output) = self.forward(input);
        output
    }
}

fn main() {
    // Example: XOR problem
    let inputs = vec![
        Matrix::new(2, 1).set(0, 0, 0.0).set(1, 0, 0.0),
        Matrix::new(2, 1).set(0, 0, 0.0).set(1, 0, 1.0),
        Matrix::new(2, 1).set(0, 0, 1.0).set(1, 0, 0.0),
        Matrix::new(2, 1).set(0, 0, 1.0).set(1, 0, 1.0),
    ];
    
    let targets = vec![
        Matrix::new(1, 1).set(0, 0, 0.0),
        Matrix::new(1, 1).set(0, 0, 1.0),
        Matrix::new(1, 1).set(0, 0, 1.0),
        Matrix::new(1, 1).set(0, 0, 0.0),
    ];
    
    // Create neural network with 2 inputs, 4 hidden neurons, 1 output
    let mut nn = NeuralNetwork::new(2, 4, 1, 1.0);
    
    println!("Training neural network on XOR problem...");
    nn.train(&inputs, &targets, 1000);
    
    // Test predictions
    println!("\nTesting predictions:");
    for (input, target) in inputs.iter().zip(targets.iter()) {
        let prediction = nn.predict(input);
        println!(
            "Input: [{}, {}] -> Target: {:.1} -> Prediction: {:.4}",
            input.get(0, 0), input.get(1, 0),
            target.get(0, 0),
            prediction.get(0, 0)
        );
    }
}
```

## Key Components Explained

### 1. **Matrix Operations**
- Custom `Matrix` struct with basic operations
- Supports multiplication, addition, subtraction, and transposition
- Includes activation functions (sigmoid and its derivative)

### 2. **Neural Network Architecture**
- Input layer (2 neurons)
- Hidden layer (4 neurons)
- Output layer (1 neuron)
- Uses sigmoid activation function

### 3. **Backpropagation Process**
1. **Forward Pass**: Compute outputs through the network
2. **Error Calculation**: Compare predictions with targets
3. **Backward Pass**: Calculate gradients using chain rule
4. **Weight Updates**: Adjust weights and biases using gradient descent

### 4. **Training Loop**
- Iterates through epochs
- Calculates total error for monitoring
- Updates network parameters using backpropagation

## Usage Notes

To run this code, you'll need to add the `rand` crate to your `Cargo.toml`:

```toml
[dependencies]
rand = "0.8"
```

The example demonstrates solving the XOR problem, which is a classic test case for neural networks. The network learns to approximate the XOR function through backpropagation, adjusting weights and biases to minimize prediction errors.

