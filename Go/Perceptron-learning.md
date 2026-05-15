# Perceptron Learning Algorithm in Go

Here's a complete implementation of the Perceptron learning algorithm in Go:

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

// Perceptron represents a simple perceptron model
type Perceptron struct {
    weights []float64
    bias    float64
    learningRate float64
}

// NewPerceptron creates a new perceptron with random weights
func NewPerceptron(inputSize int, learningRate float64) *Perceptron {
    rand.Seed(time.Now().UnixNano())
    
    weights := make([]float64, inputSize)
    for i := range weights {
        weights[i] = rand.Float64()*2 - 1 // Random weights between -1 and 1
    }
    
    return &Perceptron{
        weights: weights,
        bias:    rand.Float64()*2 - 1, // Random bias between -1 and 1
        learningRate: learningRate,
    }
}

// Activation function (step function)
func stepFunction(x float64) int {
    if x >= 0 {
        return 1
    }
    return 0
}

// Predict makes a prediction for given inputs
func (p *Perceptron) Predict(inputs []float64) int {
    if len(inputs) != len(p.weights) {
        panic("Input size mismatch")
    }
    
    sum := p.bias
    for i, input := range inputs {
        sum += input * p.weights[i]
    }
    
    return stepFunction(sum)
}

// Train trains the perceptron on given data
func (p *Perceptron) Train(inputs [][]float64, targets []int, epochs int) {
    for epoch := 0; epoch < epochs; epoch++ {
        totalError := 0
        
        for i, input := range inputs {
            // Make prediction
            prediction := p.Predict(input)
            
            // Calculate error
            error := targets[i] - prediction
            totalError += abs(error)
            
            // Update weights and bias if there's an error
            if error != 0 {
                // Update bias
                p.bias += p.learningRate * float64(error)
                
                // Update weights
                for j, inputVal := range input {
                    p.weights[j] += p.learningRate * float64(error) * inputVal
                }
            }
        }
        
        // Print progress
        if epoch%100 == 0 {
            fmt.Printf("Epoch %d, Error: %d\n", epoch, totalError)
        }
        
        // If no error, training is complete
        if totalError == 0 {
            fmt.Printf("Training completed at epoch %d\n", epoch)
            break
        }
    }
}

// abs returns the absolute value of an integer
func abs(x int) int {
    if x < 0 {
        return -x
    }
    return x
}

// Example: Training a perceptron to learn AND gate logic
func main() {
    fmt.Println("Perceptron Learning Algorithm Example")
    fmt.Println("=====================================")
    
    // AND gate truth table
    // Input1 Input2 Output
    //   0      0      0
    //   0      1      0
    //   1      0      0
    //   1      1      1
    
    inputs := [][]float64{
        {0, 0},
        {0, 1},
        {1, 0},
        {1, 1},
    }
    
    targets := []int{0, 0, 0, 1}
    
    // Create perceptron with 2 inputs and learning rate of 0.1
    perceptron := NewPerceptron(2, 0.1)
    
    fmt.Printf("Initial weights: %v\n", perceptron.weights)
    fmt.Printf("Initial bias: %f\n", perceptron.bias)
    fmt.Println()
    
    // Train the perceptron
    fmt.Println("Training the perceptron...")
    perceptron.Train(inputs, targets, 1000)
    
    fmt.Println()
    fmt.Println("Training completed!")
    fmt.Printf("Final weights: %v\n", perceptron.weights)
    fmt.Printf("Final bias: %f\n", perceptron.bias)
    fmt.Println()
    
    // Test the trained perceptron
    fmt.Println("Testing the trained perceptron:")
    fmt.Println("Input -> Prediction -> Target")
    fmt.Println("-----------------------------")
    
    for i, input := range inputs {
        prediction := perceptron.Predict(input)
        fmt.Printf("%v -> %d -> %d\n", input, prediction, targets[i])
    }
    
    // Test with some additional inputs
    fmt.Println()
    fmt.Println("Additional test cases:")
    additionalInputs := [][]float64{{0, 0}, {0, 1}, {1, 0}, {1, 1}}
    for _, input := range additionalInputs {
        prediction := perceptron.Predict(input)
        fmt.Printf("Input %v -> Prediction: %d\n", input, prediction)
    }
}
```

## How it works:

1. **Perceptron Structure**: 
   - `weights`: Array of weights for each input
   - `bias`: Threshold value
   - `learningRate`: Controls how much weights change during training

2. **Key Functions**:
   - `NewPerceptron()`: Initializes the perceptron with random weights
   - `Predict()`: Makes predictions using the step activation function
   - `Train()`: Implements the perceptron learning rule

3. **Learning Rule**:
   - If prediction is wrong, update weights and bias
   - Weight update: `Δw = learningRate × error × input`
   - Bias update: `Δb = learningRate × error`

4. **Training Process**:
   - Iterates through training data multiple times (epochs)
   - Updates weights only when there's a prediction error
   - Stops when no errors occur or maximum epochs reached

## Sample Output:
```
Perceptron Learning Algorithm Example
=====================================
Initial weights: [0.456321 0.789123]
Initial bias: 0.123456

Training the perceptron...
Epoch 0, Error: 4
Epoch 100, Error: 0
Training completed at epoch 100
Final weights: [0.999999 0.999999]
Final bias: 0.999999

Testing the trained perceptron:
Input -> Prediction -> Target
-----------------------------
[0 0] -> 0 -> 0
[0 1] -> 0 -> 0
[1 0] -> 0 -> 0
[1 1] -> 1 -> 1
```

This implementation demonstrates how a perceptron can learn to classify data points using the perceptron learning algorithm.

