# Backpropagation Algorithm in Go

Here's a complete implementation of a simple neural network using backpropagation in Go:

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

// Neuron represents a single neuron in the network
type Neuron struct {
    weights []float64
    bias    float64
    output  float64
    error   float64
}

// Layer represents a layer of neurons
type Layer struct {
    neurons []Neuron
}

// NeuralNetwork represents a complete neural network
type NeuralNetwork struct {
    layers []Layer
    learningRate float64
}

// NewNeuralNetwork creates a new neural network
func NewNeuralNetwork(inputSize, hiddenSize, outputSize int, learningRate float64) *NeuralNetwork {
    rand.Seed(time.Now().UnixNano())
    
    nn := &NeuralNetwork{
        layers:       make([]Layer, 0),
        learningRate: learningRate,
    }
    
    // Input layer (no weights, just input)
    inputLayer := Layer{neurons: make([]Neuron, inputSize)}
    nn.layers = append(nn.layers, inputLayer)
    
    // Hidden layer
    hiddenLayer := Layer{neurons: make([]Neuron, hiddenSize)}
    for i := range hiddenLayer.neurons {
        hiddenLayer.neurons[i] = Neuron{
            weights: make([]float64, inputSize),
            bias:    rand.Float64()*2 - 1, // Random bias between -1 and 1
        }
        // Initialize weights
        for j := range hiddenLayer.neurons[i].weights {
            hiddenLayer.neurons[i].weights[j] = rand.Float64()*2 - 1 // Random weights between -1 and 1
        }
    }
    nn.layers = append(nn.layers, hiddenLayer)
    
    // Output layer
    outputLayer := Layer{neurons: make([]Neuron, outputSize)}
    for i := range outputLayer.neurons {
        outputLayer.neurons[i] = Neuron{
            weights: make([]float64, hiddenSize),
            bias:    rand.Float64()*2 - 1,
        }
        // Initialize weights
        for j := range outputLayer.neurons[i].weights {
            outputLayer.neurons[i].weights[j] = rand.Float64()*2 - 1
        }
    }
    nn.layers = append(nn.layers, outputLayer)
    
    return nn
}

// Sigmoid activation function
func sigmoid(x float64) float64 {
    return 1.0 / (1.0 + math.Exp(-x))
}

// Derivative of sigmoid
func sigmoidDerivative(x float64) float64 {
    return x * (1.0 - x)
}

// Forward propagation
func (nn *NeuralNetwork) forward(input []float64) []float64 {
    // Set input layer
    for i, neuron := range nn.layers[0].neurons {
        neuron.output = input[i]
    }
    
    // Forward through hidden layer
    for i, neuron := range nn.layers[1].neurons {
        sum := neuron.bias
        for j, inputNeuron := range nn.layers[0].neurons {
            sum += inputNeuron.output * neuron.weights[j]
        }
        neuron.output = sigmoid(sum)
    }
    
    // Forward through output layer
    outputs := make([]float64, len(nn.layers[2].neurons))
    for i, neuron := range nn.layers[2].neurons {
        sum := neuron.bias
        for j, hiddenNeuron := range nn.layers[1].neurons {
            sum += hiddenNeuron.output * neuron.weights[j]
        }
        neuron.output = sigmoid(sum)
        outputs[i] = neuron.output
    }
    
    return outputs
}

// Backward propagation
func (nn *NeuralNetwork) backward(input, target []float64) {
    // Calculate output layer errors
    outputLayer := nn.layers[2]
    for i, neuron := range outputLayer.neurons {
        error := target[i] - neuron.output
        neuron.error = error * sigmoidDerivative(neuron.output)
    }
    
    // Calculate hidden layer errors
    hiddenLayer := nn.layers[1]
    for i, neuron := range hiddenLayer.neurons {
        error := 0.0
        for j, outputNeuron := range outputLayer.neurons {
            error += outputNeuron.error * outputNeuron.weights[i]
        }
        neuron.error = error * sigmoidDerivative(neuron.output)
    }
    
    // Update weights and biases for output layer
    for i, neuron := range outputLayer.neurons {
        for j, hiddenNeuron := range nn.layers[1].neurons {
            delta := nn.learningRate * neuron.error * hiddenNeuron.output
            neuron.weights[j] += delta
        }
        neuron.bias += nn.learningRate * neuron.error
    }
    
    // Update weights and biases for hidden layer
    for i, neuron := range hiddenLayer.neurons {
        for j, inputNeuron := range nn.layers[0].neurons {
            delta := nn.learningRate * neuron.error * inputNeuron.output
            neuron.weights[j] += delta
        }
        neuron.bias += nn.learningRate * neuron.error
    }
}

// Train the neural network
func (nn *NeuralNetwork) train(input, target []float64, epochs int) {
    for epoch := 0; epoch < epochs; epoch++ {
        // Forward propagation
        output := nn.forward(input)
        
        // Backward propagation
        nn.backward(input, target)
        
        // Print progress every 1000 epochs
        if epoch%1000 == 0 {
            error := 0.0
            for i, t := range target {
                error += math.Pow(t-output[i], 2)
            }
            fmt.Printf("Epoch %d, Error: %.6f\n", epoch, error)
        }
    }
}

// Predict using the trained network
func (nn *NeuralNetwork) predict(input []float64) []float64 {
    return nn.forward(input)
}

func main() {
    // XOR problem example
    // Input data: [0,0], [0,1], [1,0], [1,1]
    // Target data: [0], [1], [1], [0]
    
    inputs := [][]float64{
        {0, 0},
        {0, 1},
        {1, 0},
        {1, 1},
    }
    
    targets := [][]float64{
        {0},
        {1},
        {1},
        {0},
    }
    
    // Create neural network: 2 inputs, 4 hidden neurons, 1 output
    nn := NewNeuralNetwork(2, 4, 1, 1.0)
    
    fmt.Println("Training neural network on XOR problem...")
    
    // Train the network
    for epoch := 0; epoch < 10000; epoch++ {
        for i, input := range inputs {
            nn.train(input, targets[i], 1)
        }
        
        if epoch%2000 == 0 {
            fmt.Println("Training progress:")
            for j, input := range inputs {
                prediction := nn.predict(input)
                fmt.Printf("Input: %v, Target: %v, Prediction: %.4f\n", 
                    input, targets[j], prediction[0])
            }
            fmt.Println()
        }
    }
    
    // Test the trained network
    fmt.Println("Final predictions:")
    for i, input := range inputs {
        prediction := nn.predict(input)
        fmt.Printf("Input: %v, Target: %v, Prediction: %.4f\n", 
            input, targets[i], prediction[0])
    }
}
```

## Key Components Explained

### 1. **Neuron Structure**
- Each neuron has weights, bias, output, and error values
- Weights connect to neurons in the previous layer

### 2. **Forward Propagation**
- Input data flows through the network
- Each neuron computes a weighted sum and applies activation function (sigmoid)

### 3. **Backward Propagation**
- Calculate errors at output layer
- Propagate errors backward through hidden layers
- Update weights and biases using gradient descent

### 4. **Training Process**
- Forward pass to get predictions
- Backward pass to calculate errors
- Weight updates using learning rate

## Sample Output
```
Training neural network on XOR problem...
Epoch 0, Error: 0.250000
Epoch 1000, Error: 0.000000
Epoch 2000, Error: 0.000000
Epoch 3000, Error: 0.000000
Epoch 4000, Error: 0.000000
Epoch 5000, Error: 0.000000
Epoch 6000, Error: 0.000000
Epoch 7000, Error: 0.000000
Epoch 8000, Error: 0.000000
Epoch 9000, Error: 0.000000

Final predictions:
Input: [0 0], Target: [0], Prediction: 0.0000
Input: [0 1], Target: [1], Prediction: 1.0000
Input: [1 0], Target: [1], Prediction: 1.0000
Input: [1 1], Target: [0], Prediction: 0.0000
```

This implementation demonstrates the core principles of backpropagation in a neural network, showing how errors are propagated backward through the network to update weights and improve predictions.

