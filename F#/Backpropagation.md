# Backpropagation Algorithm in F#

Here's a complete implementation of a neural network with backpropagation in F#:

```fsharp
open System

// Define types for our neural network
type Matrix = float[,] 
type Vector = float[]

// Sigmoid activation function and its derivative
let sigmoid x = 1.0 / (1.0 + exp (-x))
let sigmoidDerivative x = x * (1.0 - x)

// Initialize weights with random values
let randomWeight () = (Random().NextDouble() * 2.0) - 1.0

// Create a matrix with random weights
let createWeightMatrix rows cols = 
    let matrix = Array2D.create rows cols 0.0
    for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
            matrix.[i, j] <- randomWeight()
    matrix

// Forward propagation
let forwardPropagation (input: Vector) (weights1: Matrix) (weights2: Matrix) =
    // Layer 1 (input to hidden)
    let hiddenLayer = Array.zeroCreate weights1.GetLength(0)
    for i in 0 .. hiddenLayer.Length - 1 do
        hiddenLayer.[i] <- 
            input
            |> Array.mapi (fun j w -> w * weights1.[i, j])
            |> Array.sum
    
    // Apply activation function
    let activatedHidden = hiddenLayer |> Array.map sigmoid
    
    // Layer 2 (hidden to output)
    let outputLayer = Array.zeroCreate weights2.GetLength(0)
    for i in 0 .. outputLayer.Length - 1 do
        outputLayer.[i] <- 
            activatedHidden
            |> Array.mapi (fun j w -> w * weights2.[i, j])
            |> Array.sum
    
    // Apply activation function
    let activatedOutput = outputLayer |> Array.map sigmoid
    
    (activatedHidden, activatedOutput)

// Backpropagation algorithm
let backpropagate (input: Vector) (target: Vector) (weights1: Matrix) (weights2: Matrix) (learningRate: float) =
    // Forward pass
    let (hiddenLayer, outputLayer) = forwardPropagation input weights1 weights2
    
    // Calculate output layer errors
    let outputErrors = 
        Array.mapi (fun i targetValue -> 
            let outputValue = outputLayer.[i]
            (targetValue - outputValue) * sigmoidDerivative outputValue
        ) target
    
    // Calculate hidden layer errors
    let hiddenErrors = 
        Array.zeroCreate weights1.GetLength(0)
        |> Array.mapi (fun i _ ->
            let error = 
                outputErrors
                |> Array.mapi (fun j outputError -> 
                    outputError * weights2.[j, i]
                )
                |> Array.sum
            error * sigmoidDerivative hiddenLayer.[i]
        )
    
    // Update weights for output layer
    let updatedWeights2 = Array2D.copy weights2
    for i in 0 .. outputErrors.Length - 1 do
        for j in 0 .. weights2.GetLength(1) - 1 do
            updatedWeights2.[i, j] <- 
                weights2.[i, j] + 
                learningRate * outputErrors.[i] * hiddenLayer.[j]
    
    // Update weights for hidden layer
    let updatedWeights1 = Array2D.copy weights1
    for i in 0 .. hiddenErrors.Length - 1 do
        for j in 0 .. weights1.GetLength(1) - 1 do
            updatedWeights1.[i, j] <- 
                weights1.[i, j] + 
                learningRate * hiddenErrors.[i] * input.[j]
    
    (updatedWeights1, updatedWeights2)

// Neural Network class
type NeuralNetwork(hiddenSize: int, learningRate: float) =
    let inputSize = 2
    let outputSize = 1
    
    // Initialize weights
    let weights1 = createWeightMatrix hiddenSize inputSize
    let weights2 = createWeightMatrix outputSize hiddenSize
    
    member this.Predict(input: Vector) =
        let (_, output) = forwardPropagation input weights1 weights2
        output.[0]
    
    member this.Train(inputs: Vector[]) targets: Vector[] =
        let mutable w1 = weights1
        let mutable w2 = weights2
        
        // Train for multiple iterations
        for _ in 1 .. 1000 do
            for i in 0 .. inputs.Length - 1 do
                let (newW1, newW2) = 
                    backpropagate inputs.[i] targets.[i] w1 w2 learningRate
                w1 <- newW1
                w2 <- newW2
        
        // Update weights in the network
        weights1 <- w1
        weights2 <- w2
        
        // Return predictions after training
        inputs
        |> Array.map (fun input -> 
            let (_, output) = forwardPropagation input weights1 weights2
            output)
    
    member this.Weights1 = weights1
    member this.Weights2 = weights2

// Example usage
[<EntryPoint>]
let main argv =
    // XOR problem data
    let inputs = 
        [| [| 0.0; 0.0 |]
           [| 0.0; 1.0 |]
           [| 1.0; 0.0 |]
           [| 1.0; 1.0 |] |]
    
    let targets = 
        [| [| 0.0 |]
           [| 1.0 |]
           [| 1.0 |]
           [| 0.0 |] |]
    
    // Create neural network with 4 hidden neurons
    let nn = NeuralNetwork(4, 1.0)
    
    printfn "Training neural network..."
    
    // Train the network
    let predictions = nn.Train(inputs, targets)
    
    printfn "Training complete!"
    printfn "Results:"
    
    for i in 0 .. inputs.Length - 1 do
        printfn "Input: %A -> Target: %A -> Prediction: %A" 
            inputs.[i] targets.[i] predictions.[i]
    
    0 // return code
```

## Key Components Explained:

### 1. **Forward Propagation**
- Takes input vector and passes it through the network
- Applies sigmoid activation function at each layer
- Returns hidden and output layer activations

### 2. **Backpropagation**
- Calculates errors at output layer
- Propagates errors backward to hidden layer
- Updates weights using gradient descent
- Uses learning rate to control update magnitude

### 3. **Training Process**
- Iterates through training data multiple times
- Updates weights after each training example
- Uses XOR problem as a demonstration

### 4. **Key Features**
- Functional programming approach in F#
- Matrix operations for efficient computation
- Clear separation of forward and backward passes
- Learning rate control for stable training

This implementation demonstrates the core backpropagation algorithm in a clean, functional F# style while solving a classic machine learning problem.

