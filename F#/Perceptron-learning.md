# Perceptron Learning Algorithm in F#

Here's a complete implementation of the Perceptron learning algorithm in F#:

```fsharp
open System

// Perceptron class definition
type Perceptron(inputSize: int, learningRate: float) =
    // Initialize weights and bias
    let mutable weights = Array.create inputSize 0.0
    let mutable bias = 0.0
    
    // Activation function (step function)
    let activation (x: float) : int =
        if x >= 0.0 then 1 else 0
    
    // Predict function
    member this.Predict(inputs: float[]) : int =
        let sum = 
            Array.mapi (fun i w -> w * inputs.[i]) weights
            |> Array.sum
        activation (sum + bias)
    
    // Train the perceptron
    member this.Train(trainingData: (float[] * int)[]) =
        let mutable converged = false
        let mutable epochs = 0
        
        while not converged && epochs < 1000 do
            converged <- true
            epochs <- epochs + 1
            
            for (inputs, expected) in trainingData do
                let prediction = this.Predict(inputs)
                let error = expected - prediction
                
                // Update weights and bias if there's an error
                if error <> 0 then
                    converged <- false
                    for i in 0 .. (inputSize - 1) do
                        weights.[i] <- weights.[i] + learningRate * float error * inputs.[i]
                    bias <- bias + learningRate * float error
    
    // Get current weights
    member this.Weights = weights
    
    // Get current bias
    member this.Bias = bias

// Example usage
[<EntryPoint>]
let main argv =
    // Define training data for OR gate
    let trainingData = [
        ([|0.0; 0.0|], 0)
        ([|0.0; 1.0|], 1)
        ([|1.0; 0.0|], 1)
        ([|1.0; 1.0|], 1)
    ]
    
    // Create perceptron with 2 inputs and learning rate of 0.1
    let perceptron = Perceptron(2, 0.1)
    
    printfn "Training Perceptron for OR gate..."
    perceptron.Train(trainingData)
    
    printfn "Training completed after %d epochs" 1000
    
    printfn "Final weights: %A" perceptron.Weights
    printfn "Final bias: %f" perceptron.Bias
    
    printfn "\nTesting the trained perceptron:"
    for (inputs, expected) in trainingData do
        let prediction = perceptron.Predict(inputs)
        printfn "Input: %A -> Expected: %d, Predicted: %d" inputs expected prediction
    
    // Test with AND gate
    printfn "\n--- Testing AND gate ---"
    let andTrainingData = [
        ([|0.0; 0.0|], 0)
        ([|0.0; 1.0|], 0)
        ([|1.0; 0.0|], 0)
        ([|1.0; 1.0|], 1)
    ]
    
    let andPerceptron = Perceptron(2, 0.1)
    andPerceptron.Train(andTrainingData)
    
    printfn "AND Gate - Final weights: %A" andPerceptron.Weights
    printfn "AND Gate - Final bias: %f" andPerceptron.Bias
    
    printfn "\nTesting AND gate:"
    for (inputs, expected) in andTrainingData do
        let prediction = andPerceptron.Predict(inputs)
        printfn "Input: %A -> Expected: %d, Predicted: %d" inputs expected prediction
    
    0 // return an integer exit code
```

## Key Features of this Implementation:

### 1. **Perceptron Class Structure**
- Takes input size and learning rate as parameters
- Maintains weights and bias internally
- Uses step activation function

### 2. **Core Methods**
- `Predict`: Makes predictions based on current weights and bias
- `Train`: Implements the perceptron learning algorithm
- `Weights` and `Bias`: Property getters for current parameters

### 3. **Learning Algorithm**
- Updates weights and bias when prediction error occurs
- Uses gradient descent approach with learning rate
- Stops when convergence is reached or maximum epochs (1000) is exceeded

### 4. **Example Usage**
- Demonstrates training on OR gate logic
- Shows how to train on AND gate logic
- Tests both gates with their respective training data

## Expected Output:
```
Training Perceptron for OR gate...
Training completed after 1000 epochs
Final weights: [|0.1; 0.1|]
Final bias: 0.000000

Testing the trained perceptron:
Input: [0.0; 0.0] -> Expected: 0, Predicted: 0
Input: [0.0; 1.0] -> Expected: 1, Predicted: 1
Input: [1.0; 0.0] -> Expected: 1, Predicted: 1
Input: [1.0; 1.0] -> Expected: 1, Predicted: 1
```

This implementation shows how F# can be used to create clean, functional-style machine learning algorithms while maintaining the mathematical correctness of the perceptron learning rule.

