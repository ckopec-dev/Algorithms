```fsharp
// Gradient Descent Implementation in F#

open System

// Define a simple linear regression model: y = ax + b
type LinearModel = { a: float; b: float }

// Cost function (Mean Squared Error)
let costFunction (model: LinearModel) (x: float[]) (y: float[]) =
    let n = float x.Length
    let predictions = Array.map (fun xi -> model.a * xi + model.b) x
    let errors = Array.zip predictions y |> Array.map (fun (pred, actual) -> (pred - actual) ** 2.0)
    Array.sum errors / (2.0 * n)

// Gradient computation for linear regression
let computeGradients (model: LinearModel) (x: float[]) (y: float[]) =
    let n = float x.Length
    let predictions = Array.map (fun xi -> model.a * xi + model.b) x
    let errors = Array.zip predictions y |> Array.map (fun (pred, actual) -> pred - actual)
    
    let da = (1.0 / n) * (Array.sumBy (fun e -> e * x.[0]) errors) // Simplified for single point
    let db = (1.0 / n) * (Array.sum errors)
    
    // For multiple points, we need to compute proper gradients:
    let da = (1.0 / n) * (Array.sumBy2 (fun xi ei -> xi * ei) x errors)
    let db = (1.0 / n) * (Array.sum errors)
    
    { a = da; b = db }

// Gradient descent implementation
let gradientDescent 
    (x: float[]) 
    (y: float[]) 
    (initialModel: LinearModel) 
    (learningRate: float) 
    (iterations: int) =
    
    let mutable model = initialModel
    
    for i in 1 .. iterations do
        // Compute gradients
        let gradients = computeGradients model x y
        
        // Update parameters
        model <- { 
            a = model.a - learningRate * gradients.a
            b = model.b - learningRate * gradients.b
        }
        
        // Print progress every 100 iterations
        if i % 100 = 0 then
            let cost = costFunction model x y
            printfn "Iteration %d: Cost = %.4f, a = %.4f, b = %.4f" i cost model.a model.b
    
    model

// Example usage
[<EntryPoint>]
let main argv =
    // Sample data points (x, y)
    let xData = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
    let yData = [| 2.1; 3.9; 6.2; 8.1; 9.8 |]
    
    // Initialize model with random parameters
    let initialModel = { a = 0.0; b = 0.0 }
    let learningRate = 0.01
    let iterations = 1000
    
    printfn "Starting gradient descent..."
    let finalModel = gradientDescent xData yData initialModel learningRate iterations
    
    printfn "\nFinal model parameters:"
    printfn "a = %.4f" finalModel.a
    printfn "b = %.4f" finalModel.b
    
    // Calculate final cost
    let finalCost = costFunction finalModel xData yData
    printfn "Final cost: %.4f" finalCost
    
    0 // Return success code
```

This F# implementation demonstrates:

1. **Linear Model Definition**: A simple linear regression model with parameters `a` and `b`
2. **Cost Function**: Computes Mean Squared Error between predictions and actual values
3. **Gradient Computation**: Calculates partial derivatives for parameter updates
4. **Gradient Descent Algorithm**: Iteratively updates model parameters using the learning rate
5. **Example Usage**: Shows how to apply the algorithm to sample data

Key F# features used:
- **Type Definitions**: `type LinearModel = { a: float; b: float }`
- **Array Operations**: `Array.map`, `Array.zip`, `Array.sumBy`
- **Mutable Variables**: `mutable model` for parameter updates
- **Pattern Matching**: Simple function definitions and parameter handling
- **Printf Formatting**: For displaying progress information

The algorithm will converge to find the optimal linear relationship between x and y values, demonstrating how gradient descent works in practice.