```fsharp
// Linear Regression Implementation in F#

open System
open MathNet.Numerics.LinearAlgebra

// Define a simple linear regression model
type LinearRegressionModel = {
    Weight: float
    Bias: float
}

// Simple implementation of linear regression using least squares
let simpleLinearRegression (x: float[]) (y: float[]) : LinearRegressionModel =
    let n = float x.Length
    
    // Calculate means
    let meanX = x |> Array.sum |> fun s -> s / n
    let meanY = y |> Array.sum |> fun s -> s / n
    
    // Calculate slope (weight) using least squares formula
    let numerator = 
        Array.zip x y
        |> Array.sumBy (fun (xi, yi) -> (xi - meanX) * (yi - meanY))
    
    let denominator = 
        x
        |> Array.sumBy (fun xi -> (xi - meanX) ** 2.0)
    
    let weight = numerator / denominator
    let bias = meanY - weight * meanX
    
    { Weight = weight; Bias = bias }

// More advanced implementation using Matrix operations
let matrixLinearRegression (xMatrix: float[,]) (yVector: float[]) : LinearRegressionModel =
    let x = Matrix<float>.Build.DenseOfArray(xMatrix)
    let y = Vector<float>.Build.Dense(yVector)
    
    // Add column of ones for bias term
    let xWithBias = 
        Matrix<float>.Build.Dense(x.RowCount, x.ColumnCount + 1, 
            fun i j -> 
                if j = 0 then 1.0 else x.[i, j - 1])
    
    // Calculate coefficients using normal equation: (X^T * X)^(-1) * X^T * y
    let xt = xWithBias.Transpose()
    let xtX = xt * xWithBias
    let xtXInv = xtX.Inverse()
    let xty = xt * y
    let coefficients = xtXInv * xty
    
    { Weight = coefficients.[1]; Bias = coefficients.[0] }

// Example usage
let example1() =
    // Sample data: height (x) vs weight (y)
    let heights = [| 150.; 160.; 170.; 180.; 190. |]
    let weights = [| 50.; 60.; 70.; 80.; 90. |]
    
    // Fit the model
    let model = simpleLinearRegression heights weights
    
    printfn "Simple Linear Regression Model:"
    printfn "Weight (slope): %f" model.Weight
    printfn "Bias (intercept): %f" model.Bias
    printfn "Equation: y = %f * x + %f" model.Weight model.Bias
    
    // Make predictions
    let predictedWeight = model.Weight * 175.0 + model.Bias
    printfn "Predicted weight for height 175cm: %f kg" predictedWeight

let example2() =
    // Using matrix approach with more data points
    let xData = [||
        [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
        [| 2.0; 3.0; 4.0; 5.0; 6.0 |]
        [| 3.0; 4.0; 5.0; 6.0; 7.0 |]
        [| 4.0; 5.0; 6.0; 7.0; 8.0 |]
        [| 5.0; 6.0; 7.0; 8.0; 9.0 |]
    |]
    
    let yData = [| 3.0; 5.0; 7.0; 9.0; 11.0 |]
    
    try
        let model = matrixLinearRegression xData yData
        printfn "\nMatrix-based Linear Regression:"
        printfn "Weight: %f" model.Weight
        printfn "Bias: %f" model.Bias
    with
    | ex -> printfn "Error in matrix calculation: %s" ex.Message

// Run examples
example1()
example2()

// Function to calculate R-squared for model evaluation
let calculateRSquared (x: float[]) (y: float[]) (model: LinearRegressionModel) : float =
    let yPredicted = x |> Array.map (fun xi -> model.Weight * xi + model.Bias)
    let yMean = y |> Array.sum |> fun s -> s / float y.Length
    
    let ssRes = 
        Array.zip y yPredicted
        |> Array.sumBy (fun (yi, yiPred) -> (yi - yiPred) ** 2.0)
    
    let ssTot = 
        y
        |> Array.sumBy (fun yi -> (yi - yMean) ** 2.0)
    
    1.0 - (ssRes / ssTot)

// Example with R-squared calculation
let exampleWithEvaluation() =
    let x = [| 1.; 2.; 3.; 4.; 5. |]
    let y = [| 2.; 4.; 6.; 8.; 10. |]
    
    let model = simpleLinearRegression x y
    let rSquared = calculateRSquared x y model
    
    printfn "\nModel Evaluation:"
    printfn "R-squared: %f" rSquared

exampleWithEvaluation()
```

This F# implementation shows:

1. **Simple Linear Regression**: Basic implementation using the least squares method
2. **Matrix-based Implementation**: Using MathNet.Numerics for more complex matrix operations
3. **Model Evaluation**: Includes R-squared calculation to assess model performance
4. **Error Handling**: Basic exception handling for matrix operations
5. **Example Usage**: Complete working examples demonstrating the algorithms

Key features:
- Uses F#'s functional programming style with immutable data structures
- Implements both simple and advanced approaches
- Includes proper type definitions and pattern matching
- Demonstrates practical usage with real-world data examples
- Provides model evaluation metrics