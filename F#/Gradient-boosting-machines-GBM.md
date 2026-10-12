```fsharp
// Gradient Boosting Machines (GBM) Implementation in F#
// This is a simplified implementation focusing on the core concepts

open System
open System.Collections.Generic

// Define a simple decision tree node for GBM
type TreeNode = {
    FeatureIndex: int
    Threshold: float
    Left: TreeNode option
    Right: TreeNode option
    Prediction: float
    IsLeaf: bool
}

// Simple Decision Tree implementation for GBM base learners
type DecisionTree() =
    member this.Predict(node: TreeNode, x: float[]) : float =
        match node with
        | { IsLeaf = true } -> node.Prediction
        | _ ->
            let featureValue = x.[node.FeatureIndex]
            if featureValue <= node.Threshold then
                match node.Left with
                | Some childNode -> this.Predict(childNode, x)
                | None -> node.Prediction
            else
                match node.Right with
                | Some childNode -> this.Predict(childNode, x)
                | None -> node.Prediction

// Gradient Boosting Machine implementation
type GradientBoostingMachine<'T> = {
    LearningRate: float
    NumberOfTrees: int
    MaxDepth: int
    Trees: TreeNode list
    BasePrediction: float
}

// Helper function to calculate mean squared error
let calculateMSE (actual: float[]) (predicted: float[]) : float =
    let differences = Array.zip actual predicted |> Array.map (fun (a, p) -> (a - p) ** 2.0)
    Array.sum differences / float actual.Length

// Simple implementation of GBM algorithm
let trainGBM (X: float[][]) (y: float[]) (learningRate: float) (numberOfTrees: int) (maxDepth: int) : GradientBoostingMachine<float> =
    
    // Initialize base prediction (mean of target values)
    let basePrediction = y |> Array.average
    
    // Initialize residuals
    let mutable residuals = y |> Array.copy
    
    // Store trees
    let mutable trees = []
    
    // Main boosting loop
    for i in 0 .. numberOfTrees - 1 do
        
        // Create a simple decision tree to fit the residuals
        // In practice, this would be more sophisticated
        let tree = {
            FeatureIndex = 0
            Threshold = 0.5
            Left = None
            Right = None
            Prediction = residuals |> Array.average
            IsLeaf = true
        }
        
        trees <- tree :: trees
        
        // Update residuals for next iteration (simplified)
        // In practice, this would involve fitting a new tree to current residuals
        let predictions = 
            X |> Array.map (fun x -> 
                if x.[0] <= 0.5 then tree.Prediction else -tree.Prediction)
        
        residuals <- 
            Array.zip y predictions 
            |> Array.map (fun (actual, pred) -> actual - pred)
    
    {
        LearningRate = learningRate
        NumberOfTrees = numberOfTrees
        MaxDepth = maxDepth
        Trees = List.rev trees
        BasePrediction = basePrediction
    }

// Prediction function for GBM
let predictGBM (model: GradientBoostingMachine<float>) (X: float[][]) : float[] =
    X |> Array.map (fun x ->
        let prediction = model.BasePrediction
        // Simple summation of tree predictions
        model.Trees 
        |> List.fold (fun acc tree -> 
            acc + model.LearningRate * tree.Prediction) prediction
    )

// Example usage
[<EntryPoint>]
let main argv =
    
    // Sample dataset
    let X = [| 
        [| 0.1; 0.2 |] 
        [| 0.3; 0.4 |] 
        [| 0.5; 0.6 |] 
        [| 0.7; 0.8 |] 
        [| 0.9; 1.0 |]
    |]
    
    let y = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
    
    // Train GBM model
    let gbmModel = 
        trainGBM X y 0.1 5 3
    
    // Make predictions
    let predictions = predictGBM gbmModel X
    
    printfn "Base prediction: %f" gbmModel.BasePrediction
    printfn "Predictions: %A" predictions
    printfn "Actual values: %A" y
    
    // Calculate MSE
    let mse = calculateMSE y predictions
    printfn "Mean Squared Error: %f" mse
    
    0 // return code
```

This F# implementation demonstrates:

1. **Core Components**:
   - `TreeNode` type representing decision tree nodes
   - `DecisionTree` class with prediction functionality
   - `GradientBoostingMachine` record storing model parameters and trees

2. **Key Features**:
   - Gradient boosting algorithm structure
   - Residual calculation and updating
   - Learning rate application
   - Tree-based ensemble approach

3. **Usage Example**:
   - Sample dataset creation
   - Model training with specified parameters
   - Prediction generation
   - Performance evaluation with MSE

**Note**: This is a simplified educational implementation. A production-ready GBM would include:
- More sophisticated tree building (CART, etc.)
- Proper gradient calculation
- Regularization techniques
- Cross-validation
- Feature importance calculation
- Better handling of categorical features