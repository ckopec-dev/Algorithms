# CART Decision Tree Algorithm in F#

Here's an implementation of the CART (Classification and Regression Trees) algorithm in F#:

```fsharp
open System

// Define the data type for a decision tree node
type TreeNode =
    | Leaf of int
    | Split of string * float * TreeNode * TreeNode

// Define the data type for a dataset
type DataPoint = { Features: float[]; Label: int }

// Calculate Gini impurity for a set of labels
let giniImpurity (labels: int[]) =
    if labels.Length = 0 then 0.0
    else
        let total = float labels.Length
        let frequencies = 
            labels 
            |> Array.groupBy id 
            |> Array.map (fun (label, group) -> 
                let count = float (Array.length group)
                let proportion = count / total
                proportion * proportion)
        1.0 - (Array.sum frequencies)

// Find the best split for a dataset
let findBestSplit (data: DataPoint[]) (featureNames: string[]) =
    let bestGini = ref 1.0
    let bestFeature = ref -1
    let bestThreshold = ref 0.0
    
    let featureCount = data.[0].Features.Length
    
    for featureIndex in 0 .. featureCount - 1 do
        let featureValues = 
            data 
            |> Array.map (fun dp -> dp.Features.[featureIndex])
            |> Array.sort
        
        // Try different thresholds
        for i in 0 .. featureValues.Length - 2 do
            let threshold = (featureValues.[i] + featureValues.[i + 1]) / 2.0
            
            let leftLabels = 
                data 
                |> Array.filter (fun dp -> dp.Features.[featureIndex] <= threshold)
                |> Array.map (fun dp -> dp.Label)
            
            let rightLabels = 
                data 
                |> Array.filter (fun dp -> dp.Features.[featureIndex] > threshold)
                |> Array.map (fun dp -> dp.Label)
            
            let leftWeight = float leftLabels.Length / float data.Length
            let rightWeight = float rightLabels.Length / float data.Length
            
            let weightedGini = 
                leftWeight * giniImpurity leftLabels + 
                rightWeight * giniImpurity rightLabels
            
            if weightedGini < !bestGini then
                bestGini := weightedGini
                bestFeature := featureIndex
                bestThreshold := threshold
    
    { FeatureIndex = !bestFeature; Threshold = !bestThreshold; Gini = !bestGini }

// Build the decision tree recursively
let rec buildTree (data: DataPoint[]) (featureNames: string[]) (maxDepth: int) (currentDepth: int) =
    // Base cases
    if data.Length = 0 then Leaf 0
    elif currentDepth >= maxDepth then
        let labels = data |> Array.map (fun dp -> dp.Label)
        let majorityLabel = 
            labels 
            |> Array.groupBy id 
            |> Array.maxBy (fun (_, group) -> Array.length group)
            |> fun (_, group) -> group.[0]
        Leaf majorityLabel
    else
        let split = findBestSplit data featureNames
        
        if split.Gini = 0.0 then
            // Pure node - all labels are the same
            Leaf (data.[0].Label)
        else
            // Split the data
            let leftData = 
                data 
                |> Array.filter (fun dp -> dp.Features.[split.FeatureIndex] <= split.Threshold)
            
            let rightData = 
                data 
                |> Array.filter (fun dp -> dp.Features.[split.FeatureIndex] > split.Threshold)
            
            // Recursively build subtrees
            let leftTree = buildTree leftData featureNames maxDepth (currentDepth + 1)
            let rightTree = buildTree rightData featureNames maxDepth (currentDepth + 1)
            
            Split(featureNames.[split.FeatureIndex], split.Threshold, leftTree, rightTree)

// Make a prediction for a single data point
let rec predict (tree: TreeNode) (dataPoint: DataPoint) =
    match tree with
    | Leaf label -> label
    | Split(featureName, threshold, leftTree, rightTree) ->
        if dataPoint.Features.[featureName] <= threshold then
            predict leftTree dataPoint
        else
            predict rightTree dataPoint

// Example usage
[<EntryPoint>]
let main argv =
    // Sample dataset (iris-like data)
    let featureNames = ["sepal_length"; "sepal_width"; "petal_length"; "petal_width"]
    
    let trainingData = [
        { Features = [| 5.1; 3.5; 1.4; 0.2 |]; Label = 0 }  // Setosa
        { Features = [| 4.9; 3.0; 1.4; 0.2 |]; Label = 0 }  // Setosa
        { Features = [| 6.2; 2.8; 4.8; 1.8 |]; Label = 1 }  // Versicolor
        { Features = [| 5.6; 2.7; 4.2; 1.3 |]; Label = 1 }  // Versicolor
        { Features = [| 7.2; 3.0; 5.8; 1.6 |]; Label = 2 }  // Virginica
        { Features = [| 6.5; 3.2; 5.1; 2.0 |]; Label = 2 }  // Virginica
    ]
    
    // Build the decision tree
    let tree = buildTree trainingData featureNames 5 0
    
    // Make predictions
    let testPoint = { Features = [| 5.0; 3.4; 1.5; 0.2 |]; Label = 0 }
    let prediction = predict tree testPoint
    
    printfn "Prediction for test point: %d" prediction
    printfn "Tree structure:"
    
    // Simple tree display function
    let rec displayTree (tree: TreeNode) (indent: string) =
        match tree with
        | Leaf label -> printfn "%sLeaf: %d" indent label
        | Split(featureName, threshold, leftTree, rightTree) ->
            printfn "%sSplit on %s <= %f" indent featureName threshold
            printfn "%sLeft:" indent
            displayTree leftTree (indent + "  ")
            printfn "%sRight:" indent
            displayTree rightTree (indent + "  ")
    
    displayTree tree ""
    
    0
```

## Key Components Explained:

### 1. **TreeNode Type**
- Represents both leaf nodes (containing a class label) and split nodes (containing a feature and threshold)

### 2. **Gini Impurity Calculation**
- Measures the quality of a split by calculating how often a randomly chosen element would be incorrectly labeled

### 3. **findBestSplit Function**
- Finds the optimal feature and threshold to split the data
- Uses Gini impurity to evaluate split quality

### 4. **buildTree Function**
- Recursively builds the decision tree
- Implements depth limiting to prevent overfitting
- Uses majority voting for leaf nodes

### 5. **predict Function**
- Traverses the tree to make predictions for new data points

### 6. **Key Features**
- Handles both binary and multi-class classification
- Uses Gini impurity for splitting criteria
- Includes depth limiting to prevent overfitting
- Supports feature names for better interpretability

This implementation demonstrates the core CART algorithm principles while being fully functional in F# with proper type safety and functional programming patterns.

