```fsharp
// Random Forest Implementation in F#

open System
open System.Collections.Generic

// Define a simple data point type
type DataPoint = {
    Features: float[]
    Label: int
}

// Decision Tree Node
type TreeNode = 
    | Leaf of int
    | Split of int * float * TreeNode * TreeNode  // feature_index, threshold, left_child, right_child

// Simple Decision Tree implementation
type DecisionTree() =
    let mutable root: TreeNode option = None
    
    member this.Train(data: DataPoint[], maxDepth: int, minSamples: int) =
        let rec buildTree (points: DataPoint[]) (depth: int) : TreeNode =
            if depth >= maxDepth || points.Length <= minSamples then
                // Create leaf node with most common label
                let labels = points |> Array.map (fun p -> p.Label)
                let mostCommon = 
                    labels 
                    |> Seq.groupBy id 
                    |> Seq.maxBy (fun (_, g) -> Seq.length g) 
                    |> fst
                Leaf(mostCommon)
            else
                // Find best split
                let bestSplit = findBestSplit points
                match bestSplit with
                | None -> 
                    let labels = points |> Array.map (fun p -> p.Label)
                    let mostCommon = 
                        labels 
                        |> Seq.groupBy id 
                        |> Seq.maxBy (fun (_, g) -> Seq.length g) 
                        |> fst
                    Leaf(mostCommon)
                | Some (featureIndex, threshold) ->
                    let leftPoints = points |> Array.filter (fun p -> p.Features.[featureIndex] <= threshold)
                    let rightPoints = points |> Array.filter (fun p -> p.Features.[featureIndex] > threshold)
                    
                    if leftPoints.Length = 0 || rightPoints.Length = 0 then
                        let labels = points |> Array.map (fun p -> p.Label)
                        let mostCommon = 
                            labels 
                            |> Seq.groupBy id 
                            |> Seq.maxBy (fun (_, g) -> Seq.length g) 
                            |> fst
                        Leaf(mostCommon)
                    else
                        let leftChild = buildTree leftPoints (depth + 1)
                        let rightChild = buildTree rightPoints (depth + 1)
                        Split(featureIndex, threshold, leftChild, rightChild)
        
        root <- Some (buildTree data 0)
    
    member this.Predict(features: float[]) : int =
        let rec predictNode (node: TreeNode) : int =
            match node with
            | Leaf(label) -> label
            | Split(featureIndex, threshold, leftChild, rightChild) ->
                if features.[featureIndex] <= threshold then
                    predictNode leftChild
                else
                    predictNode rightChild
        
        match root with
        | Some node -> predictNode node
        | None -> failwith "Model not trained"
    
    // Simple implementation to find best split (in practice, this would be more sophisticated)
    let findBestSplit (points: DataPoint[]) : (int * float) option =
        if points.Length = 0 then None
        else
            // For demonstration, we'll just pick a random feature and threshold
            let numFeatures = points.[0].Features.Length
            let featureIndex = Random().Next(numFeatures)
            let values = points |> Array.map (fun p -> p.Features.[featureIndex])
            let minVal = values |> Array.min
            let maxVal = values |> Array.max
            let threshold = (minVal + maxVal) / 2.0
            Some(featureIndex, threshold)

// Random Forest implementation
type RandomForest(numTrees: int, maxDepth: int, minSamples: int) =
    let trees = ref [| |]
    
    member this.Train(data: DataPoint[]) =
        let treeArray = Array.zeroCreate numTrees
        
        for i in 0 .. numTrees - 1 do
            // Create bootstrap sample
            let bootstrapSample = createBootstrapSample data
            let tree = DecisionTree()
            tree.Train(bootstrapSample, maxDepth, minSamples)
            treeArray.[i] <- tree
        
        trees := treeArray
    
    member this.Predict(features: float[]) : int =
        let predictions = 
            trees.Value 
            |> Array.map (fun tree -> tree.Predict(features))
        
        // Return majority vote
        predictions 
        |> Seq.groupBy id 
        |> Seq.maxBy (fun (_, group) -> Seq.length group) 
        |> fst
    
    member this.PredictProba(features: float[]) : float[] =
        let predictions = 
            trees.Value 
            |> Array.map (fun tree -> tree.Predict(features))
        
        // Return probability distribution
        let total = float predictions.Length
        [0 .. 2] // Assuming binary classification with labels 0 and 1
        |> List.map (fun label -> 
            predictions 
            |> Seq.filter (fun p -> p = label)
            |> Seq.length
            |> float
            |> fun x -> x / total)

// Helper function to create bootstrap sample
let createBootstrapSample (data: DataPoint[]) : DataPoint[] =
    let random = Random()
    let n = data.Length
    Array.init n (fun _ -> data.[random.Next(n)])

// Example usage
[<EntryPoint>]
let main argv =
    // Create sample data
    let sampleData = [
        { Features = [| 1.0; 2.0 |]; Label = 0 }
        { Features = [| 2.0; 3.0 |]; Label = 0 }
        { Features = [| 3.0; 1.0 |]; Label = 1 }
        { Features = [| 4.0; 2.0 |]; Label = 1 }
        { Features = [| 5.0; 3.0 |]; Label = 1 }
        { Features = [| 6.0; 4.0 |]; Label = 0 }
    ]
    
    // Create and train Random Forest
    let rf = RandomForest(5, 3, 1)
    rf.Train(sampleData |> Array.ofList)
    
    // Make prediction
    let testPoint = [| 2.5; 2.5 |]
    let prediction = rf.Predict(testPoint)
    printfn "Prediction for [%f; %f]: %d" testPoint.[0] testPoint.[1] prediction
    
    // Get probability
    let proba = rf.PredictProba(testPoint)
    printfn "Probabilities: [%f; %f]" proba.[0] proba.[1]
    
    0
```