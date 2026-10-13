```fsharp
// ID3 Decision Tree Algorithm Implementation in F#

open System
open System.Collections.Generic

// Define the data structure for a decision tree node
type DecisionTreeNode =
    | Leaf of string
    | Internal of string * Map<string, DecisionTreeNode>

// Define the dataset type
type DataPoint = {
    Attributes: Map<string, string>
    Class: string
}

// Calculate entropy of a dataset
let calculateEntropy (data: DataPoint[]) : float =
    let total = float data.Length
    if total = 0.0 then 0.0
    else
        let classCounts = 
            data 
            |> Array.groupBy (fun dp -> dp.Class)
            |> Array.map (fun (classLabel, group) -> (classLabel, float group.Length))
        
        classCounts
        |> Array.sumBy (fun (_, count) -> 
            let p = count / total
            if p = 0.0 then 0.0 else -p * log2 p)
    
and log2 x = log x / log 2.0

// Calculate information gain for an attribute
let calculateInformationGain (data: DataPoint[]) (attribute: string) : float =
    let total = float data.Length
    let weightedEntropy = 
        data
        |> Array.groupBy (fun dp -> dp.Attributes.[attribute])
        |> Array.sumBy (fun (value, subset) ->
            let subsetSize = float subset.Length
            if subsetSize = 0.0 then 0.0 else
                let entropy = calculateEntropy subset
                (subsetSize / total) * entropy)
    
    let totalEntropy = calculateEntropy data
    totalEntropy - weightedEntropy

// Find the best attribute to split on
let findBestAttribute (data: DataPoint[]) (attributes: string[]) : string =
    attributes
    |> Array.maxBy (calculateInformationGain data)

// Check if all instances have the same class
let isPure (data: DataPoint[]) : bool =
    match data with
    | [||] -> true
    | _ -> 
        let firstClass = data.[0].Class
        data |> Array.forall (fun dp -> dp.Class = firstClass)

// Get the most common class in a dataset
let getMajorityClass (data: DataPoint[]) : string =
    data
    |> Array.groupBy (fun dp -> dp.Class)
    |> Array.sortByDescending (fun (_, group) -> group.Length)
    |> Array.head
    |> fun (_, group) -> group.[0].Class

// ID3 Algorithm Implementation
let rec id3 (data: DataPoint[]) (attributes: string[]) : DecisionTreeNode =
    // Base case 1: if no data, return default leaf
    if data.Length = 0 then Leaf "Unknown"
    
    // Base case 2: if all instances have same class, return leaf node
    elif isPure data then Leaf data.[0].Class
    
    // Base case 3: if no attributes left, return majority class
    elif attributes.Length = 0 then Leaf (getMajorityClass data)
    
    else
        // Find best attribute to split on
        let bestAttribute = findBestAttribute data attributes
        
        // Get all possible values for the best attribute
        let attributeValues = 
            data 
            |> Array.map (fun dp -> dp.Attributes.[bestAttribute])
            |> Set.ofArray
        
        // Create internal node with children
        let children = 
            attributeValues
            |> Set.toArray
            |> Array.map (fun value ->
                let subset = 
                    data 
                    |> Array.filter (fun dp -> dp.Attributes.[bestAttribute] = value)
                
                // Remove the best attribute from remaining attributes
                let remainingAttributes = 
                    attributes 
                    |> Array.filter (fun attr -> attr <> bestAttribute)
                
                (value, id3 subset remainingAttributes))
            |> Map.ofArray
        
        Internal (bestAttribute, children)

// Example usage
let exampleData = [
    { Attributes = Map.ofList [("Outlook", "Sunny"); ("Temperature", "Hot"); ("Humidity", "High"); ("Wind", "Weak")]; Class = "No" }
    { Attributes = Map.ofList [("Outlook", "Sunny"); ("Temperature", "Hot"); ("Humidity", "High"); ("Wind", "Strong")]; Class = "No" }
    { Attributes = Map.ofList [("Outlook", "Overcast"); ("Temperature", "Hot"); ("Humidity", "High"); ("Wind", "Weak")]; Class = "Yes" }
    { Attributes = Map.ofList [("Outlook", "Rain"); ("Temperature", "Mild"); ("Humidity", "High"); ("Wind", "Weak")]; Class = "Yes" }
    { Attributes = Map.ofList [("Outlook", "Rain"); ("Temperature", "Cool"); ("Humidity", "Normal"); ("Wind", "Weak")]; Class = "Yes" }
    { Attributes = Map.ofList [("Outlook", "Rain"); ("Temperature", "Cool"); ("Humidity", "Normal"); ("Wind", "Strong")]; Class = "No" }
    { Attributes = Map.ofList [("Outlook", "Overcast"); ("Temperature", "Cool"); ("Humidity", "Normal"); ("Wind", "Strong")]; Class = "Yes" }
    { Attributes = Map.ofList [("Outlook", "Sunny"); ("Temperature", "Mild"); ("Humidity", "High"); ("Wind", "Weak")]; Class = "No" }
    { Attributes = Map.ofList [("Outlook", "Sunny"); ("Temperature", "Cool"); ("Humidity", "Normal"); ("Wind", "Weak")]; Class = "Yes" }
    { Attributes = Map.ofList [("Outlook", "Rain"); ("Temperature", "Mild"); ("Humidity", "Normal"); ("Wind", "Strong")]; Class = "Yes" }
    { Attributes = Map.ofList [("Outlook", "Sunny"); ("Temperature", "Mild"); ("Humidity", "Normal"); ("Wind", "Strong")]; Class = "Yes" }
    { Attributes = Map.ofList [("Outlook", "Overcast"); ("Temperature", "Mild"); ("Humidity", "High"); ("Wind", "Strong")]; Class = "Yes" }
    { Attributes = Map.ofList [("Outlook", "Overcast"); ("Temperature", "Hot"); ("Humidity", "Normal"); ("Wind", "Weak")]; Class = "Yes" }
    { Attributes = Map.ofList [("Outlook", "Rain"); ("Temperature", "Mild"); ("Humidity", "High"); ("Wind", "Strong")]; Class = "No" }
]

let allAttributes = ["Outlook"; "Temperature"; "Humidity"; "Wind"]

// Build the decision tree
let decisionTree = id3 exampleData allAttributes

// Function to classify a new instance
let rec classify (tree: DecisionTreeNode) (instance: Map<string, string>) : string =
    match tree with
    | Leaf classLabel -> classLabel
    | Internal(attribute, children) ->
        match instance.TryFind attribute with
        | Some value ->
            match Map.tryFind value children with
            | Some childNode -> classify childNode instance
            | None -> "Unknown"
        | None -> "Unknown"

// Example classification
let testInstance = Map.ofList [("Outlook", "Sunny"); ("Temperature", "Cool"); ("Humidity", "High"); ("Wind", "Weak")]
let prediction = classify decisionTree testInstance

printfn "Decision Tree Prediction: %s" prediction
```

This F# implementation of the ID3 algorithm includes:

1. **Data Structures**: 
   - `DecisionTreeNode` type for representing tree nodes (leaf or internal)
   - `DataPoint` record for storing training examples

2. **Core Functions**:
   - `calculateEntropy`: Computes entropy of a dataset
   - `calculateInformationGain`: Calculates information gain for an attribute
   - `findBestAttribute`: Selects the best attribute to split on
   - `id3`: Main recursive ID3 algorithm implementation
   - `classify`: Function to classify new instances using the trained tree

3. **Key Features**:
   - Handles edge cases (empty data, pure datasets, no attributes)
   - Uses entropy-based splitting criterion
   - Implements information gain calculation
   - Includes example dataset and usage demonstration

The algorithm builds a decision tree that can classify new instances based on the learned rules from the training data.