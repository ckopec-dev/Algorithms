# Decision Tree Learning Algorithm in F#

Here's a complete implementation of a decision tree learning algorithm in F#:

```fsharp
// Decision Tree Implementation in F#

type Attribute = string
type Value = string
type Label = string

// Data structure for a dataset row
type DataRow = {
    Attributes: Map<Attribute, Value>
    Label: Label
}

// Decision tree node types
type DecisionTreeNode =
    | Leaf of Label
    | Internal of Attribute * Map<Value, DecisionTreeNode>

// Decision Tree class
type DecisionTree() =
    member this.BuildTree(data: DataRow list, attributes: Attribute list, defaultLabel: Label) : DecisionTreeNode =
        match data with
        | [] -> Leaf defaultLabel
        | _ ->
            let labels = data |> List.map (fun row -> row.Label)
            let allSameLabel = labels |> List.distinct |> List.length = 1
            
            if allSameLabel then
                Leaf (labels.[0])
            elif attributes.IsEmpty then
                Leaf (this.majorityValue data)
            else
                let bestAttribute = this.selectBestAttribute data attributes
                let attributeValues = 
                    data 
                    |> List.collect (fun row -> 
                        match Map.tryFind bestAttribute row.Attributes with
                        | Some value -> [value]
                        | None -> [])
                    |> Set.ofList
                    |> Set.toList
                
                let subtrees = 
                    attributeValues 
                    |> List.map (fun value ->
                        let subset = 
                            data 
                            |> List.filter (fun row -> 
                                match Map.tryFind bestAttribute row.Attributes with
                                | Some v when v = value -> true
                                | _ -> false)
                        let newAttributes = attributes |> List.except [bestAttribute]
                        let newDefaultLabel = this.majorityValue data
                        (value, this.BuildTree(subset, newAttributes, newDefaultLabel)))
                    |> Map.ofList
                
                Internal (bestAttribute, subtrees)

    member this.selectBestAttribute(data: DataRow list, attributes: Attribute list) : Attribute =
        let bestGain = 
            attributes 
            |> List.maxBy (fun attr -> this.informationGain data attr)
        bestGain

    member this.informationGain(data: DataRow list, attribute: Attribute) : float =
        let totalEntropy = this.entropy data
        let weightedAverage = 
            data 
            |> this.splitByAttribute attribute
            |> List.sumBy (fun (value, subset) -> 
                let weight = float (List.length subset) / float (List.length data)
                weight * this.entropy subset)
        totalEntropy - weightedAverage

    member this.entropy(data: DataRow list) : float =
        if List.isEmpty data then 0.0
        else
            let labels = data |> List.map (fun row -> row.Label)
            let labelCounts = 
                labels 
                |> List.groupBy id 
                |> List.map (fun (label, count) -> (label, List.length count))
                |> Map.ofList
            
            let total = float (List.length labels)
            labelCounts 
            |> Map.toList
            |> List.sumBy (fun (_, count) ->
                let probability = float count / total
                if probability = 0.0 then 0.0 else -probability * log2 probability)

    member this.splitByAttribute(attribute: Attribute, data: DataRow list) : (Value * DataRow list) list =
        data 
        |> List.groupBy (fun row -> 
            match Map.tryFind attribute row.Attributes with
            | Some value -> value
            | None -> "Unknown")
        |> List.map (fun (value, subset) -> (value, subset))

    member this.majorityValue(data: DataRow list) : Label =
        let labels = data |> List.map (fun row -> row.Label)
        labels 
        |> List.groupBy id 
        |> List.maxBy (fun (_, group) -> List.length group)
        |> fun (label, _) -> label

    member this.classify(tree: DecisionTreeNode, instance: Map<Attribute, Value>) : Label =
        match tree with
        | Leaf label -> label
        | Internal(attribute, subtrees) ->
            match Map.tryFind attribute instance with
            | Some value ->
                match Map.tryFind value subtrees with
                | Some subtree -> this.classify subtree instance
                | None -> 
                    // If value not found in training data, return most common label
                    let labels = 
                        instance 
                        |> Map.toList 
                        |> List.map (fun (_, v) -> v)
                    labels.[0] // Simple fallback - in practice, you'd want better handling
            | None -> 
                // If attribute not present, return most common label
                let allLabels = 
                    instance 
                    |> Map.toList 
                    |> List.map (fun (_, v) -> v)
                allLabels.[0]

    member this.printTree(tree: DecisionTreeNode, indent: string = "") : unit =
        match tree with
        | Leaf label -> printfn "%sLabel: %s" indent label
        | Internal(attribute, subtrees) ->
            printfn "%sAttribute: %s" indent attribute
            for (value, subtree) in Map.toList subtrees do
                printfn "%s  %s:" indent value
                this.printTree subtree (indent + "    ")

// Helper function to calculate log base 2
let log2 x = log x / log 2.0

// Example usage
[<EntryPoint>]
let main argv =
    // Create sample dataset
    let data = [
        { Attributes = Map.ofList [("Outlook", "Sunny"); ("Temperature", "Hot"); ("Humidity", "High"); ("Wind", "Weak")]; Label = "No" }
        { Attributes = Map.ofList [("Outlook", "Sunny"); ("Temperature", "Hot"); ("Humidity", "High"); ("Wind", "Strong")]; Label = "No" }
        { Attributes = Map.ofList [("Outlook", "Overcast"); ("Temperature", "Hot"); ("Humidity", "High"); ("Wind", "Weak")]; Label = "Yes" }
        { Attributes = Map.ofList [("Outlook", "Rain"); ("Temperature", "Mild"); ("Humidity", "High"); ("Wind", "Weak")]; Label = "Yes" }
        { Attributes = Map.ofList [("Outlook", "Rain"); ("Temperature", "Cool"); ("Humidity", "Normal"); ("Wind", "Weak")]; Label = "Yes" }
        { Attributes = Map.ofList [("Outlook", "Rain"); ("Temperature", "Cool"); ("Humidity", "Normal"); ("Wind", "Strong")]; Label = "No" }
        { Attributes = Map.ofList [("Outlook", "Overcast"); ("Temperature", "Cool"); ("Humidity", "Normal"); ("Wind", "Strong")]; Label = "Yes" }
        { Attributes = Map.ofList [("Outlook", "Sunny"); ("Temperature", "Mild"); ("Humidity", "High"); ("Wind", "Weak")]; Label = "No" }
        { Attributes = Map.ofList [("Outlook", "Sunny"); ("Temperature", "Cool"); ("Humidity", "Normal"); ("Wind", "Weak")]; Label = "Yes" }
        { Attributes = Map.ofList [("Outlook", "Rain"); ("Temperature", "Mild"); ("Humidity", "Normal"); ("Wind", "Strong")]; Label = "Yes" }
        { Attributes = Map.ofList [("Outlook", "Sunny"); ("Temperature", "Mild"); ("Humidity", "Normal"); ("Wind", "Strong")]; Label = "Yes" }
        { Attributes = Map.ofList [("Outlook", "Overcast"); ("Temperature", "Mild"); ("Humidity", "High"); ("Wind", "Strong")]; Label = "Yes" }
        { Attributes = Map.ofList [("Outlook", "Overcast"); ("Temperature", "Hot"); ("Humidity", "Normal"); ("Wind", "Weak")]; Label = "Yes" }
        { Attributes = Map.ofList [("Outlook", "Rain"); ("Temperature", "Mild"); ("Humidity", "High"); ("Wind", "Strong")]; Label = "No" }
    ]

    let attributes = ["Outlook"; "Temperature"; "Humidity"; "Wind"]
    
    // Build the decision tree
    let tree = DecisionTree()
    let defaultLabel = "Yes" // Could be calculated from majority of data
    
    let learnedTree = tree.BuildTree(data, attributes, defaultLabel)
    
    // Print the tree
    printfn "Learned Decision Tree:"
    tree.printTree learnedTree
    
    // Test classification
    let testInstance = Map.ofList [("Outlook", "Sunny"); ("Temperature", "Cool"); ("Humidity", "High"); ("Wind", "Weak")]
    let prediction = tree.classify learnedTree testInstance
    printfn "\nPrediction for test instance: %s" prediction
    
    0 // return code
```

## Key Features of this Implementation:

1. **Data Structures**: 
   - `DataRow` for representing training examples
   - `DecisionTreeNode` as a discriminated union for tree structure

2. **Core Algorithms**:
   - Information gain calculation for attribute selection
   - Entropy computation for measuring impurity
   - Recursive tree building algorithm

3. **Main Functions**:
   - `BuildTree`: Main decision tree learning function
   - `selectBestAttribute`: Chooses the best attribute to split on
   - `informationGain`: Calculates information gain for each attribute
   - `entropy`: Computes entropy of a dataset
   - `classify`: Makes predictions on new instances
   - `printTree`: Visualizes the learned tree structure

4. **Example Usage**: 
   - Sample tennis playing dataset from the classic decision tree example
   - Demonstrates building and using the tree for classification

This implementation follows the ID3 algorithm principles and can be extended with additional features like pruning, handling continuous attributes, or different splitting criteria.