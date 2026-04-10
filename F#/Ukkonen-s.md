# Ukkonen's Algorithm Implementation in F#

Here's a complete implementation of Ukkonen's algorithm for constructing suffix trees in F#:

```fsharp
open System
open System.Collections.Generic

// Suffix tree node representation
type SuffixTreeNode = {
    Id: int
    Children: Map<char, SuffixTreeNode>
    Start: int
    End: int
    SuffixIndex: int option
}

// Suffix tree structure
type SuffixTree = {
    Root: SuffixTreeNode
    Text: string
    NodeCount: int
    ActiveNode: SuffixTreeNode
    ActiveEdge: int
    ActiveLength: int
    RemainingSuffixCount: int
    LastNewNode: SuffixTreeNode option
}

// Utility functions
let createNode (id: int) (start: int) (end_: int) : SuffixTreeNode =
    {
        Id = id
        Children = Map.empty
        Start = start
        End = end_
        SuffixIndex = None
    }

let createRootNode () : SuffixTreeNode =
    createNode 0 0 -1

// Ukkonen's algorithm implementation
let buildSuffixTree (text: string) : SuffixTree =
    let text = text + "$" // Add terminal character
    let n = text.Length
    let mutable nodeCount = 1
    let root = createRootNode()
    
    // Initialize suffix tree
    let suffixTree = {
        Root = root
        Text = text
        NodeCount = nodeCount
        ActiveNode = root
        ActiveEdge = 0
        ActiveLength = 0
        RemainingSuffixCount = 0
        LastNewNode = None
    }
    
    // Helper function to get character at position
    let charAt (index: int) : char =
        if index < text.Length then text.[index] else '\0'
    
    // Helper function to get edge length
    let edgeLength (node: SuffixTreeNode) : int =
        node.End - node.Start + 1
    
    // Helper function to walk down
    let rec walkDown (node: SuffixTreeNode) (length: int) : SuffixTreeNode =
        if length < edgeLength node then
            node
        else
            let nextChar = charAt (node.Start + length)
            let nextNode = 
                match node.Children.TryFind nextChar with
                | Some child -> child
                | None -> node
            walkDown nextNode (length - edgeLength node)
    
    // Main algorithm
    let rec build (tree: SuffixTree) (i: int) : SuffixTree =
        if i >= n then
            tree
        else
            let tree = { tree with RemainingSuffixCount = tree.RemainingSuffixCount + 1 }
            tree.LastNewNode <- None
            
            // Extend suffix tree
            let rec extend (tree: SuffixTree) : SuffixTree =
                if tree.ActiveLength = 0 then
                    // Rule 2: If active length is 0, create new leaf
                    let newNode = 
                        let node = createNode tree.NodeCount i n - 1
                        { node with SuffixIndex = Some (i - tree.RemainingSuffixCount + 1) }
                    
                    let newTree = 
                        { tree with 
                            NodeCount = tree.NodeCount + 1
                            Root = 
                                { tree.Root with 
                                    Children = 
                                        tree.Root.Children.Add(tree.Text.[i], newNode) } }
                    { newTree with ActiveNode = newTree.Root }
                else
                    // Check if we can extend
                    let edgeChar = charAt (tree.ActiveNode.Start + tree.ActiveLength - 1)
                    match tree.ActiveNode.Children.TryFind edgeChar with
                    | Some child ->
                        // Rule 3: If we can extend, move to next node
                        let nextNode = walkDown child tree.ActiveLength
                        if nextNode = child then
                            // Rule 3: We can extend the edge
                            { tree with ActiveLength = tree.ActiveLength + 1 }
                        else
                            // Rule 3: We need to split the edge
                            let splitPoint = tree.ActiveNode.Start + tree.ActiveLength - 1
                            let splitNode = 
                                createNode tree.NodeCount tree.ActiveNode.Start splitPoint
                            
                            let newTree = 
                                { tree with 
                                    NodeCount = tree.NodeCount + 1
                                    Root = 
                                        { tree.Root with 
                                            Children = 
                                                tree.Root.Children.Add(edgeChar, splitNode) } }
                            
                            { newTree with 
                                ActiveNode = splitNode
                                ActiveLength = tree.ActiveLength - 1 }
                    | None ->
                        // Rule 2: Create new leaf
                        let newNode = 
                            let node = createNode tree.NodeCount i n - 1
                            { node with SuffixIndex = Some (i - tree.RemainingSuffixCount + 1) }
                        
                        let newTree = 
                            { tree with 
                                NodeCount = tree.NodeCount + 1
                                Root = 
                                    { tree.Root with 
                                        Children = 
                                            tree.Root.Children.Add(edgeChar, newNode) } }
                        { newTree with ActiveNode = newTree.Root }
            
            let tree = extend tree
            { tree with RemainingSuffixCount = tree.RemainingSuffixCount - 1 }
    
    // Process each character
    let rec process (tree: SuffixTree) (i: int) : SuffixTree =
        if i >= n then
            tree
        else
            let tree = build tree i
            process tree (i + 1)
    
    process suffixTree 0

// Utility function to print suffix tree
let printSuffixTree (tree: SuffixTree) =
    let rec printNode (node: SuffixTreeNode) (depth: int) =
        let indent = String.replicate depth "  "
        let edgeLabel = 
            if node.Start >= 0 && node.End >= 0 then
                tree.Text.[node.Start..node.End]
            else
                ""
        printfn "%sNode %d: %s" indent node.Id edgeLabel
        
        if node.SuffixIndex.IsSome then
            printfn "%s  Suffix Index: %d" indent node.SuffixIndex.Value
        
        Map.iter (fun key value -> printNode value (depth + 1)) node.Children
    
    printNode tree.Root 0

// Example usage
let example () =
    let text = "banana"
    printfn "Building suffix tree for: %s" text
    
    let suffixTree = buildSuffixTree text
    printfn "\nSuffix Tree Structure:"
    printSuffixTree suffixTree
    
    printfn "\nText: %s" text
    printfn "Length: %d" text.Length

// Run the example
example()
```

## Key Features of this Implementation:

### 1. **Data Structures**
- `SuffixTreeNode`: Represents nodes in the suffix tree with:
  - `Id`: Node identifier
  - `Children`: Map of characters to child nodes
  - `Start` and `End`: Range of characters in the text
  - `SuffixIndex`: Index of suffix ending at this node

### 2. **Algorithm Components**
- **Active Point Management**: Tracks current position in the tree
- **Edge Length Calculation**: Computes length of edges between nodes
- **Walk Down**: Moves through the tree efficiently
- **Three Rules of Ukkonen's Algorithm**:
  - Rule 1: Extension rule for existing edges
  - Rule 2: Extension rule for new leaves
  - Rule 3: Extension rule for edge splitting

### 3. **Time Complexity**
- **Time**: O(n) where n is the length of the text
- **Space**: O(n) for storing the suffix tree

### 4. **Usage Example**
```fsharp
let text = "banana"
let tree = buildSuffixTree text
printSuffixTree tree
```

This implementation demonstrates the core principles of Ukkonen's algorithm while maintaining F#'s functional programming style with immutable data structures and recursive algorithms.

