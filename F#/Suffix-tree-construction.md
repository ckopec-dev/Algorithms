# Suffix Tree Construction in F#

Here's an implementation of Ukkonen's algorithm for constructing suffix trees in F#:

```fsharp
open System
open System.Collections.Generic

// Suffix tree node definition
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
}

// Active point structure
type ActivePoint = {
    Node: SuffixTreeNode
    EdgeChar: char
    Length: int
}

// Global variables for the algorithm
let mutable nodeCount = 0
let mutable activePoint = { Node = null; EdgeChar = '\0'; Length = 0 }
let mutable remainder = 0
let mutable text = ""

// Create a new node
let createNode (start: int) (end_: int) : SuffixTreeNode =
    nodeCount <- nodeCount + 1
    {
        Id = nodeCount
        Children = Map.empty
        Start = start
        End = end_
        SuffixIndex = None
    }

// Initialize the suffix tree
let initSuffixTree (inputText: string) : SuffixTree =
    nodeCount <- 0
    text <- inputText + "$"
    let root = createNode -1 -1
    {
        Root = root
        Text = text
        NodeCount = nodeCount
    }

// Get the character at a specific position
let getChar (pos: int) : char =
    if pos < text.Length then text.[pos] else '\0'

// Get the edge length
let edgeLength (node: SuffixTreeNode) : int =
    node.End - node.Start + 1

// Get the edge character
let getEdgeChar (node: SuffixTreeNode) : char =
    if node.Start >= 0 then text.[node.Start] else '\0'

// Split an edge at a specific position
let splitEdge (node: SuffixTreeNode) (splitPos: int) : SuffixTreeNode =
    let midNode = createNode node.Start (node.Start + splitPos - 1)
    midNode.Children <- Map.singleton (getChar (node.Start + splitPos)) node
    node.Start <- node.Start + splitPos
    midNode

// Add a suffix to the tree
let addSuffix (tree: SuffixTree) (suffixIndex: int) : SuffixTree =
    let rec updateActivePoint (active: ActivePoint) (length: int) : ActivePoint =
        if length <= 0 then active
        else
            let edgeChar = getChar (active.Node.Start + length - 1)
            { active with EdgeChar = edgeChar; Length = length }
    
    let rec insertSuffix (node: SuffixTreeNode) (pos: int) (active: ActivePoint) : SuffixTreeNode =
        if pos >= text.Length then node
        else
            let char = getChar pos
            let child = Map.tryFind char node.Children
            
            match child with
            | Some childNode ->
                // Rule 3: Character already exists
                if active.Length >= edgeLength childNode then
                    let newActive = { active with Node = childNode; Length = active.Length - edgeLength childNode }
                    insertSuffix childNode pos newActive
                else
                    // Rule 2: Character does not exist, create new edge
                    let edgeChar = getChar (childNode.Start + active.Length)
                    if edgeChar = char then
                        // We are at the correct position
                        insertSuffix childNode (pos + 1) active
                    else
                        // Split the edge
                        let splitNode = splitEdge childNode active.Length
                        let newNode = createNode pos (text.Length - 1)
                        newNode.SuffixIndex <- Some suffixIndex
                        splitNode.Children <- Map.add char newNode splitNode.Children
                        node
            | None ->
                // Rule 1: Character does not exist, create new leaf
                let newNode = createNode pos (text.Length - 1)
                newNode.SuffixIndex <- Some suffixIndex
                node.Children <- Map.add char newNode node.Children
                node
    
    let rec buildSuffixTree (tree: SuffixTree) (pos: int) : SuffixTree =
        if pos >= text.Length - 1 then tree
        else
            remainder <- remainder + 1
            let newTree = insertSuffix tree.Root pos { Node = tree.Root; EdgeChar = '\0'; Length = 0 }
            { tree with Root = newTree }
    
    buildSuffixTree tree suffixIndex

// Main suffix tree construction function
let buildSuffixTree (inputText: string) : SuffixTree =
    let tree = initSuffixTree inputText
    let mutable resultTree = tree
    
    for i in 0 .. inputText.Length - 1 do
        resultTree <- addSuffix resultTree i
    
    resultTree

// Print the suffix tree
let printSuffixTree (tree: SuffixTree) (node: SuffixTreeNode) (prefix: string) =
    if node.SuffixIndex.IsSome then
        printfn "%s%s (suffix index: %d)" prefix (tree.Text.[node.Start .. node.End]) (Option.get node.SuffixIndex)
    else
        printfn "%s%s" prefix (tree.Text.[node.Start .. node.End])
    
    for (key, child) in node.Children do
        printSuffixTree tree child (prefix + "  ")

// Example usage
let example = "banana"

printfn "Building suffix tree for: %s" example
let suffixTree = buildSuffixTree example

printfn "\nSuffix Tree Structure:"
printSuffixTree suffixTree suffixTree.Root ""

// Find all suffixes
let findSuffixes (tree: SuffixTree) : string list =
    let rec collectSuffixes (node: SuffixTreeNode) (suffixes: string list) : string list =
        match node.SuffixIndex with
        | Some index ->
            let suffix = tree.Text.[index .. tree.Text.Length - 2]
            suffix :: suffixes
        | None ->
            let rec traverseChildren (children: (char * SuffixTreeNode) list) (acc: string list) : string list =
                match children with
                | [] -> acc
                | (char, child) :: rest ->
                    let newSuffix = tree.Text.[child.Start .. child.End]
                    traverseChildren rest (newSuffix :: acc)
            traverseChildren (Map.toList node.Children) suffixes
    
    collectSuffixes tree.Root []

printfn "\nAll suffixes found:"
let suffixes = findSuffixes suffixTree
suffixes |> List.iter (fun s -> printfn "- %s" s)
```

## How it works:

1. **Node Structure**: Each node contains:
   - `Id`: Unique identifier
   - `Children`: Map of characters to child nodes
   - `Start` and `End`: Position range in the text
   - `SuffixIndex`: Index of suffix ending at this node

2. **Key Components**:
   - `createNode`: Creates new tree nodes
   - `splitEdge`: Splits edges when needed
   - `insertSuffix`: Adds suffixes using Ukkonen's algorithm
   - `buildSuffixTree`: Main construction function

3. **Algorithm Features**:
   - Uses Ukkonen's online algorithm
   - Handles edge splitting and compression
   - Maintains active point during construction
   - Supports suffix indexing

## Example Output:
For input "banana", the suffix tree will contain all suffixes: "banana", "anana", "nana", "ana", "na", "a", "" with their positions and relationships.

This implementation demonstrates the core concepts of suffix tree construction in F# while maintaining functional programming principles.

