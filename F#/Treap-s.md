# Treap Implementation in F#

A treap is a binary search tree that also maintains heap property, combining the benefits of both data structures. Here's an implementation in F#:

```fsharp
open System

// Define the treap node type
type TreapNode<'T> when 'T : comparison = {
    Value: 'T
    Priority: int
    Left: TreapNode<'T> option
    Right: TreapNode<'T> option
}

// Type alias for the treap itself
type Treap<'T> when 'T : comparison = {
    Root: TreapNode<'T> option
}

// Helper function to create a new node
let createNode value priority left right =
    { Value = value; Priority = priority; Left = left; Right = right }

// Helper function to perform rotations
let rotateRight (node: TreapNode<'T>) =
    match node.Left with
    | Some leftChild ->
        let newRoot = createNode 
            leftChild.Value 
            leftChild.Priority 
            leftChild.Left 
            (Some { node with Value = leftChild.Value; Priority = leftChild.Priority; Left = None })
        newRoot
    | None -> node

let rotateLeft (node: TreapNode<'T>) =
    match node.Right with
    | Some rightChild ->
        let newRoot = createNode 
            rightChild.Value 
            rightChild.Priority 
            (Some { node with Value = rightChild.Value; Priority = rightChild.Priority; Right = None })
            rightChild.Right
        newRoot
    | None -> node

// Insert operation maintaining both BST and heap properties
let rec insert treap value =
    let rec insertHelper node =
        match node with
        | None -> Some (createNode value (Random().Next(1000)) None None)
        | Some n ->
            if Comparer<'T>.Default.Compare value n.Value < 0 then
                // Insert to left subtree
                match n.Left with
                | None -> 
                    let newNode = createNode value (Random().Next(1000)) None None
                    Some { n with Left = Some newNode }
                | Some left ->
                    let newLeft = insertHelper (Some left)
                    let updatedNode = { n with Left = newLeft }
                    // Check heap property and rotate if needed
                    match updatedNode.Left with
                    | Some leftChild when leftChild.Priority > updatedNode.Priority ->
                        Some (rotateRight updatedNode)
                    | _ -> Some updatedNode
            else
                // Insert to right subtree
                match n.Right with
                | None -> 
                    let newNode = createNode value (Random().Next(1000)) None None
                    Some { n with Right = Some newNode }
                | Some right ->
                    let newRight = insertHelper (Some right)
                    let updatedNode = { n with Right = newRight }
                    // Check heap property and rotate if needed
                    match updatedNode.Right with
                    | Some rightChild when rightChild.Priority > updatedNode.Priority ->
                        Some (rotateLeft updatedNode)
                    | _ -> Some updatedNode
    
    { treap with Root = insertHelper treap.Root }

// Search operation in the treap
let rec search treap value =
    let rec searchHelper node =
        match node with
        | None -> false
        | Some n ->
            match Comparer<'T>.Default.Compare value n.Value with
            | 0 -> true
            | x when x < 0 -> searchHelper n.Left
            | _ -> searchHelper n.Right
    
    searchHelper treap.Root

// In-order traversal to print elements
let rec inorderTraversal node =
    match node with
    | None -> []
    | Some n ->
        (inorderTraversal n.Left) @ [n.Value] @ (inorderTraversal n.Right)

let printTreap treap =
    match treap.Root with
    | None -> printfn "Empty treap"
    | Some root -> 
        let elements = inorderTraversal (Some root)
        printfn "Treap elements: %A" elements

// Example usage
[<EntryPoint>]
let main argv =
    // Create an empty treap
    let emptyTreap: Treap<int> = { Root = None }
    
    // Insert elements
    let treap1 = insert emptyTreap 10
    let treap2 = insert treap1 5
    let treap3 = insert treap2 15
    let treap4 = insert treap3 3
    let treap5 = insert treap4 7
    
    printfn "Initial treap:"
    printTreap treap5
    
    // Search for elements
    printfn "Search 7: %b" (search treap5 7)
    printfn "Search 12: %b" (search treap5 12)
    
    // Insert more elements
    let treap6 = insert treap5 12
    let treap7 = insert treap6 18
    
    printfn "After inserting 12 and 18:"
    printTreap treap7
    
    0
```

## Key Features of this Implementation:

1. **Node Structure**: Each node contains value, priority (random number), and left/right children
2. **Insert Operation**: Maintains both BST property (left < parent < right) and heap property (parent priority > children priorities)
3. **Rotation Operations**: Right rotation for left-heavy nodes and left rotation for right-heavy nodes
4. **Search Operation**: Standard BST search with O(log n) average time complexity
5. **In-order Traversal**: For displaying sorted elements

## Time Complexities:
- **Insert**: O(log n) average, O(n) worst case
- **Search**: O(log n) average, O(n) worst case  
- **Space**: O(n) for storing n nodes

This implementation demonstrates how treaps combine the benefits of binary search trees and heaps to provide efficient dynamic set operations.