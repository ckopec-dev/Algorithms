# AVL Tree Balancing Algorithm in F#

Here's a complete implementation of an AVL tree with balancing operations in F#:

```fsharp
// AVL Tree Node Definition
type AVLNode<'T when 'T : comparison> = {
    Value: 'T
    Left: AVLNode<'T> option
    Right: AVLNode<'T> option
    Height: int
}

// AVL Tree Type
type AVLTree<'T when 'T : comparison> = {
    Root: AVLNode<'T> option
}

// Helper functions
let height node = 
    match node with
    | None -> 0
    | Some n -> n.Height

let balanceFactor node =
    match node with
    | None -> 0
    | Some n -> height n.Left - height n.Right

let max x y = if x > y then x else y

let createNode value left right =
    let newHeight = 1 + max (height left) (height right)
    { Value = value; Left = left; Right = right; Height = newHeight }

// Right rotation
let rotateRight y =
    match y with
    | None -> None
    | Some node ->
        match node.Left with
        | None -> y
        | Some x ->
            let newRight = Some { node with Left = x.Right }
            let newX = { x with Right = newRight; Height = max (height x.Left) (height x.Right) + 1 }
            Some { newX with Height = max (height newX.Left) (height newX.Right) + 1 }

// Left rotation
let rotateLeft x =
    match x with
    | None -> None
    | Some node ->
        match node.Right with
        | None -> x
        | Some y ->
            let newLeft = Some { node with Right = y.Left }
            let newY = { y with Left = newLeft; Height = max (height y.Left) (height y.Right) + 1 }
            Some { newY with Height = max (height newY.Left) (height newY.Right) + 1 }

// Balance the tree
let balance node =
    match node with
    | None -> None
    | Some n ->
        let newHeight = max (height n.Left) (height n.Right) + 1
        let balancedNode = { n with Height = newHeight }
        
        let bf = balanceFactor (Some balancedNode)
        
        // Left Left Case
        if bf > 1 && balanceFactor n.Left > 0 then
            rotateRight (Some balancedNode)
        
        // Left Right Case
        elif bf > 1 && balanceFactor n.Left < 0 then
            let newLeft = rotateLeft n.Left
            let newRoot = { balancedNode with Left = newLeft }
            rotateRight (Some newRoot)
        
        // Right Right Case
        elif bf < -1 && balanceFactor n.Right < 0 then
            rotateLeft (Some balancedNode)
        
        // Right Left Case
        elif bf < -1 && balanceFactor n.Right > 0 then
            let newRight = rotateRight n.Right
            let newRoot = { balancedNode with Right = newRight }
            rotateLeft (Some newRoot)
        
        else
            Some balancedNode

// Insert function
let rec insert tree value =
    let rec insertHelper node =
        match node with
        | None -> Some { Value = value; Left = None; Right = None; Height = 1 }
        | Some n ->
            if value < n.Value then
                let newLeft = insertHelper n.Left
                let newRoot = { n with Left = newLeft }
                balance (Some newRoot)
            elif value > n.Value then
                let newRight = insertHelper n.Right
                let newRoot = { n with Right = newRight }
                balance (Some newRoot)
            else
                // Value already exists, return as is
                Some n
    
    { tree with Root = insertHelper tree.Root }

// Search function
let rec search tree value =
    match tree.Root with
    | None -> false
    | Some root ->
        let rec searchHelper node =
            match node with
            | None -> false
            | Some n ->
                if value = n.Value then
                    true
                elif value < n.Value then
                    searchHelper n.Left
                else
                    searchHelper n.Right
        
        searchHelper (Some root)

// In-order traversal to display tree
let rec inorderTraversal node =
    match node with
    | None -> []
    | Some n ->
        (inorderTraversal n.Left) @ [n.Value] @ (inorderTraversal n.Right)

// Example usage
let example() =
    let tree = { Root = None }
    
    // Insert values in sequence that would cause imbalance
    let tree1 = insert tree 10
    let tree2 = insert tree1 20
    let tree3 = insert tree2 30
    let tree4 = insert tree3 40
    let tree5 = insert tree4 50
    let tree6 = insert tree5 25
    
    printfn "In-order traversal: %A" (inorderTraversal tree6.Root)
    
    // Check if values exist
    printfn "Search 25: %b" (search tree6 25)
    printfn "Search 35: %b" (search tree6 35)
    
    // Display tree structure
    match tree6.Root with
    | None -> printfn "Empty tree"
    | Some root -> 
        printfn "Root value: %A" root.Value
        printfn "Root height: %d" root.Height

// Run the example
example()
```

## Key Components Explained:

### 1. **Node Structure**
- Each node contains: value, left child, right child, and height
- Height is used to determine balance factor

### 2. **Balance Operations**
- **Right Rotation**: Used when Left-Left case occurs
- **Left Rotation**: Used when Right-Right case occurs
- **Double Rotations**: For Left-Right and Right-Left cases

### 3. **Balance Factor Calculation**
- Balance factor = height(Left) - height(Right)
- If |balance factor| > 1, tree is unbalanced

### 4. **Insertion Process**
1. Standard BST insertion
2. Update heights
3. Check balance factor
4. Perform rotations if necessary

### 5. **Time Complexity**
- Insertion: O(log n)
- Search: O(log n)
- Space: O(n)

This implementation maintains the AVL property that the height difference between left and right subtrees is at most 1 for all nodes.

