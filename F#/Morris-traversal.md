# Morris Traversal in F#

Morris traversal is an algorithm for traversing binary trees without using recursion or a stack. It uses the tree's structure itself to create temporary links for traversal.

```fsharp
// Define the binary tree node type
type TreeNode = 
    { 
        val: int
        left: TreeNode option
        right: TreeNode option 
    }

// Morris Inorder Traversal Implementation
let morrisTraversal (root: TreeNode option) : int list =
    let mutable current = root
    let mutable result = []
    
    while current.IsSome do
        match current with
        | Some node ->
            match node.left with
            | Some leftNode ->
                // Find the inorder predecessor
                let mutable predecessor = leftNode
                while 
                    match predecessor.right with
                    | Some right -> right <> node
                    | None -> false
                do
                    match predecessor.right with
                    | Some right -> predecessor <- right
                    | None -> ()
                
                // Make current as right child of predecessor
                match predecessor.right with
                | Some _ -> 
                    // Revert the changes
                    predecessor.right <- None
                    result <- node.val :: result
                    current <- node.right
                | None -> 
                    // Make current as right child of predecessor
                    predecessor.right <- Some node
                    current <- node.left
            | None ->
                // Visit current node
                result <- node.val :: result
                current <- node.right
        | None -> current <- None
    
    // Reverse the result since we built it backwards
    List.rev result

// Alternative implementation with clearer structure
let morrisInorderTraversal (root: TreeNode option) : int list =
    let rec traverse current result =
        match current with
        | None -> result
        | Some node ->
            match node.left with
            | Some left ->
                // Find inorder predecessor
                let mutable predecessor = left
                while 
                    match predecessor.right with
                    | Some right -> right <> node
                    | None -> false
                do
                    match predecessor.right with
                    | Some right -> predecessor <- right
                    | None -> ()
                
                match predecessor.right with
                | Some _ ->
                    // Revert the temporary link
                    predecessor.right <- None
                    // Visit current node
                    traverse node.right (node.val :: result)
                | None ->
                    // Create temporary link
                    predecessor.right <- Some node
                    // Move to left subtree
                    traverse node.left result
            | None ->
                // Visit current node
                traverse node.right (node.val :: result)
    
    traverse root [] |> List.rev

// Example usage
let createSampleTree() : TreeNode option =
    // Create tree:
    //       1
    //      / \
    //     2   3
    //    / \
    //   4   5
    
    let node4 = { val = 4; left = None; right = None }
    let node5 = { val = 5; left = None; right = None }
    let node2 = { val = 2; left = Some node4; right = Some node5 }
    let node3 = { val = 3; left = None; right = None }
    let node1 = { val = 1; left = Some node2; right = Some node3 }
    
    Some node1

// Test the implementation
let testMorrisTraversal() =
    let tree = createSampleTree()
    let result = morrisInorderTraversal tree
    printfn "Morris Traversal Result: %A" result
    // Expected output: [4; 2; 5; 1; 3]

// Run the test
testMorrisTraversal()
```

## Key Features of Morris Traversal:

1. **Space Complexity**: O(1) - No extra space for stack or recursion
2. **Time Complexity**: O(n) - Each edge is visited at most 3 times
3. **Inorder Traversal**: Visits nodes in left-root-right order
4. **Threaded Tree**: Temporarily modifies tree structure to create links

## How it works:

1. Start with the root node
2. For each node, find its inorder predecessor (rightmost node in left subtree)
3. If predecessor's right is null, create a temporary link and move to left
4. If predecessor's right points to current node, remove the link and visit current node
5. Continue until all nodes are visited

This algorithm is particularly useful when memory is constrained and you cannot use recursion or explicit stack structures.

