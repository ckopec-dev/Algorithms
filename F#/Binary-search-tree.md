# Binary Search Tree in F#

Here's an implementation of a Binary Search Tree algorithm in F#:

```fsharp
// Define the BST node type
type BST<'T when 'T : comparison> = 
    | Empty
    | Node of 'T * BST<'T> * BST<'T>

// Create a new empty tree
let empty = Empty

// Insert a value into the BST
let rec insert value tree =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(nodeValue, left, right) ->
        if value < nodeValue then
            Node(nodeValue, insert value left, right)
        elif value > nodeValue then
            Node(nodeValue, left, insert value right)
        else
            tree // Value already exists, don't insert duplicates

// Search for a value in the BST
let rec search value tree =
    match tree with
    | Empty -> false
    | Node(nodeValue, left, right) ->
        if value < nodeValue then
            search value left
        elif value > nodeValue then
            search value right
        else
            true

// Find the minimum value in the BST
let rec findMin tree =
    match tree with
    | Empty -> failwith "Empty tree"
    | Node(nodeValue, Empty, _) -> nodeValue
    | Node(_, left, _) -> findMin left

// Find the maximum value in the BST
let rec findMax tree =
    match tree with
    | Empty -> failwith "Empty tree"
    | Node(nodeValue, _, Empty) -> nodeValue
    | Node(_, _, right) -> findMax right

// Delete a value from the BST
let rec delete value tree =
    match tree with
    | Empty -> Empty
    | Node(nodeValue, left, right) ->
        if value < nodeValue then
            Node(nodeValue, delete value left, right)
        elif value > nodeValue then
            Node(nodeValue, left, delete value right)
        else
            // Node to delete found
            match left, right with
            | Empty, _ -> right
            | _, Empty -> left
            | _, _ -> 
                // Node has two children
                let minRight = findMin right
                Node(minRight, left, delete minRight right)

// In-order traversal (returns values in sorted order)
let rec inorder tree =
    seq {
        match tree with
        | Empty -> ()
        | Node(nodeValue, left, right) ->
            yield! inorder left
            yield nodeValue
            yield! inorder right
    }

// Example usage
let example() =
    let tree = 
        empty
        |> insert 50
        |> insert 30
        |> insert 70
        |> insert 20
        |> insert 40
        |> insert 60
        |> insert 80
    
    printfn "Tree contents (in-order): %A" (List.ofSeq (inorder tree))
    printfn "Search 40: %b" (search 40 tree)
    printfn "Search 25: %b" (search 25 tree)
    printfn "Minimum value: %d" (findMin tree)
    printfn "Maximum value: %d" (findMax tree)
    
    let treeAfterDelete = delete 30 tree
    printfn "After deleting 30: %A" (List.ofSeq (inorder treeAfterDelete))

// Run the example
example()
```

## Key Features of this Implementation:

1. **Generic Type**: Uses `'T when 'T : comparison` to support any comparable type
2. **Immutable Structure**: All operations return new trees rather than modifying existing ones
3. **Complete Operations**:
   - Insert values
   - Search for values
   - Find minimum and maximum values
   - Delete values
   - In-order traversal
4. **Proper BST Properties**: Maintains the binary search tree property where left subtree contains smaller values and right subtree contains larger values

## Sample Output:
```
Tree contents (in-order): [20; 30; 40; 50; 60; 70; 80]
Search 40: true
Search 25: false
Minimum value: 20
Maximum value: 80
After deleting 30: [20; 40; 50; 60; 70; 80]
```

This implementation demonstrates F#'s functional programming strengths with immutable data structures and pattern matching.

