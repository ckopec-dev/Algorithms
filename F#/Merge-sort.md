# Merge Sort Algorithm in F#

Here's an implementation of the merge sort algorithm in F#:

```fsharp
// Merge sort implementation in F#
let rec mergeSort list =
    match list with
    | [] -> []  // Empty list is already sorted
    | [x] -> [x]  // Single element list is already sorted
    | _ ->
        // Split the list into two halves
        let mid = List.length list / 2
        let left = List.take mid list
        let right = List.skip mid list
        
        // Recursively sort both halves and merge them
        merge (mergeSort left) (mergeSort right)

// Helper function to merge two sorted lists
and merge left right =
    match left, right with
    | [], _ -> right
    | _, [] -> left
    | x :: xs, y :: ys ->
        if x <= y then
            x :: merge xs right
        else
            y :: merge left ys

// Example usage
let numbers = [64; 34; 25; 12; 22; 11; 90]
let sortedNumbers = mergeSort numbers

printfn "Original list: %A" numbers
printfn "Sorted list: %A" sortedNumbers
```

## Output:
```
Original list: [64; 34; 25; 12; 22; 11; 90]
Sorted list: [11; 12; 22; 25; 34; 64; 90]
```

## How it works:

1. **Base cases**: Empty lists and single-element lists are already sorted
2. **Divide**: Split the list into two roughly equal halves
3. **Conquer**: Recursively sort both halves
4. **Combine**: Merge the two sorted halves back together

## Key F# features used:

- **Pattern matching** for handling different list cases
- **Recursive functions** with `rec` keyword
- **List functions** like `List.take`, `List.skip`, and `List.length`
- **Tail recursion** optimization in the merge function
- **Let bindings** for clean variable declarations

The time complexity is O(n log n) and space complexity is O(n).

