# Quick Sort Algorithm in F#

Here's an implementation of the Quick Sort algorithm in F#:

```fsharp
let rec quickSort list =
    match list with
    | [] -> []
    | pivot :: rest ->
        let smaller = [x for x in rest when x <= pivot]
        let larger = [x for x in rest when x > pivot]
        (quickSort smaller) @ [pivot] @ (quickSort larger)

// Alternative implementation using partition
let rec quickSort2 list =
    match list with
    | [] -> []
    | pivot :: rest ->
        let smaller, larger = List.partition (fun x -> x <= pivot) rest
        (quickSort2 smaller) @ [pivot] @ (quickSort2 larger)

// Example usage
let numbers = [3; 6; 8; 10; 1; 2; 1]
let sortedNumbers = quickSort numbers
printfn "Original: %A" numbers
printfn "Sorted: %A" sortedNumbers

// Output:
// Original: [3; 6; 8; 10; 1; 2; 1]
// Sorted: [1; 1; 2; 3; 6; 8; 10]
```

## How it works:

1. **Base case**: If the list is empty, return an empty list
2. **Partitioning**: Choose the first element as the pivot
3. **Separation**: Split the remaining elements into two lists:
   - Elements less than or equal to the pivot
   - Elements greater than the pivot
4. **Recursion**: Recursively sort both sublists
5. **Combination**: Concatenate the sorted smaller elements, pivot, and sorted larger elements

## Key F# Features Used:

- **Pattern matching** with `match ... with`
- **List comprehension** with `[x for x in rest when x <= pivot]`
- **List partitioning** with `List.partition`
- **Recursive functions** with `rec`
- **List concatenation** with `@` operator

The algorithm has an average time complexity of O(n log n) and space complexity of O(log n).

