# Binary Search Algorithm in F#

Here's an implementation of the binary search algorithm in F#:

```fsharp
// Recursive binary search implementation
let rec binarySearch (arr: int[]) (target: int) (left: int) (right: int) : int =
    if left > right then
        -1  // Target not found
    else
        let mid = left + (right - left) / 2
        
        match compare arr.[mid] target with
        | 0 -> mid  // Found target at mid index
        | 1 -> binarySearch arr target left (mid - 1)  // Search left half
        | _ -> binarySearch arr target (mid + 1) right  // Search right half

// Wrapper function to simplify usage
let binarySearchWrapper (arr: int[]) (target: int) : int =
    binarySearch arr target 0 (arr.Length - 1)

// Iterative binary search implementation
let binarySearchIterative (arr: int[]) (target: int) : int =
    let mutable left = 0
    let mutable right = arr.Length - 1
    let mutable result = -1
    
    while left <= right do
        let mid = left + (right - left) / 2
        
        match compare arr.[mid] target with
        | 0 -> 
            result <- mid
            right <- left - 1  // Exit loop
        | 1 -> 
            right <- mid - 1
        | _ -> 
            left <- mid + 1
    
    result

// Example usage
let numbers = [|1; 3; 5; 7; 9; 11; 13; 15; 17; 19|]

// Test the recursive implementation
let result1 = binarySearchWrapper numbers 7
printfn "Recursive search for 7: Index %d" result1  // Output: Index 3

let result2 = binarySearchWrapper numbers 1
printfn "Recursive search for 1: Index %d" result2  // Output: Index 0

let result3 = binarySearchWrapper numbers 19
printfn "Recursive search for 19: Index %d" result3  // Output: Index 9

let result4 = binarySearchWrapper numbers 6
printfn "Recursive search for 6: Index %d" result4  // Output: Index -1 (not found)

// Test the iterative implementation
let result5 = binarySearchIterative numbers 11
printfn "Iterative search for 11: Index %d" result5  // Output: Index 5
```

## Key Features:

- **Recursive Implementation**: Uses pattern matching with `compare` function
- **Iterative Implementation**: Uses mutable variables for loop control
- **Error Handling**: Returns -1 when target is not found
- **Type Safety**: Uses `int[]` arrays with proper type annotations
- **Functional Style**: Leverages F#'s pattern matching and immutable data structures

## Time Complexity:
- **Best Case**: O(1) - target found at middle
- **Average/Worst Case**: O(log n) - each step halves the search space

## Space Complexity:
- **Recursive**: O(log n) - due to call stack
- **Iterative**: O(1) - constant space usage

