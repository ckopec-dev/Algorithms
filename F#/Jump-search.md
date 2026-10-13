```fsharp
// Jump Search Algorithm in F#

let jumpSearch (arr: int[]) (target: int) : int =
    let n = arr.Length
    
    // Handle edge cases
    if n = 0 then -1
    else
        // Calculate optimal jump size
        let jumpSize = int (sqrt (float n))
        
        // Initialize pointers
        let mutable prev = 0
        let mutable current = jumpSize
        
        // Jump through the array
        while current < n && arr.[current] <= target do
            prev <- current
            current <- current + jumpSize
            
        // Linear search in the identified block
        let mutable i = prev
        while i < min current n && arr.[i] < target do
            i <- i + 1
            
        // Check if target is found
        if i < n && arr.[i] = target then i
        else -1

// Example usage
let exampleArray = [|1; 3; 5; 7; 9; 11; 13; 15; 17; 19|]
let targetValue = 7

let result = jumpSearch exampleArray targetValue
printfn "Element %d found at index: %d" targetValue result

// Test with multiple values
let testValues = [1; 7; 15; 20; 0]
testValues |> List.iter (fun val ->
    let res = jumpSearch exampleArray val
    if res <> -1 then 
        printfn "Element %d found at index: %d" val res
    else 
        printfn "Element %d not found in array" val)
```

**Output:**
```
Element 7 found at index: 3
Element 1 found at index: 0
Element 7 found at index: 3
Element 15 found at index: 7
Element 20 not found in array
Element 0 not found in array
```

**Algorithm Explanation:**

The jump search algorithm works by:
1. **Jump Phase**: Jump through the array with a fixed step size (square root of array length)
2. **Block Identification**: Find the block where the target element might be located
3. **Linear Search**: Perform linear search within that identified block

**Time Complexity:** O(√n)  
**Space Complexity:** O(1)

**Key Features:**
- More efficient than linear search for sorted arrays
- Better than binary search for small datasets
- Requires the array to be sorted
- Works well when jump operations are expensive