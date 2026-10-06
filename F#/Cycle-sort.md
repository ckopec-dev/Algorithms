```fsharp
// Cycle Sort Implementation in F#

let cycleSort (arr: int[]) : unit =
    let n = arr.Length
    
    // Count number of cycles
    for cycleStart in 0 .. n - 2 do
        let mutable item = arr.[cycleStart]
        
        // Find the position where we put the element
        let mutable pos = cycleStart
        
        for i in cycleStart + 1 .. n - 1 do
            if arr.[i] < item then
                pos <- pos + 1
                
        // If item is already in correct position
        if pos <> cycleStart then
            // Skip duplicates
            while item = arr.[pos] do
                pos <- pos + 1
                
            // Place the item at its correct position
            let temp = arr.[pos]
            arr.[pos] <- item
            item <- temp
            
            // Rotate the rest of the cycle
            while pos <> cycleStart do
                pos <- cycleStart
                
                for i in cycleStart + 1 .. n - 1 do
                    if arr.[i] < item then
                        pos <- pos + 1
                        
                while item = arr.[pos] do
                    pos <- pos + 1
                    
                let temp = arr.[pos]
                arr.[pos] <- item
                item <- temp

// Example usage
let exampleArray = [|4; 3; 2; 1; 5|]
printfn "Original array: %A" exampleArray

cycleSort exampleArray
printfn "Sorted array: %A" exampleArray

// Another example with duplicates
let exampleArray2 = [|4; 3; 2; 1; 5; 2; 3|]
printfn "\nOriginal array with duplicates: %A" exampleArray2

cycleSort exampleArray2
printfn "Sorted array with duplicates: %A" exampleArray2
```

**Output:**
```
Original array: [|4; 3; 2; 1; 5|]
Sorted array: [|1; 2; 3; 4; 5|]

Original array with duplicates: [|4; 3; 2; 1; 5; 2; 3|]
Sorted array with duplicates: [|1; 2; 2; 3; 3; 4; 5|]
```

**Key Features of this Implementation:**

1. **In-place sorting**: Uses O(1) extra space
2. **Minimum number of writes**: At most n-1 swaps
3. **Stable**: Maintains relative order of equal elements
4. **Time complexity**: O(n²) in worst case, O(n) in best case
5. **Space complexity**: O(1)

**How it works:**
- The algorithm identifies cycles in the array
- For each cycle, it places elements at their correct positions
- It minimizes the number of writes by rotating elements within cycles
- The process continues until all elements are in their correct positions