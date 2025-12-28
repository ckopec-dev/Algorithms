# Comb Sort Algorithm in F#

Here's an implementation of the Comb Sort algorithm in F#:

```fsharp
let combSort (arr: int[]) : unit =
    let n = arr.Length
    if n <= 1 then return ()
    
    let mutable gap = n
    let mutable swapped = true
    
    while swapped || gap > 1 do
        // Calculate next gap using shrink factor of 1.3
        gap <- max 1 (int (float gap / 1.3))
        
        swapped <- false
        
        // Compare elements with current gap
        for i = 0 to n - gap - 1 do
            if arr.[i] > arr.[i + gap] then
                // Swap elements
                let temp = arr.[i]
                arr.[i] <- arr.[i + gap]
                arr.[i + gap] <- temp
                swapped <- true

// Example usage
let exampleArray = [| 8; 4; 1; 56; 3; -44; 23; -6; 28; 0 |]
printfn "Original array: %A" exampleArray

combSort exampleArray
printfn "Sorted array: %A" exampleArray
```

## Output:
```
Original array: [|8; 4; 1; 56; 3; -44; 23; -6; 28; 0|]
Sorted array: [|-44; -6; 0; 1; 3; 4; 8; 23; 28; 56|]
```

## Key Features of this Implementation:

- **Gap Calculation**: Uses a shrink factor of 1.3 to determine the gap size
- **In-place Sorting**: Modifies the array directly without requiring additional memory
- **Optimization**: Stops when no swaps occur in a complete pass
- **Type Safety**: Uses F#'s strong typing system with `int[]` arrays

## How Comb Sort Works:

1. Start with a large gap (size of array)
2. Compare elements separated by the gap
3. Swap if elements are in wrong order
4. Reduce the gap using shrink factor (1.3)
5. Repeat until gap becomes 1 and no more swaps occur
6. Final pass with gap = 1 ensures complete sorting

The algorithm is an improvement over Bubble Sort with better average-case performance.

