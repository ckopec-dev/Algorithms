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
        for i in 0 .. n - gap - 1 do
            if arr.[i] > arr.[i + gap] then
                // Swap elements
                let temp = arr.[i]
                arr.[i] <- arr.[i + gap]
                arr.[i + gap] <- temp
                swapped <- true

// Example usage
let exampleArray = [| 64; 34; 25; 12; 22; 11; 90; 88; 76; 50; 42 |]
printfn "Original array: %A" exampleArray

combSort exampleArray
printfn "Sorted array: %A" exampleArray
```

## How it works:

1. **Initial Gap**: Start with a gap size equal to the array length
2. **Shrink Factor**: Reduce the gap by a factor of 1.3 in each iteration
3. **Comparison**: Compare elements separated by the current gap
4. **Swap**: If elements are out of order, swap them
5. **Continue**: Repeat until no more swaps are needed and gap becomes 1

## Output:
```
Original array: [64; 34; 25; 12; 22; 11; 90; 88; 76; 50; 42]
Sorted array: [11; 12; 22; 25; 34; 42; 50; 64; 76; 88; 90]
```

## Key Features:

- **In-place sorting**: Uses O(1) extra space
- **Time Complexity**: O(n²) worst case, O(n log n) average case
- **Adaptive**: Performs better than bubble sort due to the gap reduction
- **Simple implementation**: Easy to understand and modify

The algorithm is particularly useful for small datasets or when the data is partially sorted, as it can be more efficient than bubble sort due to its larger initial gaps.

