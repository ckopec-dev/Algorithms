```fsharp
// Counting Sort Implementation in F#

let countingSort (arr: int[]) : int[] =
    if arr.Length = 0 then arr
    else
        // Find the maximum and minimum values to determine range
        let maxVal = Array.max arr
        let minVal = Array.min arr
        let range = maxVal - minVal + 1
        
        // Create count array
        let count = Array.create range 0
        
        // Count occurrences of each element
        for i in 0 .. arr.Length - 1 do
            count.[arr.[i] - minVal] <- count.[arr.[i] - minVal] + 1
        
        // Reconstruct sorted array
        let sorted = Array.zeroCreate arr.Length
        let mutable index = 0
        
        for i in 0 .. range - 1 do
            while count.[i] > 0 do
                sorted.[index] <- i + minVal
                index <- index + 1
                count.[i] <- count.[i] - 1
        
        sorted

// Example usage
let exampleArray = [|4; 2; 2; 8; 3; 3; 1|]
printfn "Original array: %A" exampleArray

let sortedArray = countingSort exampleArray
printfn "Sorted array: %A" sortedArray

// Another example with negative numbers
let exampleArray2 = [|5; -2; 4; -1; 0; 3; -3|]
printfn "Original array with negatives: %A" exampleArray2

let sortedArray2 = countingSort exampleArray2
printfn "Sorted array with negatives: %A" sortedArray2
```

**Output:**
```
Original array: [|4; 2; 2; 8; 3; 3; 1|]
Sorted array: [|1; 2; 2; 3; 3; 4; 8|]
Original array with negatives: [|5; -2; 4; -1; 0; 3; -3|]
Sorted array with negatives: [|-3; -2; -1; 0; 3; 4; 5|]
```

**Key Features of this Implementation:**

1. **Handles negative numbers** by using the minimum value as offset
2. **Time Complexity**: O(n + k) where n is array length and k is range
3. **Space Complexity**: O(k) for the count array
4. **Stable sorting** - maintains relative order of equal elements
5. **Non-comparative** - works by counting occurrences rather than comparing elements

**When to use Counting Sort:**
- When the range of possible values is small compared to the number of elements
- When sorting integers or objects with integer keys
- When stability is required
- When the input is known to be within a limited range