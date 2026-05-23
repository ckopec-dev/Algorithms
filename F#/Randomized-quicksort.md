```fsharp
open System

// Randomized QuickSort implementation in F#
let randomizedQuickSort (arr: int[]) =
    let random = Random()
    
    let swap (arr: int[]) i j =
        let temp = arr.[i]
        arr.[i] <- arr.[j]
        arr.[j] <- temp
    
    let partition (arr: int[]) low high =
        // Pick random pivot and swap with last element
        let randomIndex = low + random.Next(high - low + 1)
        swap arr randomIndex high
        
        let pivot = arr.[high]
        let mutable i = low - 1
        
        for j in low .. high - 1 do
            if arr.[j] <= pivot then
                i <- i + 1
                swap arr i j
        
        swap arr (i + 1) high
        i + 1
    
    let rec quickSortHelper (arr: int[]) low high =
        if low < high then
            let pivotIndex = partition arr low high
            quickSortHelper arr low (pivotIndex - 1)
            quickSortHelper arr (pivotIndex + 1) high
    
    quickSortHelper arr 0 (arr.Length - 1)

// Example usage
let exampleArray = [|9; 3; 7; 1; 5; 8; 2; 6; 4; 0|]
printfn "Original array: %A" exampleArray

randomizedQuickSort exampleArray
printfn "Sorted array: %A" exampleArray

// Another example with duplicate elements
let exampleArray2 = [|5; 2; 8; 2; 9; 1; 5; 5|]
printfn "\nOriginal array with duplicates: %A" exampleArray2

randomizedQuickSort exampleArray2
printfn "Sorted array with duplicates: %A" exampleArray2
```

This F# implementation of Randomized QuickSort includes:

**Key Features:**
- **Random pivot selection**: Uses `Random.Next()` to pick a random element as pivot
- **In-place sorting**: Modifies the array directly without creating new arrays
- **Partitioning logic**: Standard Lomuto partition scheme
- **Recursive sorting**: Sorts subarrays recursively

**How it works:**
1. A random element is chosen as pivot and swapped with the last element
2. Partitioning arranges elements so smaller elements are on the left, larger on the right
3. Recursively sort the subarrays on both sides of the pivot

**Time Complexity:**
- Average case: O(n log n)
- Worst case: O(n²) - very rare due to randomization
- Space complexity: O(log n) for recursion stack

**Output example:**
```
Original array: [|9; 3; 7; 1; 5; 8; 2; 6; 4; 0|]
Sorted array: [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9|]

Original array with duplicates: [|5; 2; 8; 2; 9; 1; 5; 5|]
Sorted array with duplicates: [|1; 2; 2; 5; 5; 5; 8; 9|]
```

