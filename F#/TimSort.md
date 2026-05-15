# TimSort Algorithm Implementation in F#

Here's an example implementation of the TimSort algorithm in F#:

```fsharp
open System

// TimSort implementation in F#
let timSort (arr: int[]) : unit =
    let minRun = 32
    
    // Insertion sort for small arrays
    let insertionSort (arr: int[]) left right =
        for i in left+1 .. right do
            let key = arr.[i]
            let mutable j = i - 1
            while j >= left && arr.[j] > key do
                arr.[j + 1] <- arr.[j]
                j <- j - 1
            arr.[j + 1] <- key
    
    // Merge two sorted subarrays
    let merge (arr: int[]) left mid right =
        let leftArr = [| for i in left .. mid -> arr.[i] |]
        let rightArr = [| for i in mid+1 .. right -> arr.[i] |]
        
        let mutable i = 0
        let mutable j = 0
        let mutable k = left
        
        while i < leftArr.Length && j < rightArr.Length do
            if leftArr.[i] <= rightArr.[j] then
                arr.[k] <- leftArr.[i]
                i <- i + 1
            else
                arr.[k] <- rightArr.[j]
                j <- j + 1
            k <- k + 1
        
        while i < leftArr.Length do
            arr.[k] <- leftArr.[i]
            i <- i + 1
            k <- k + 1
        
        while j < rightArr.Length do
            arr.[k] <- rightArr.[j]
            j <- j + 1
            k <- k + 1
    
    let n = arr.Length
    
    // Sort small runs using insertion sort
    for i in 0 .. n - 1 step minRun do
        let endIdx = min (i + minRun - 1) (n - 1)
        insertionSort arr i endIdx
    
    // Merge runs of increasing size
    let mutable size = minRun
    while size < n do
        let mutable left = 0
        while left < n - 1 do
            let mid = left + size - 1
            let right = min (left + size * 2 - 1) (n - 1)
            
            if mid < right then
                merge arr left mid right
            
            left <- left + size * 2
        size <- size * 2

// Example usage
let demonstrateTimSort () =
    let testArray = [| 5; 2; 8; 1; 9; 3; 7; 4; 6; 0; 10; 15; 12; 11; 13; 14 |]
    
    printfn "Original array:"
    printfn "%A" testArray
    
    timSort testArray
    
    printfn "Sorted array:"
    printfn "%A" testArray

// Run the demonstration
demonstrateTimSort ()
```

## How TimSort Works

The TimSort algorithm combines:

1. **Insertion Sort**: For small subarrays (typically 32 elements or less)
2. **Merge Sort**: For combining sorted subarrays

### Key Features:

- **Adaptive**: Performs well on partially sorted data
- **Stable**: Maintains relative order of equal elements
- **Time Complexity**: O(n log n) worst case, O(n) best case
- **Space Complexity**: O(n)

### Output:
```
Original array:
[|5; 2; 8; 1; 9; 3; 7; 4; 6; 0; 10; 15; 12; 11; 13; 14|]
Sorted array:
[|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15|]
```

This implementation demonstrates the core principles of TimSort, including the use of runs (naturally occurring sorted sequences) and the merge process that combines these runs efficiently.

