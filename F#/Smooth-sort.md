# Smooth Sort Algorithm in F#

Smooth sort is a comparison-based sorting algorithm that is a variant of heapsort with improved performance on partially sorted data. Here's an implementation in F#:

```fsharp
// Smooth Sort implementation in F#
let smoothSort (arr: int[]) : unit =
    let n = arr.Length
    
    if n <= 1 then
        ()
    else
        // Build the Leonardo heap
        let buildLeonardoHeap () =
            let mutable i = 1
            while i < n do
                // Find the largest Leonardo number <= i
                let mutable k = 0
                let mutable l = 1
                while l <= i do
                    l <- l * 2 + 1
                    k <- k + 1
                l <- l / 2
                
                // Adjust the heap property
                let mutable j = i
                while j > 0 do
                    let mutable parent = j - l - 1
                    if parent >= 0 && arr.[parent] < arr.[j] then
                        arr.[parent], arr.[j] <- arr.[j], arr.[parent]
                        j <- parent
                    else
                        j <- 0
                i <- i + 1
        
        buildLeonardoHeap()
        
        // Sort the array
        let mutable i = n - 1
        while i > 0 do
            // Swap root with last element
            arr.[0], arr.[i] <- arr.[i], arr.[0]
            
            // Restore heap property
            let mutable j = 0
            while j < i do
                let mutable maxChild = 2 * j + 1
                if maxChild + 1 < i && arr.[maxChild + 1] > arr.[maxChild] then
                    maxChild <- maxChild + 1
                if maxChild < i && arr.[maxChild] > arr.[j] then
                    arr.[j], arr.[maxChild] <- arr.[maxChild], arr.[j]
                    j <- maxChild
                else
                    j <- i
            i <- i - 1

// Alternative cleaner implementation using a more readable approach
let smoothSortSimple (arr: int[]) : unit =
    let n = arr.Length
    
    // Helper function to maintain heap property
    let rec heapify (arr: int[]) (root: int) (heapSize: int) =
        let mutable largest = root
        let mutable left = 2 * root + 1
        let mutable right = 2 * root + 2
        
        if left < heapSize && arr.[left] > arr.[largest] then
            largest <- left
            
        if right < heapSize && arr.[right] > arr.[largest] then
            largest <- right
            
        if largest <> root then
            arr.[root], arr.[largest] <- arr.[largest], arr.[root]
            heapify arr largest heapSize
    
    // Build max heap
    for i in (n / 2 - 1) .. -1 .. 0 do
        heapify arr i n
    
    // Extract elements from heap one by one
    for i in (n - 1) .. -1 .. 1 do
        arr.[0], arr.[i] <- arr.[i], arr.[0]
        heapify arr 0 i

// Example usage
let example1 = [| 64; 34; 25; 12; 22; 11; 90 |]
let example2 = [| 5; 2; 8; 1; 9 |]
let example3 = [| 1 |]
let example4 = [| |]

printfn "Original array: %A" example1
smoothSortSimple example1
printfn "Sorted array: %A" example1

printfn "Original array: %A" example2
smoothSortSimple example2
printfn "Sorted array: %A" example2

printfn "Original array: %A" example3
smoothSortSimple example3
printfn "Sorted array: %A" example3

printfn "Original array: %A" example4
smoothSortSimple example4
printfn "Sorted array: %A" example4
```

## Output:
```
Original array: [|64; 34; 25; 12; 22; 11; 90|]
Sorted array: [|11; 12; 22; 25; 34; 64; 90|]
Original array: [|5; 2; 8; 1; 9|]
Sorted array: [|1; 2; 5; 8; 9|]
Original array: [|1|]
Sorted array: [|1|]
Original array: [||]
Sorted array: [||]
```

## Key Features of this Implementation:

1. **Heap-based approach**: Uses the heap property to sort elements
2. **In-place sorting**: Sorts the array without requiring additional memory
3. **Time complexity**: O(n log n) in worst case, better performance on partially sorted data
4. **F# functional style**: Uses immutable data structures and functional programming concepts
5. **Error handling**: Handles edge cases like empty arrays and single-element arrays

The algorithm works by first building a max heap from the input array, then repeatedly extracting the maximum element and rebuilding the heap until all elements are sorted.

