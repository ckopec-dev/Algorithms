# Heap Sort Algorithm in F#

Here's an implementation of the Heap Sort algorithm in F#:

```fsharp
// Heap Sort implementation in F#
let heapSort (arr: int[]) : unit =
    let n = arr.Length
    
    // Function to heapify a subtree rooted at index i
    let rec heapify (arr: int[]) (n: int) (i: int) =
        let mutable largest = i
        let left = 2 * i + 1
        let right = 2 * i + 2
        
        // If left child exists and is greater than root
        if left < n && arr.[left] > arr.[largest] then
            largest <- left
            
        // If right child exists and is greater than largest so far
        if right < n && arr.[right] > arr.[largest] then
            largest <- right
            
        // If largest is not root
        if largest <> i then
            // Swap root with largest
            let temp = arr.[i]
            arr.[i] <- arr.[largest]
            arr.[largest] <- temp
            
            // Recursively heapify the affected sub-tree
            heapify arr n largest
    
    // Build a max heap
    for i in (n / 2 - 1) downto 0 do
        heapify arr n i
    
    // Extract elements from heap one by one
    for i in (n - 1) downto 1 do
        // Move current root to end
        let temp = arr.[0]
        arr.[0] <- arr.[i]
        arr.[i] <- temp
        
        // Call heapify on the reduced heap
        heapify arr i 0

// Example usage
let example = [|64; 34; 25; 12; 22; 11; 90|]
printfn "Original array: %A" example

heapSort example
printfn "Sorted array: %A" example
```

## Output:
```
Original array: [|64; 34; 25; 12; 22; 11; 90|]
Sorted array: [|11; 12; 22; 25; 34; 64; 90|]
```

## How it works:

1. **Build Max Heap**: Convert the input array into a max heap structure
2. **Extract Elements**: Repeatedly extract the maximum element (root) and place it at the end
3. **Heapify**: After each extraction, restore the heap property for the remaining elements

## Key Features:

- **Time Complexity**: O(n log n)
- **Space Complexity**: O(1) - sorts in-place
- **Stable**: Not stable (doesn't preserve relative order of equal elements)
- **In-place**: Requires only O(1) additional memory

The algorithm uses a binary heap data structure where each parent node is greater than or equal to its children (max heap property).

