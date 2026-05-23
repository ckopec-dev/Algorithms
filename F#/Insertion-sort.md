```fsharp
let insertionSort (arr: int[]) : unit =
    let n = arr.Length
    if n <= 1 then return ()
    
    for i in 1 .. n - 1 do
        let key = arr.[i]
        let mutable j = i - 1
        
        // Move elements greater than key one position ahead
        while j >= 0 && arr.[j] > key do
            arr.[j + 1] <- arr.[j]
            j <- j - 1
            
        // Insert key at correct position
        arr.[j + 1] <- key

// Example usage
let numbers = [|64; 34; 25; 12; 22; 11; 90|]
printfn "Original array: %A" numbers

insertionSort numbers
printfn "Sorted array: %A" numbers
```

**Output:**
```
Original array: [|64; 34; 25; 12; 22; 11; 90|]
Sorted array: [|11; 12; 22; 25; 34; 64; 90|]
```

**How it works:**
1. Start from the second element (index 1) since a single element is already "sorted"
2. Take the current element as `key`
3. Compare `key` with elements to its left
4. Shift larger elements one position to the right
5. Insert `key` at its correct position
6. Repeat until all elements are processed

**Time Complexity:** O(n²)  
**Space Complexity:** O(1)

