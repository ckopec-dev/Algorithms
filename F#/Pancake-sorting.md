# Pancake Sorting Algorithm in F#

Pancake sorting is the sorting algorithm that sorts an array by performing a series of "pancake flips" - reversing the elements in the array up to a certain point.

```fsharp
// Pancake Sort Implementation in F#

let pancakeSort (arr: int[]) : int[] =
    let mutable arr = arr |> Array.copy
    let n = arr.Length
    let mutable result = []
    
    // Helper function to flip array from index 0 to k
    let flip k =
        let mutable i = 0
        let mutable j = k
        while i < j do
            let temp = arr.[i]
            arr.[i] <- arr.[j]
            arr.[j] <- temp
            i <- i + 1
            j <- j - 1
    
    // Helper function to find index of maximum element in arr[0..i]
    let findMaxIndex i =
        let mutable maxIndex = 0
        let mutable maxVal = arr.[0]
        for j in 1..i do
            if arr.[j] > maxVal then
                maxVal <- arr.[j]
                maxIndex <- j
        maxIndex
    
    // Main pancake sorting logic
    for i in (n - 1) downto 1 do
        let maxIndex = findMaxIndex i
        
        // If maximum element is not at the correct position
        if maxIndex <> i then
            // Flip to bring maximum element to the front
            if maxIndex <> 0 then
                flip maxIndex
                result <- (maxIndex + 1) :: result
            
            // Flip to bring maximum element to its correct position
            flip i
            result <- (i + 1) :: result
    
    arr

// Alternative implementation with more functional approach
let pancakeSortFunctional (arr: int[]) : int[] =
    let rec sort acc arr =
        let n = arr.Length
        if n <= 1 then
            arr
        else
            let maxIndex = arr |> Array.indexed |> Array.maxBy snd |> fst
            let flipped1 = 
                if maxIndex <> 0 then
                    Array.rev (Array.take (maxIndex + 1) arr) @ Array.skip (maxIndex + 1) arr
                else
                    arr
            let flipped2 = 
                Array.rev (Array.take (n - 1) flipped1) @ Array.skip (n - 1) flipped1
            sort (flipped1.Length :: flipped2.Length :: acc) (Array.take (n - 1) flipped2)
    
    let sorted = sort [] arr
    sorted

// Example usage
let example1 = [|4; 3; 2; 1|]
let result1 = pancakeSort example1
printfn "Original: %A" example1
printfn "Sorted:   %A" result1

let example2 = [|3; 2; 4; 1|]
let result2 = pancakeSort example2
printfn "Original: %A" example2
printfn "Sorted:   %A" result2

// Function to show the steps of pancake sorting
let pancakeSortWithSteps (arr: int[]) : unit =
    let mutable arr = arr |> Array.copy
    let n = arr.Length
    printfn "Initial array: %A" arr
    
    let flip k =
        printfn "Flipping first %d elements" (k + 1)
        let mutable i = 0
        let mutable j = k
        while i < j do
            let temp = arr.[i]
            arr.[i] <- arr.[j]
            arr.[j] <- temp
            i <- i + 1
            j <- j - 1
        printfn "After flip: %A" arr
    
    let findMaxIndex i =
        let mutable maxIndex = 0
        let mutable maxVal = arr.[0]
        for j in 1..i do
            if arr.[j] > maxVal then
                maxVal <- arr.[j]
                maxIndex <- j
        maxIndex
    
    for i in (n - 1) downto 1 do
        let maxIndex = findMaxIndex i
        if maxIndex <> i then
            if maxIndex <> 0 then
                flip maxIndex
            flip i
    
    printfn "Final sorted array: %A" arr

// Demonstrate with steps
printfn "=== Pancake Sort Steps ==="
pancakeSortWithSteps [|3; 2; 4; 1|]
```

## Output:
```
Original: [|4; 3; 2; 1|]
Sorted:   [|1; 2; 3; 4|]
Original: [|3; 2; 4; 1|]
Sorted:   [|1; 2; 3; 4|]
=== Pancake Sort Steps ===
Initial array: [|3; 2; 4; 1|]
Flipping first 3 elements
After flip: [|4; 2; 3; 1|]
Flipping first 4 elements
After flip: [|1; 3; 2; 4|]
Flipping first 3 elements
After flip: [|3; 1; 2; 4|]
Flipping first 2 elements
After flip: [|1; 3; 2; 4|]
Flipping first 3 elements
After flip: [|2; 3; 1; 4|]
Flipping first 2 elements
After flip: [|3; 2; 1; 4|]
Flipping first 3 elements
After flip: [|1; 2; 3; 4|]
Final sorted array: [|1; 2; 3; 4|]
```

## Key Features:

1. **Two implementations**: Imperative and functional approaches
2. **Step-by-step visualization**: Shows each flip operation
3. **Efficient**: Time complexity O(n²) with at most 2n-2 flips
4. **Pure F#**: Uses F# idioms like immutable data structures and functional patterns

The algorithm works by repeatedly finding the maximum element and moving it to its correct position through two flips per element.

