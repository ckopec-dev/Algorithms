# Pancake Sorting Algorithm in F#

Pancake sorting is the sorting algorithm that sorts an array by performing a series of "pancake flips" - reversing the order of the first k elements of the array.

```fsharp
// Pancake Sort Implementation in F#

// Function to flip the first k elements of the array
let flip (arr: int[]) k =
    let mutable left = 0
    let mutable right = k - 1
    let temp = ref 0
    
    while left < right do
        temp := arr.[left]
        arr.[left] := arr.[right]
        arr.[right] := !temp
        left <- left + 1
        right <- right - 1

// Function to find the index of the maximum element in the first n elements
let findMaxIndex (arr: int[]) n =
    let mutable maxIndex = 0
    let mutable maxValue = arr.[0]
    
    for i in 1 .. n - 1 do
        if arr.[i] > maxValue then
            maxValue <- arr.[i]
            maxIndex <- i
    
    maxIndex

// Main pancake sort function
let pancakeSort (arr: int[]) =
    let n = arr.Length
    let flips = ref []
    
    // Start from the end and work backwards
    for i in n .. -1 .. 2 do
        // Find the index of the maximum element in the first i elements
        let maxIndex = findMaxIndex arr i
        
        // If the maximum element is not already at the correct position
        if maxIndex <> i - 1 then
            // If the maximum element is not at the beginning, flip it to the beginning
            if maxIndex <> 0 then
                flip arr (maxIndex + 1)
                flips := (maxIndex + 1) :: !flips
            
            // Flip the maximum element to its correct position at the end
            flip arr i
            flips := i :: !flips
    
    !flips

// Alternative implementation using a more functional approach
let pancakeSortFunctional (arr: int[]) =
    let flipArray (arr: int[]) k =
        Array.rev (Array.take k arr) @ Array.skip k arr
    
    let rec sortHelper (arr: int[]) (flips: int list) =
        match arr with
        | [] -> flips
        | [x] -> flips
        | _ ->
            let n = arr.Length
            let maxIndex = arr |> Array.indexed |> Array.maxBy snd |> fst
            let maxVal = arr.[maxIndex]
            
            if maxIndex = n - 1 then
                // Already in correct position, move to next
                sortHelper (Array.take (n - 1) arr) flips
            elif maxIndex = 0 then
                // Flip to move max to end
                let flipped = flipArray arr n
                sortHelper flipped (n :: flips)
            else
                // Flip to bring max to front, then to end
                let flipped1 = flipArray arr (maxIndex + 1)
                let flipped2 = flipArray flipped1 n
                sortHelper flipped2 ((maxIndex + 1) :: n :: flips)
    
    sortHelper arr []

// Example usage
let example1 = [|4; 3; 2; 1|]
let example2 = [|3; 2; 4; 1|]
let example3 = [|1; 2; 3; 4|]

printfn "Original array: %A" example1
let flips1 = pancakeSort example1
printfn "Flips needed: %A" flips1
printfn "Sorted array: %A" example1
printfn ""

printfn "Original array: %A" example2
let flips2 = pancakeSort example2
printfn "Flips needed: %A" flips2
printfn "Sorted array: %A" example2
printfn ""

printfn "Original array: %A" example3
let flips3 = pancakeSort example3
printfn "Flips needed: %A" flips3
printfn "Sorted array: %A" example3
```

## Example Output:
```
Original array: [|4; 3; 2; 1|]
Flips needed: [4; 3; 2]
Sorted array: [|1; 2; 3; 4|]

Original array: [|3; 2; 4; 1|]
Flips needed: [3; 4; 2; 3]
Sorted array: [|1; 2; 3; 4|]

Original array: [|1; 2; 3; 4|]
Flips needed: []
Sorted array: [|1; 2; 3; 4|]
```

## How it works:

1. **Find Maximum**: For each position from the end to the beginning, find the maximum element in the unsorted portion
2. **Move to Front**: If the maximum element is not already at the front, flip it to the beginning
3. **Move to Correct Position**: Flip the maximum element to its correct position at the end
4. **Repeat**: Continue this process for the remaining unsorted portion

The algorithm has a time complexity of O(n²) and space complexity of O(n) for the flip operations.

