```fsharp
// BogoSort implementation in F#

open System
open System.Linq

// Helper function to check if array is sorted
let isSorted (arr: int[]) : bool =
    arr.Zip(arr.Skip 1, fun x y -> x <= y)
       .All(fun x -> x)

// Helper function to shuffle array in place
let shuffle (arr: int[]) : unit =
    let rng = Random()
    for i in 0 .. arr.Length - 1 do
        let j = rng.Next(i, arr.Length)
        let temp = arr.[i]
        arr.[i] <- arr.[j]
        arr.[j] <- temp

// BogoSort algorithm
let bogoSort (arr: int[]) : int[] =
    let mutable sorted = false
    let mutable attempts = 0
    
    while not sorted do
        shuffle arr
        sorted <- isSorted arr
        attempts <- attempts + 1
        printfn "Attempt %d: %A" attempts arr
    
    printfn "Sorted in %d attempts" attempts
    arr

// Example usage
let example1 = [|5; 2; 8; 1; 9|]
printfn "Original array: %A" example1
let result1 = bogoSort (Array.copy example1)
printfn "Sorted array: %A" result1

printfn "\n---\n"

let example2 = [|3; 1; 4; 1; 5; 9; 2; 6|]
printfn "Original array: %A" example2
let result2 = bogoSort (Array.copy example2)
printfn "Sorted array: %A" result2
```

This F# implementation of BogoSort demonstrates:

1. **isSorted function**: Checks if an array is sorted in ascending order
2. **shuffle function**: Randomly shuffles the array elements
3. **bogoSort function**: Main sorting algorithm that repeatedly shuffles until sorted
4. **Example usage**: Shows the algorithm working on different arrays

**Key F# features used:**
- `Array.copy` to avoid mutating the original array
- `Zip` and `Skip` for comparing adjacent elements
- `All` for checking if all elements satisfy a condition
- `printfn` for debugging output
- `mutable` variables for tracking state

**Note**: BogoSort has an average time complexity of O((n+1)!) and should only be used for educational purposes, as it's extremely inefficient for practical use.

