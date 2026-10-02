```fsharp
open System
open System.Linq

// BogoSort implementation in F#
let bogoSort (arr: int[]) : int[] =
    let isSorted (arr: int[]) : bool =
        arr |> Seq.pairwise
            |> Seq.forall (fun (a, b) -> a <= b)
    
    let shuffle (arr: int[]) : unit =
        let rng = Random()
        for i in 0 .. arr.Length - 1 do
            let j = rng.Next(i, arr.Length)
            let temp = arr.[i]
            arr.[i] <- arr.[j]
            arr.[j] <- temp
    
    let mutable sorted = false
    let mutable attempts = 0
    
    while not sorted do
        shuffle arr
        sorted <- isSorted arr
        attempts <- attempts + 1
        printfn "Attempt %d: %A" attempts arr
    
    arr

// Example usage
let exampleArray = [|5; 2; 8; 1; 9|]
printfn "Original array: %A" exampleArray

let sortedArray = bogoSort exampleArray
printfn "Sorted array: %A" sortedArray
```

This F# implementation of BogoSort works as follows:

1. **isSorted function**: Checks if the array is sorted by comparing each element with its successor
2. **shuffle function**: Randomly shuffles the array elements using Fisher-Yates shuffle algorithm
3. **Main bogoSort logic**: Continuously shuffles and checks until the array is sorted
4. **Output**: Shows each attempt as the algorithm tries different permutations

**Note**: BogoSort has an average time complexity of O(n!) and should only be used for educational purposes, as it's extremely inefficient for any practical sorting needs.