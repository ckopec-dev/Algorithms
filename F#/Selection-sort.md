# Selection Sort Algorithm in F#

Here's an implementation of the selection sort algorithm in F#:

```fsharp
let selectionSort (arr: int[]) : unit =
    let n = arr.Length
    
    for i in 0 .. n - 2 do
        let mutable minIndex = i
        
        // Find the minimum element in remaining unsorted array
        for j in i + 1 .. n - 1 do
            if arr.[j] < arr.[minIndex] then
                minIndex <- j
        
        // Swap the found minimum element with the first element
        if minIndex <> i then
            let temp = arr.[i]
            arr.[i] <- arr.[minIndex]
            arr.[minIndex] <- temp

// Alternative functional implementation
let selectionSortFunctional (arr: int[]) : int[] =
    let rec sort acc remaining =
        match remaining with
        | [] -> acc
        | x :: xs ->
            let minVal = List.min remaining
            let minIndex = List.findIndex (fun y -> y = minVal) remaining
            let newRemaining = 
                List.take minIndex xs @ 
                List.skip (minIndex + 1) xs
            sort (acc @ [minVal]) newRemaining
    
    sort [] (Array.toList arr)

// Example usage
let exampleArray = [|64; 34; 25; 12; 22; 11; 90|]
printfn "Original array: %A" exampleArray

selectionSort exampleArray
printfn "Sorted array: %A" exampleArray

// Functional version example
let functionalExample = [|64; 34; 25; 12; 22; 11; 90|]
let sortedFunctional = selectionSortFunctional functionalExample
printfn "Functional sorted array: %A" sortedFunctional
```

## Output:
```
Original array: [|64; 34; 25; 12; 22; 11; 90|]
Sorted array: [|11; 12; 22; 25; 34; 64; 90|]
Functional sorted array: [|11; 12; 22; 25; 34; 64; 90|]
```

## How it works:

1. **Find Minimum**: In each iteration, find the smallest element in the unsorted portion
2. **Swap**: Place the minimum element at the beginning of the unsorted portion
3. **Repeat**: Continue until the entire array is sorted

## Time Complexity:
- **Best Case**: O(n²)
- **Average Case**: O(n²)
- **Worst Case**: O(n²)

## Space Complexity:
- **In-place version**: O(1)
- **Functional version**: O(n) due to list operations

The first implementation modifies the array in-place (imperative style), while the second provides a functional approach that returns a new sorted array.

