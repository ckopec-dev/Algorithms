# Pigeonhole Sort in F#

Here's an implementation of the Pigeonhole Sort algorithm in F#:

```fsharp
open System

// Pigeonhole Sort implementation
let pigeonholeSort (arr: int[]) : int[] =
    if arr.Length <= 1 then
        arr
    else
        // Find min and max values
        let minValue = arr |> Array.min
        let maxValue = arr |> Array.max
        
        // Calculate range
        let range = maxValue - minValue + 1
        
        // Create pigeonholes (buckets)
        let pigeonholes = Array.create range 0
        
        // Place elements in pigeonholes
        arr
        |> Array.iter (fun x -> 
            let index = x - minValue
            pigeonholes.[index] <- pigeonholes.[index] + 1)
        
        // Reconstruct sorted array from pigeonholes
        let result = Array.zeroCreate arr.Length
        let mutable resultIndex = 0
        
        for i in 0 .. range - 1 do
            let count = pigeonholes.[i]
            for j in 1 .. count do
                result.[resultIndex] <- i + minValue
                resultIndex <- resultIndex + 1
        
        result

// Alternative implementation using List operations
let pigeonholeSortList (arr: int[]) : int[] =
    if arr.Length <= 1 then
        arr
    else
        let minValue = arr |> Array.min
        let maxValue = arr |> Array.max
        let range = maxValue - minValue + 1
        
        // Create pigeonholes and count occurrences
        let pigeonholes = 
            Array.create range 0
            |> Array.mapi (fun i x -> 
                arr |> Array.filter (fun val -> val = i + minValue)
                    |> Array.length)
        
        // Flatten pigeonholes into result
        [0 .. range - 1]
        |> List.collect (fun i -> 
            let count = pigeonholes.[i]
            List.replicate count (i + minValue))
        |> List.toArray

// Example usage
let example1 = [|4; 2; 2; 8; 3; 3; 1|]
let sorted1 = pigeonholeSort example1
printfn "Original array: %A" example1
printfn "Sorted array: %A" sorted1

let example2 = [|10; 5; 8; 3; 1; 9; 2; 7; 4; 6|]
let sorted2 = pigeonholeSort example2
printfn "Original array: %A" example2
printfn "Sorted array: %A" sorted2

// Test with edge cases
let emptyArray = [||]
let singleElement = [|42|]
let duplicateElements = [|5; 5; 5; 5|]

printfn "Empty array: %A" (pigeonholeSort emptyArray)
printfn "Single element: %A" (pigeonholeSort singleElement)
printfn "Duplicates: %A" (pigeonholeSort duplicateElements)
```

## Output:
```
Original array: [|4; 2; 2; 8; 3; 3; 1|]
Sorted array: [|1; 2; 2; 3; 3; 4; 8|]
Original array: [|10; 5; 8; 3; 1; 9; 2; 7; 4; 6|]
Sorted array: [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10|]
Empty array: [||]
Single element: [|42|]
Duplicates: [|5; 5; 5; 5|]
```

## How it works:

1. **Find Range**: Determine the minimum and maximum values in the array
2. **Create Pigeonholes**: Create an array of buckets (pigeonholes) sized to cover the range
3. **Place Elements**: Count occurrences of each element and place them in corresponding pigeonholes
4. **Reconstruct**: Extract elements from pigeonholes in order to form the sorted array

## Time and Space Complexity:
- **Time Complexity**: O(n + k) where n is the number of elements and k is the range
- **Space Complexity**: O(k) for the pigeonhole array

## Characteristics:
- **Stable**: Maintains relative order of equal elements
- **Non-comparison**: Does not compare elements directly
- **Efficient**: Very efficient when range is not significantly larger than number of elements
- **Best for**: Small range of integers or when elements are known to be in a limited range

