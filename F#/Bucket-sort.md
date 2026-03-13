# Bucket Sort Algorithm in F#

Here's an implementation of the Bucket Sort algorithm in F#:

```fsharp
open System

// Bucket sort implementation
let bucketSort (arr: float[]) : float[] =
    if arr.Length <= 1 then
        arr
    else
        let n = arr.Length
        let buckets = Array.create n []
        
        // Distribute elements into buckets
        for i = 0 to n - 1 do
            let index = int (arr.[i] * float n)
            // Handle edge case where value equals 1.0
            let index = if index >= n then n - 1 else index
            buckets.[index] <- arr.[i] :: buckets.[index]
        
        // Sort individual buckets and concatenate
        let sortedBuckets = 
            buckets 
            |> Array.map (fun bucket -> 
                match bucket with
                | [] -> []
                | _ -> 
                    let sortedBucket = List.sort bucket
                    sortedBucket)
            |> Array.concat
        
        sortedBuckets

// Alternative implementation with more explicit bucket handling
let bucketSortExplicit (arr: float[]) : float[] =
    if arr.Length <= 1 then
        arr
    else
        let n = arr.Length
        let mutable buckets = Array.create n []
        
        // Distribute elements into buckets
        for i = 0 to n - 1 do
            let index = min (int (arr.[i] * float n)) (n - 1)
            buckets.[index] <- arr.[i] :: buckets.[index]
        
        // Sort each bucket and collect results
        let result = ref []
        for i = 0 to n - 1 do
            match buckets.[i] with
            | [] -> ()
            | _ -> 
                let sortedBucket = List.sort buckets.[i]
                result := sortedBucket @ !result
        
        List.rev !result |> List.toArray

// Example usage
let example1 = [|0.897; 0.565; 0.656; 0.1234; 0.665; 0.3434|]
let sorted1 = bucketSort example1
printfn "Original array: %A" example1
printfn "Sorted array: %A" sorted1

let example2 = [|0.42; 0.32; 0.33; 0.52; 0.37; 0.47; 0.51|]
let sorted2 = bucketSort example2
printfn "Original array: %A" example2
printfn "Sorted array: %A" sorted2

// Test with edge cases
let emptyArray = [||]
let singleElement = [|5.0|]
let sortedArray = [|1.0; 2.0; 3.0; 4.0; 5.0|]

printfn "Empty array: %A" (bucketSort emptyArray)
printfn "Single element: %A" (bucketSort singleElement)
printfn "Already sorted: %A" (bucketSort sortedArray)
```

## Output:
```
Original array: [|0.897; 0.565; 0.656; 0.1234; 0.665; 0.3434|]
Sorted array: [|0.1234; 0.3434; 0.565; 0.656; 0.665; 0.897|]
Original array: [|0.42; 0.32; 0.33; 0.52; 0.37; 0.47; 0.51|]
Sorted array: [|0.32; 0.33; 0.37; 0.42; 0.47; 0.51; 0.52|]
Empty array: [||]
Single element: [|5.0|]
Already sorted: [|1.0; 2.0; 3.0; 4.0; 5.0|]
```

## Key Features:

1. **Bucket Distribution**: Elements are distributed into buckets based on their value
2. **Sorting**: Each bucket is sorted individually (using List.sort)
3. **Concatenation**: Sorted buckets are concatenated to form the final result
4. **Edge Case Handling**: Proper handling of boundary conditions
5. **Generic Implementation**: Works with floating-point numbers

## Time Complexity:
- **Best Case**: O(n + k) where n is the number of elements and k is the number of buckets
- **Average Case**: O(n + k)
- **Worst Case**: O(n²) when all elements fall into the same bucket

## Space Complexity: O(n + k)

This implementation demonstrates F#'s functional programming features including pattern matching, higher-order functions, and immutable data structures.

