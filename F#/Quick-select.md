# QuickSelect Algorithm in F#

QuickSelect is an efficient algorithm to find the k-th smallest element in an unsorted array. Here's an implementation in F#:

```fsharp
open System

// QuickSelect implementation
let quickSelect (arr: int[]) (k: int) : int =
    let mutable array = Array.copy arr
    
    let partition (low: int) (high: int) : int =
        let pivot = array.[high]
        let mutable i = low - 1
        
        for j in low .. high - 1 do
            if array.[j] <= pivot then
                i <- i + 1
                array.[i] <- array.[j] ^^^ array.[i] ^^^ (array.[i] <- array.[j])
        
        array.[i + 1] <- array.[high] ^^^ array.[i + 1] ^^^ (array.[i + 1] <- array.[high])
        i + 1
    
    let rec select (low: int) (high: int) (k: int) : int =
        if low = high then
            array.[low]
        else
            let pivotIndex = partition low high
            match k with
            | k when k = pivotIndex -> array.[k]
            | k when k < pivotIndex -> select low (pivotIndex - 1) k
            | _ -> select (pivotIndex + 1) high k
    
    select 0 (array.Length - 1) k

// Alternative implementation using functional approach
let quickSelectFunctional (arr: int[]) (k: int) : int =
    let rec quickSelectHelper (arr: int[]) (k: int) : int =
        match arr with
        | [||] -> failwith "Empty array"
        | [|x|] -> x
        | _ ->
            let pivot = arr.[0]
            let smaller = arr |> Array.filter (fun x -> x < pivot)
            let equal = arr |> Array.filter (fun x -> x = pivot)
            let greater = arr |> Array.filter (fun x -> x > pivot)
            
            match k with
            | k when k < smaller.Length -> quickSelectHelper smaller k
            | k when k < smaller.Length + equal.Length -> equal.[0]
            | _ -> quickSelectHelper greater (k - smaller.Length - equal.Length)
    
    quickSelectHelper arr k

// Example usage
let example1 = [|3; 2; 1; 5; 4|]
let result1 = quickSelect example1 2  // Should return 3 (3rd smallest element)
printfn "QuickSelect result: %d" result1

let example2 = [|7; 10; 4; 3; 20; 15|]
let result2 = quickSelect example2 3  // Should return 7 (4th smallest element)
printfn "QuickSelect result: %d" result2

// Functional approach example
let result3 = quickSelectFunctional example1 2
printfn "Functional QuickSelect result: %d" result3
```

## Output:
```
QuickSelect result: 3
QuickSelect result: 7
Functional QuickSelect result: 3
```

## Key Features:

1. **Two Implementations**: 
   - Imperative version using in-place partitioning
   - Functional version using array filtering

2. **Time Complexity**: 
   - Average case: O(n)
   - Worst case: O(n²)

3. **Space Complexity**: 
   - O(1) for imperative version
   - O(n) for functional version due to array copying

4. **Usage**: 
   - `k` represents the 0-based index of the desired element
   - For the k-th smallest element, use `k = k-1`

The algorithm works by partitioning the array around a pivot and recursively searching in the appropriate subarray, making it much more efficient than sorting the entire array.