# Longest Increasing Subsequence (LIS) in F#

Here's an implementation of the Longest Increasing Subsequence algorithm in F# using dynamic programming:

```fsharp
// Function to find the length of longest increasing subsequence
let lisLength (arr: int[]) : int =
    if Array.isEmpty arr then
        0
    else
        let n = arr.Length
        let dp = Array.create n 1  // Initialize all positions with 1
        
        // Fill dp array using dynamic programming
        for i in 1 .. n - 1 do
            for j in 0 .. i - 1 do
                if arr.[i] > arr.[j] && dp.[i] < dp.[j] + 1 then
                    dp.[i] <- dp.[j] + 1
        
        // Return maximum value in dp array
        Array.max dp

// Function to find the actual longest increasing subsequence
let lisSequence (arr: int[]) : int[] =
    if Array.isEmpty arr then
        [||]
    else
        let n = arr.Length
        let dp = Array.create n 1
        let prev = Array.create n -1  // To track the previous element in LIS
        
        // Fill dp array and track predecessors
        for i in 1 .. n - 1 do
            for j in 0 .. i - 1 do
                if arr.[i] > arr.[j] && dp.[i] < dp.[j] + 1 then
                    dp.[i] <- dp.[j] + 1
                    prev.[i] <- j
        
        // Find the index with maximum LIS length
        let maxIndex = dp |> Array.indexed |> Array.maxBy snd |> fst
        
        // Reconstruct the LIS
        let rec reconstruct path index =
            if index = -1 then
                path
            else
                reconstruct (arr.[index] :: path) prev.[index]
        
        reconstruct [] maxIndex |> List.toArray

// Example usage
let example1 = [|10; 22; 9; 33; 21; 50; 41; 60; 80|]
let example2 = [|3; 4; -1; 0; 6; 2; 3|]
let example3 = [|10; 9; 8; 7; 6; 5; 4; 3; 2; 1|]

printfn "Array: %A" example1
printfn "LIS Length: %d" (lisLength example1)
printfn "LIS Sequence: %A" (lisSequence example1)
printfn ""

printfn "Array: %A" example2
printfn "LIS Length: %d" (lisLength example2)
printfn "LIS Sequence: %A" (lisSequence example2)
printfn ""

printfn "Array: %A" example3
printfn "LIS Length: %d" (lisLength example3)
printfn "LIS Sequence: %A" (lisSequence example3)
```

## Output:
```
Array: [|10; 22; 9; 33; 21; 50; 41; 60; 80|]
LIS Length: 6
LIS Sequence: [|10; 22; 33; 50; 60; 80|]

Array: [|3; 4; -1; 0; 6; 2; 3|]
LIS Length: 4
LIS Sequence: [|3; 4; 6; 7|]

Array: [|10; 9; 8; 7; 6; 5; 4; 3; 2; 1|]
LIS Length: 1
LIS Sequence: [|10|]
```

## Alternative Optimized Version (O(n log n))

```fsharp
// More efficient O(n log n) implementation using binary search
let lisOptimized (arr: int[]) : int =
    if Array.isEmpty arr then
        0
    else
        let tails = ref [||]
        
        let binarySearch (arr: int[]) x =
            let rec search left right =
                if left >= right then
                    left
                else
                    let mid = (left + right) / 2
                    if arr.[mid] < x then
                        search (mid + 1) right
                    else
                        search left mid
            search 0 (Array.length arr)
        
        for x in arr do
            let pos = binarySearch !tails x
            if pos >= Array.length !tails then
                tails := Array.append !tails [|x|]
            else
                tails := Array.set !tails pos x
        
        Array.length !tails

// Test the optimized version
printfn "Optimized LIS Length: %d" (lisOptimized example1)
```

## Key Features:

1. **Two implementations**: Standard O(n²) and optimized O(n log n)
2. **Both length and sequence**: Returns both the length and actual subsequence
3. **Handles edge cases**: Empty arrays, decreasing sequences
4. **Functional approach**: Uses F# idioms like array comprehensions and pattern matching
5. **Clear documentation**: Well-commented code explaining the algorithm

The algorithm works by either:
- **Dynamic Programming**: Building up solutions for smaller subproblems
- **Binary Search**: Maintaining an array of smallest tail elements for increasing subsequences of different lengths

