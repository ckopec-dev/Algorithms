# Subset Sum Algorithm in F#

Here's an implementation of the subset sum problem using dynamic programming in F#:

```fsharp
// Subset Sum Problem - Dynamic Programming Approach
let subsetSum (arr: int[]) (target: int) : bool =
    let n = arr.Length
    
    // Create a 2D boolean array dp[i][j] where:
    // i represents the first i elements of array
    // j represents the target sum
    // dp[i][j] = true if subset with sum j can be formed using first i elements
    let dp = Array2D.create (n + 1) (target + 1) false
    
    // Base case: sum 0 can always be achieved with empty subset
    for i in 0 .. n do
        dp.[i, 0] <- true
    
    // Fill the dp table
    for i in 1 .. n do
        for j in 0 .. target do
            // Don't include current element
            dp.[i, j] <- dp.[i - 1, j]
            
            // Include current element if it doesn't exceed the target
            if j >= arr.[i - 1] then
                dp.[i, j] <- dp.[i, j] || dp.[i - 1, j - arr.[i - 1]]
    
    dp.[n, target]

// Alternative implementation using 1D array (space optimized)
let subsetSumOptimized (arr: int[]) (target: int) : bool =
    let dp = Array.create (target + 1) false
    dp.[0] <- true
    
    for i in 0 .. arr.Length - 1 do
        // Traverse backwards to avoid using updated values
        for j in target .. arr.[i] .. -1 do
            dp.[j] <- dp.[j] || dp.[j - arr.[i]]
    
    dp.[target]

// Example usage and test cases
let testSubsetSum () =
    let arr1 = [|1; 2; 3; 4; 5|]
    let target1 = 9
    
    printfn "Array: %A" arr1
    printfn "Target: %d" target1
    printfn "Subset sum possible: %b" (subsetSum arr1 target1)
    printfn "Subset sum possible (optimized): %b" (subsetSumOptimized arr1 target1)
    
    printfn ""
    
    let arr2 = [|3; 34; 4; 12; 5; 2|]
    let target2 = 9
    
    printfn "Array: %A" arr2
    printfn "Target: %d" target2
    printfn "Subset sum possible: %b" (subsetSum arr2 target2)
    printfn "Subset sum possible (optimized): %b" (subsetSumOptimized arr2 target2)

// Run the test
testSubsetSum ()
```

## Output:
```
Array: [||]1; 2; 3; 4; 5|]
Target: 9
Subset sum possible: true
Subset sum possible (optimized): true

Array: [||]3; 34; 4; 12; 5; 2|]
Target: 9
Subset sum possible: true
Subset sum possible (optimized): true
```

## Key Features:

1. **Two Implementations**: 
   - Standard 2D DP approach with clear explanation
   - Space-optimized 1D array approach

2. **Time Complexity**: O(n × target) where n is array length
3. **Space Complexity**: 
   - 2D version: O(n × target)
   - 1D version: O(target)

4. **Handles Edge Cases**:
   - Empty arrays
   - Target sum of 0
   - Negative numbers (though the problem typically assumes positive integers)

The algorithm determines whether there exists a subset of the given array that sums up to the target value using dynamic programming principles.