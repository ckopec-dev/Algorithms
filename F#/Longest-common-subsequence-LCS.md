# Longest Common Subsequence (LCS) in F#

Here's an implementation of the LCS algorithm in F# using dynamic programming:

```fsharp
// Function to compute the length of longest common subsequence
let lcsLength (x: string) (y: string) : int =
    let m = x.Length
    let n = y.Length
    
    // Create a 2D array to store the lengths of LCS
    let dp = Array2D.create (m + 1) (n + 1) 0
    
    // Fill the dp table
    for i in 1 .. m do
        for j in 1 .. n do
            if x.[i - 1] = y.[j - 1] then
                dp.[i, j] <- dp.[i - 1, j - 1] + 1
            else
                dp.[i, j] <- max dp.[i - 1, j] dp.[i, j - 1]
    
    dp.[m, n]

// Function to compute the actual LCS string
let lcsString (x: string) (y: string) : string =
    let m = x.Length
    let n = y.Length
    
    // Create a 2D array to store the lengths of LCS
    let dp = Array2D.create (m + 1) (n + 1) 0
    
    // Fill the dp table
    for i in 1 .. m do
        for j in 1 .. n do
            if x.[i - 1] = y.[j - 1] then
                dp.[i, j] <- dp.[i - 1, j - 1] + 1
            else
                dp.[i, j] <- max dp.[i - 1, j] dp.[i, j - 1]
    
    // Backtrack to find the actual LCS
    let rec backtrack i j acc =
        if i <= 0 || j <= 0 then
            acc
        elif x.[i - 1] = y.[j - 1] then
            backtrack (i - 1) (j - 1) (x.[i - 1] :: acc)
        elif dp.[i - 1, j] > dp.[i, j - 1] then
            backtrack (i - 1) j acc
        else
            backtrack i (j - 1) acc
    
    backtrack m n [] |> List.toArray |> System.String.Concat

// Example usage
let example1 = 
    let str1 = "ABCDGH"
    let str2 = "AEDFHR"
    printfn "String 1: %s" str1
    printfn "String 2: %s" str2
    printfn "LCS Length: %d" (lcsLength str1 str2)
    printfn "LCS String: %s" (lcsString str1 str2)
    printfn ""

let example2 = 
    let str1 = "AGGTAB"
    let str2 = "GXTXAYB"
    printfn "String 1: %s" str1
    printfn "String 2: %s" str2
    printfn "LCS Length: %d" (lcsLength str1 str2)
    printfn "LCS String: %s" (lcsString str1 str2)
    printfn ""

// Run examples
example1
example2
```

## Output:
```
String 1: ABCDGH
String 2: AEDFHR
LCS Length: 3
LCS String: ADH

String 1: AGGTAB
String 2: GXTXAYB
LCS Length: 4
LCS String: GTAB
```

## Key Features of this Implementation:

1. **Dynamic Programming Approach**: Uses a 2D array to store intermediate results
2. **Time Complexity**: O(m × n) where m and n are the lengths of the strings
3. **Space Complexity**: O(m × n) for the DP table
4. **Two Functions**: 
   - `lcsLength`: Returns just the length of LCS
   - `lcsString`: Returns the actual LCS string
5. **Functional Style**: Uses immutable data structures and recursion where appropriate

The algorithm works by building up a table where each cell `dp[i,j]` represents the length of the LCS between the first `i` characters of string `x` and the first `j` characters of string `y`.

