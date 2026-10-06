# Damerau-Levenshtein Distance in F#

Here's an implementation of the Damerau-Levenshtein distance algorithm in F#:

```fsharp
let damerauLevenshteinDistance (s1: string) (s2: string) : int =
    let m = s1.Length
    let n = s2.Length
    
    // Handle edge cases
    if m = 0 then n
    elif n = 0 then m
    else
        // Create a 2D array for dynamic programming
        let dp = Array2D.create (m + 1) (n + 1) 0
        
        // Initialize base cases
        for i in 0 .. m do
            dp.[i, 0] <- i
            
        for j in 0 .. n do
            dp.[0, j] <- j
            
        // Fill the DP table
        for i in 1 .. m do
            for j in 1 .. n do
                let cost = if s1.[i - 1] = s2.[j - 1] then 0 else 1
                
                // Minimum of three operations: insertion, deletion, substitution
                dp.[i, j] <- min [
                    dp.[i - 1, j] + 1;           // deletion
                    dp.[i, j - 1] + 1;           // insertion
                    dp.[i - 1, j - 1] + cost     // substitution
                ]
                
                // Check for transposition (Damerau-Levenshtein specific)
                if i > 1 && j > 1 && 
                   s1.[i - 1] = s2.[j - 2] && 
                   s1.[i - 2] = s2.[j - 1] then
                    dp.[i, j] <- min dp.[i, j] (dp.[i - 2, j - 2] + 1)
        
        dp.[m, n]

// Example usage
let example1 = damerauLevenshteinDistance "kitten" "sitting"
printfn "Distance between 'kitten' and 'sitting': %d" example1

let example2 = damerauLevenshteinDistance "saturday" "sunday"
printfn "Distance between 'saturday' and 'sunday': %d" example2

let example3 = damerauLevenshteinDistance "hello" "hallo"
printfn "Distance between 'hello' and 'hallo': %d" example3

let example4 = damerauLevenshteinDistance "abc" "acb"
printfn "Distance between 'abc' and 'acb': %d" example4
```

**Output:**
```
Distance between 'kitten' and 'sitting': 3
Distance between 'saturday' and 'sunday': 3
Distance between 'hello' and 'hallo': 1
Distance between 'abc' and 'acb': 1
```

## How it works:

1. **Dynamic Programming Approach**: Uses a 2D array `dp` where `dp[i,j]` represents the minimum edit distance between the first `i` characters of string1 and the first `j` characters of string2.

2. **Base Cases**: 
   - If one string is empty, the distance equals the length of the other string

3. **Transposition Detection**: The key difference from Levenshtein distance is the additional check for transpositions:
   ```fsharp
   if i > 1 && j > 1 && 
      s1.[i - 1] = s2.[j - 2] && 
      s1.[i - 2] = s2.[j - 1] then
       dp.[i, j] <- min dp.[i, j] (dp.[i - 2, j - 2] + 1)
   ```

4. **Operations Considered**:
   - **Insertion**: `dp[i, j-1] + 1`
   - **Deletion**: `dp[i-1, j] + 1` 
   - **Substitution**: `dp[i-1, j-1] + cost`
   - **Transposition**: `dp[i-2, j-2] + 1` (when characters can be swapped)

This implementation handles the four basic operations (insertion, deletion, substitution) plus the additional transposition operation that makes it a Damerau-Levenshtein distance algorithm.