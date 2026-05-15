# Levenshtein Distance in F#

Here's an implementation of the Levenshtein distance algorithm in F#:

```fsharp
let levenshteinDistance (s1: string) (s2: string) : int =
    let m = s1.Length
    let n = s2.Length
    
    // Create a 2D array to store the distances
    let dp = Array2D.create (m + 1) (n + 1) 0
    
    // Initialize the first row and column
    for i in 0 .. m do
        dp.[i, 0] <- i
    
    for j in 0 .. n do
        dp.[0, j] <- j
    
    // Fill the dp table
    for i in 1 .. m do
        for j in 1 .. n do
            let cost = if s1.[i - 1] = s2.[j - 1] then 0 else 1
            dp.[i, j] <- 
                min [
                    dp.[i - 1, j] + 1          // deletion
                    dp.[i, j - 1] + 1          // insertion
                    dp.[i - 1, j - 1] + cost   // substitution
                ]
    
    dp.[m, n]

// Example usage
let result1 = levenshteinDistance "kitten" "sitting"
printfn "Distance between 'kitten' and 'sitting': %d" result1  // Output: 3

let result2 = levenshteinDistance "saturday" "sunday"
printfn "Distance between 'saturday' and 'sunday': %d" result2  // Output: 3

let result3 = levenshteinDistance "hello" "hello"
printfn "Distance between 'hello' and 'hello': %d" result3  // Output: 0

let result4 = levenshteinDistance "abc" "def"
printfn "Distance between 'abc' and 'def': %d" result4  // Output: 3
```

## How it works:

1. **Dynamic Programming Approach**: Uses a 2D array to store the minimum edit distances between prefixes of the two strings.

2. **Initialization**: 
   - First row represents the cost of converting empty string to prefix of s2
   - First column represents the cost of converting prefix of s1 to empty string

3. **Recurrence Relation**:
   - If characters match: cost = 0, otherwise cost = 1
   - For each cell: `dp[i,j] = min(deletion, insertion, substitution)`

4. **Time Complexity**: O(m × n) where m and n are the lengths of the strings
5. **Space Complexity**: O(m × n)

## Alternative Functional Implementation:

```fsharp
let levenshteinDistanceFunctional (s1: string) (s2: string) : int =
    let rec editDistance i j memo =
        if i = 0 then j
        elif j = 0 then i
        else
            match Map.tryFind (i, j) memo with
            | Some result -> result
            | None ->
                let cost = if s1.[i - 1] = s2.[j - 1] then 0 else 1
                let result = 
                    min [
                        editDistance (i - 1) j (Map.add (i, j) (i + 1) memo) + 1
                        editDistance i (j - 1) (Map.add (i, j) (j + 1) memo) + 1
                        editDistance (i - 1) (j - 1) (Map.add (i, j) cost memo) + cost
                    ]
                Map.add (i, j) result memo |> editDistance i j
    
    editDistance s1.Length s2.Length Map.empty
```

The first implementation is more straightforward and efficient for most use cases, while the functional version demonstrates a recursive approach with memoization.

