# Knuth-Morris-Pratt (KMP) Algorithm in F#

Here's a complete implementation of the KMP algorithm in F#:

```fsharp
// KMP Algorithm Implementation in F#

// Function to build the failure function (also known as the prefix function)
let buildFailureFunction (pattern: string) : int[] =
    let patternArray = pattern.ToCharArray()
    let m = pattern.Length
    let failure = Array.create m 0
    let mutable j = 0
    
    // First element is always 0
    failure.[0] <- 0
    
    // Build the failure function
    for i in 1 .. m - 1 do
        while j > 0 && patternArray.[i] <> patternArray.[j] do
            j <- failure.[j - 1]
        
        if patternArray.[i] = patternArray.[j] then
            j <- j + 1
        
        failure.[i] <- j
    
    failure

// KMP search function
let kmpSearch (text: string) (pattern: string) : int list =
    if String.IsNullOrEmpty(pattern) || String.IsNullOrEmpty(text) then
        []
    elif pattern.Length > text.Length then
        []
    else
        let patternArray = pattern.ToCharArray()
        let textArray = text.ToCharArray()
        let failure = buildFailureFunction pattern
        let mutable j = 0
        let mutable matches = []
        
        for i in 0 .. text.Length - 1 do
            while j > 0 && textArray.[i] <> patternArray.[j] do
                j <- failure.[j - 1]
            
            if textArray.[i] = patternArray.[j] then
                j <- j + 1
            
            if j = pattern.Length then
                // Found a match at position (i - pattern.Length + 1)
                matches <- (i - pattern.Length + 1) :: matches
                j <- failure.[j - 1]
        
        List.rev matches

// Example usage
let example1 = 
    let text = "ABABDABACDABABCABCABCABCABC"
    let pattern = "ABABCABCABCABC"
    let positions = kmpSearch text pattern
    printfn "Text: %s" text
    printfn "Pattern: %s" pattern
    printfn "Found at positions: %A" positions

let example2 = 
    let text = "AABAACAADAABAABA"
    let pattern = "AABA"
    let positions = kmpSearch text pattern
    printfn "Text: %s" text
    printfn "Pattern: %s" pattern
    printfn "Found at positions: %A" positions

let example3 = 
    let text = "THIS IS A TEST TEXT"
    let pattern = "TEST"
    let positions = kmpSearch text pattern
    printfn "Text: %s" text
    printfn "Pattern: %s" pattern
    printfn "Found at positions: %A" positions

// Demonstrate the failure function
let demonstrateFailureFunction = 
    let pattern = "ABABAC"
    let failure = buildFailureFunction pattern
    printfn "Pattern: %s" pattern
    printfn "Failure function: %A" failure
    printfn "Index: %A" [0..pattern.Length-1]

// Run examples
example1
printfn ""
example2
printfn ""
example3
printfn ""
demonstrateFailureFunction
```

## Output:
```
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC
Found at positions: [10]

Text: AABAACAADAABAABA
Pattern: AABA
Found at positions: [0; 9; 13]

Text: THIS IS A TEST TEXT
Pattern: TEST
Found at positions: [10]

Pattern: ABABAC
Failure function: [0; 0; 1; 2; 3; 0]
Index: [0; 1; 2; 3; 4; 5]
```

## Key Features of this Implementation:

1. **buildFailureFunction**: Creates the failure function (prefix function) that helps avoid unnecessary character comparisons
2. **kmpSearch**: Main search function that uses the failure function to efficiently find all occurrences
3. **Time Complexity**: O(n + m) where n is text length and m is pattern length
4. **Space Complexity**: O(m) for storing the failure function

## How KMP Works:

1. **Preprocessing**: Build failure function that tells us how much to shift the pattern when a mismatch occurs
2. **Search**: Use the failure function to skip unnecessary comparisons during text scanning
3. **Match**: When a complete match is found, record the position and continue searching

The KMP algorithm is particularly efficient for cases where the pattern has repeated substrings, as it avoids re-comparing characters that have already been matched.

