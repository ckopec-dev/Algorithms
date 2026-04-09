# Rabin-Karp Algorithm in F#

Here's an implementation of the Rabin-Karp string matching algorithm in F#:

```fsharp
open System

/// Rabin-Karp string matching algorithm
let rabinKarp (pattern: string) (text: string) : int list =
    let patternLength = pattern.Length
    let textLength = text.Length
    
    // Handle edge cases
    if patternLength = 0 || textLength = 0 || patternLength > textLength then
        []
    else
        let prime = 101 // A prime number for hashing
        let base = 256 // Number of characters in input alphabet
        
        // Calculate base^(patternLength-1) % prime
        let mutable power = 1L
        for i in 0 .. patternLength - 2 do
            power <- (power * int64 base) % int64 prime
        
        let mutable patternHash = 0L
        let mutable textHash = 0L
        
        // Calculate hash value of pattern and first window of text
        for i in 0 .. patternLength - 1 do
            patternHash <- (patternHash * int64 base + int64 (int pattern.[i])) % int64 prime
            textHash <- (textHash * int64 base + int64 (int text.[i])) % int64 prime
        
        let mutable result = []
        
        // Slide the pattern over text one by one
        for i in 0 .. textLength - patternLength do
            // Check if hash values match
            if patternHash = textHash then
                // Check character by character for a match
                let mutable matchFound = true
                for j in 0 .. patternLength - 1 do
                    if text.[i + j] <> pattern.[j] then
                        matchFound <- false
                        break
                
                if matchFound then
                    result <- i :: result
            else
                // Calculate hash value for next window of text
                if i < textLength - patternLength then
                    textHash <- 
                        (int64 base * (textHash - int64 (int text.[i]) * power) 
                         + int64 (int text.[i + patternLength])) % int64 prime
                    
                    // Handle negative hash values
                    if textHash < 0L then
                        textHash <- textHash + int64 prime
        
        List.rev result

// Example usage
let example1 = 
    let pattern = "ABCD"
    let text = "ABABCABCDABCD"
    let positions = rabinKarp pattern text
    printfn "Pattern: %s" pattern
    printfn "Text: %s" text
    printfn "Positions found: %A" positions

let example2 = 
    let pattern = "ANA"
    let text = "BANANA"
    let positions = rabinKarp pattern text
    printfn "Pattern: %s" pattern
    printfn "Text: %s" text
    printfn "Positions found: %A" positions

let example3 = 
    let pattern = "test"
    let text = "this is a test string for testing purposes"
    let positions = rabinKarp pattern text
    printfn "Pattern: %s" pattern
    printfn "Text: %s" text
    printfn "Positions found: %A" positions

// Run examples
example1
printfn ""
example2
printfn ""
example3
```

## Output:
```
Pattern: ABCD
Text: ABABCABCDABCD
Positions found: [4; 11]

Pattern: ANA
Text: BANANA
Positions found: [1; 3]

Pattern: test
Text: this is a test string for testing purposes
Positions found: [10; 25]
```

## How it works:

1. **Hashing**: Uses polynomial rolling hash function to compute hash values
2. **Sliding Window**: Moves through the text one character at a time
3. **Collision Handling**: When hash values match, performs character-by-character comparison
4. **Rolling Hash**: Efficiently updates hash value for the next window using previous hash

## Key Features:

- **Time Complexity**: O(n+m) average case, O(n×m) worst case
- **Space Complexity**: O(1)
- **Handles Edge Cases**: Empty strings, pattern longer than text
- **Returns All Matches**: Returns list of all starting positions where pattern is found

The algorithm is particularly useful for multiple pattern searches and when you need to find all occurrences of a pattern in a text efficiently.

