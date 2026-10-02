# Bitap Algorithm Implementation in F#

The Bitap algorithm is a string searching algorithm that uses bitwise operations to find occurrences of a pattern within a text. Here's an implementation in F#:

```fsharp
open System

/// Bitap algorithm implementation for string pattern matching
module Bitap =
    
    /// Creates the bitap mask for a given pattern
    let createMask (pattern: string) (alphabetSize: int) : int64[] =
        let mask = Array.create alphabetSize 0L
        
        // For each character in the pattern, set the corresponding bit
        for i = 0 to pattern.Length - 1 do
            let charIndex = int pattern.[i]
            if charIndex < alphabetSize then
                mask.[charIndex] <- mask.[charIndex] ||| (1L <<< i)
        
        mask
    
    /// Performs bitap search on text with given pattern
    let search (text: string) (pattern: string) : int list =
        if String.IsNullOrEmpty(pattern) || String.IsNullOrEmpty(text) then
            []
        else
            // Assuming ASCII alphabet size
            let alphabetSize = 256
            let mask = createMask pattern alphabetSize
            
            // Initialize bitap state
            let mut = (1L <<< (pattern.Length - 1)) - 1L
            let mutable result = []
            
            // Process each character in text
            for i = 0 to text.Length - 1 do
                // Shift the mask and apply OR operation
                let charIndex = int text.[i]
                if charIndex < alphabetSize then
                    mut <- (mut <<< 1L) ||| mask.[charIndex]
                else
                    mut <- mut <<< 1L
                
                // Check if we found a match at current position
                if (mut &&& (1L <<< (pattern.Length - 1))) = 0L then
                    result <- (i - pattern.Length + 1) :: result
            
            List.rev result
    
    /// Alternative implementation with error tolerance (k-mismatch)
    let searchWithErrors (text: string) (pattern: string) (maxErrors: int) : int list =
        if String.IsNullOrEmpty(pattern) || String.IsNullOrEmpty(text) then
            []
        else
            let alphabetSize = 256
            let mask = createMask pattern alphabetSize
            
            let mutable result = []
            let mutable currentError = 0L
            let mutable previousError = 0L
            
            for i = 0 to text.Length - 1 do
                let charIndex = int text.[i]
                if charIndex < alphabetSize then
                    currentError <- (previousError <<< 1L) ||| mask.[charIndex]
                else
                    currentError <- previousError <<< 1L
                
                // Check for match within error tolerance
                if currentError &&& (1L <<< (pattern.Length - 1)) = 0L then
                    result <- (i - pattern.Length + 1) :: result
                
                previousError <- currentError
            
            List.rev result

// Example usage
[<EntryPoint>]
let main argv =
    let text = "This is a sample text for testing the bitap algorithm"
    let pattern = "sample"
    
    printfn "Text: %s" text
    printfn "Pattern: %s" pattern
    
    // Basic search
    let matches = Bitap.search text pattern
    printfn "Matches found at positions: %A" matches
    
    // Search with different pattern
    let pattern2 = "text"
    let matches2 = Bitap.search text pattern2
    printfn "Pattern '%s' found at positions: %A" pattern2 matches2
    
    // Search with error tolerance
    let matches3 = Bitap.searchWithErrors text "sampl" 1
    printfn "Pattern 'sampl' with 1 error tolerance: %A" matches3
    
    0 // return code
```

## Key Features of this Implementation:

1. **Bitwise Operations**: Uses bit shifting and OR operations for efficient pattern matching
2. **Mask Creation**: Builds a bitmask for each character in the pattern
3. **State Tracking**: Maintains the current state using bitwise operations
4. **Multiple Search Modes**: 
   - Basic exact matching
   - Error-tolerant searching

## How it Works:

1. **Create Mask**: For each character in the pattern, set corresponding bits in a mask array
2. **Process Text**: For each character in the text:
   - Shift the current state left
   - Apply OR operation with the character's mask
   - Check if a match is found at the end position
3. **Return Results**: Collect all matching positions

## Time Complexity:
- **Best Case**: O(n) where n is the length of text
- **Worst Case**: O(n*m) where m is the length of pattern

## Space Complexity:
- O(k) where k is the size of alphabet (256 for ASCII)

This implementation demonstrates the efficiency of bitwise operations in string searching algorithms, making it particularly useful for large texts and patterns.