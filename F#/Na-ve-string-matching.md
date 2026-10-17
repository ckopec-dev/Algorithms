# Naïve String Matching Algorithm in F#

Here's an implementation of the naïve string matching algorithm in F#:

```fsharp
// Naïve string matching algorithm implementation
let naiveStringMatch (pattern: string) (text: string) : int list =
    let patternLength = pattern.Length
    let textLength = text.Length
    
    // Handle edge cases
    if patternLength = 0 || textLength = 0 || patternLength > textLength then
        []
    else
        let rec search i matches =
            // If we've reached the end of text, return matches
            if i > textLength - patternLength then
                matches
            else
                // Check if pattern matches at position i
                let isMatch = 
                    let rec check j =
                        if j >= patternLength then
                            true
                        elif text.[i + j] = pattern.[j] then
                            check (j + 1)
                        else
                            false
                    check 0
                
                // If match found, add position to matches and continue searching
                if isMatch then
                    search (i + 1) (i :: matches)
                else
                    search (i + 1) matches
        
        // Start searching from position 0 and reverse the result
        List.rev (search 0 [])

// Alternative more concise implementation using built-in functions
let naiveStringMatchSimple (pattern: string) (text: string) : int list =
    let patternLength = pattern.Length
    let textLength = text.Length
    
    if patternLength = 0 || textLength = 0 || patternLength > textLength then
        []
    else
        [0 .. textLength - patternLength]
        |> List.filter (fun i -> 
            String.sub text i patternLength = pattern)
        |> List.rev

// Example usage and testing
let example1 = 
    let text = "ABABCABABA"
    let pattern = "ABA"
    printfn "Text: %s" text
    printfn "Pattern: %s" pattern
    let matches = naiveStringMatch pattern text
    printfn "Found at positions: %A" matches
    matches

let example2 = 
    let text = "AAAA"
    let pattern = "AA"
    printfn "\nText: %s" text
    printfn "Pattern: %s" pattern
    let matches = naiveStringMatch pattern text
    printfn "Found at positions: %A" matches
    matches

let example3 = 
    let text = "Hello World"
    let pattern = "World"
    printfn "\nText: %s" text
    printfn "Pattern: %s" pattern
    let matches = naiveStringMatch pattern text
    printfn "Found at positions: %A" matches
    matches

// Run examples
let _ = 
    example1 |> ignore
    example2 |> ignore
    example3 |> ignore
```

## Output:
```
Text: ABABCABABA
Pattern: ABA
Found at positions: [0; 5; 7]

Text: AAAA
Pattern: AA
Found at positions: [0; 1; 2]

Text: Hello World
Pattern: World
Found at positions: [6]
```

## Algorithm Explanation:

The naïve string matching algorithm works by:
1. **Sliding the pattern** over the text from left to right
2. **Comparing character by character** at each position
3. **Recording matches** when the entire pattern is found
4. **Continuing** until the end of the text

## Time Complexity:
- **Best case**: O(n) - when pattern is not found
- **Average/Worst case**: O(n × m) where n is text length and m is pattern length

## Space Complexity:
- O(k) where k is the number of matches found

The algorithm is simple but inefficient for large texts. It's mainly used for educational purposes or when simplicity is preferred over performance.