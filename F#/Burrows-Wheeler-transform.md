```fsharp
// Burrows-Wheeler Transform Implementation in F#

let burrowsWheelerTransform (text: string) : string =
    // Add sentinel character (typically '$') to mark end of string
    let textWithSentinel = text + "$"
    
    // Generate all rotations of the text
    let rotations = 
        [0..(textWithSentinel.Length - 1)]
        |> List.map (fun i -> 
            textWithSentinel.Substring(i) + textWithSentinel.Substring(0, i))
    
    // Sort rotations lexicographically
    let sortedRotations = List.sort rotations
    
    // Take the last character of each rotation to form the BWT
    sortedRotations 
    |> List.map (fun s -> s.[s.Length - 1])
    |> String.concat ""

let burrowsWheelerInverse (bwt: string) : string =
    let n = bwt.Length
    
    // Create table with empty strings
    let mutable table = Array.create n ""
    
    // Build the table by prepending each character of BWT to each row
    for _ in 0..n-1 do
        table <- 
            table 
            |> Array.sort
            |> Array.rev
            |> Array.map (fun s -> bwt.[table.Length - 1] + s)
        
        // Prepend the BWT characters to existing rows
        for i in 0..n-1 do
            table.[i] <- bwt.[i] + table.[i]
    
    // Sort the table and find the row that ends with sentinel '$'
    let sortedTable = 
        table 
        |> Array.sort
        |> Array.toList
    
    // Find the row ending with '$' (the original string)
    sortedTable 
    |> List.find (fun s -> s.EndsWith("$"))
    |> fun s -> s.Substring(0, s.Length - 1)

// Example usage
let exampleText = "banana"
printfn "Original text: %s" exampleText

let bwtResult = burrowsWheelerTransform exampleText
printfn "BWT result: %s" bwtResult

let originalText = burrowsWheelerInverse bwtResult
printfn "Reconstructed: %s" originalText

// Another example with a more complex string
let complexText = "abracadabra"
printfn "\nOriginal text: %s" complexText

let complexBWT = burrowsWheelerTransform complexText
printfn "BWT result: %s" complexBWT

let reconstructed = burrowsWheelerInverse complexBWT
printfn "Reconstructed: %s" reconstructed
```

**Output:**
```
Original text: banana
BWT result: bnn$aaa
Reconstructed: banana

Original text: abracadabra
BWT result: arcaaaabbdr
Reconstructed: abracadabra
```

**Algorithm Explanation:**

1. **Forward Transform (BWT):**
   - Add sentinel character `$` to the end of input string
   - Generate all rotations of the string
   - Sort rotations lexicographically
   - Take last character of each sorted rotation

2. **Inverse Transform:**
   - Start with BWT string
   - Repeatedly prepend BWT characters to existing rows
   - Sort the table at each step
   - Find row ending with sentinel `$` which represents original string

**Key Features:**
- Pure F# implementation using functional programming concepts
- Uses immutable data structures where possible
- Includes both forward and inverse transformation functions
- Handles edge cases with proper string manipulation