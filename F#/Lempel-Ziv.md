# Lempel-Ziv Algorithm Implementation in F#

Here's a simple implementation of the Lempel-Ziv compression algorithm in F#:

```fsharp
open System
open System.Text

type LzNode = {
    Value: string
    Index: int
}

// Simple LZ77 compression implementation
let compressLZ77 (input: string) (windowSize: int) (lookaheadSize: int) =
    let inputChars = input.ToCharArray()
    let result = ref []
    
    let rec compress pos =
        if pos >= inputChars.Length then
            !result
        else
            // Find the longest match in the sliding window
            let windowStart = max 0 (pos - windowSize)
            let windowEnd = pos
            let lookaheadEnd = min (pos + lookaheadSize) inputChars.Length
            
            let mutable bestMatch = (0, 0) // (offset, length)
            let mutable bestLength = 0
            
            // Search for the longest match in the window
            for windowPos = windowStart to windowEnd - 1 do
                let mutable matchLength = 0
                let mutable i = 0
                
                while (windowPos + i < windowEnd) && 
                      (pos + i < lookaheadEnd) && 
                      (inputChars.[windowPos + i] = inputChars.[pos + i]) do
                    matchLength <- matchLength + 1
                    i <- i + 1
                
                if matchLength > bestLength then
                    bestLength <- matchLength
                    bestMatch <- (windowEnd - windowPos, matchLength)
            
            // If we found a match, add it to the result
            if bestLength > 0 then
                result := (bestMatch, inputChars.[pos + bestLength]) :: !result
                compress (pos + bestLength + 1)
            else
                // No match found, output the character directly
                result := ((0, 0), inputChars.[pos]) :: !result
                compress (pos + 1)
    
    compress 0 |> List.rev

// Simple LZ77 decompression implementation
let decompressLZ77 (compressed: (int * int * char) list) =
    let result = StringBuilder()
    
    for (offset, length, nextChar) in compressed do
        if length > 0 then
            // Copy from the sliding window
            let currentLength = result.Length
            let copyStart = currentLength - offset
            
            for i = 0 to length - 1 do
                result.Append(result.[copyStart + i]) |> ignore
        // Add the next character
        result.Append(nextChar) |> ignore
    
    result.ToString()

// Example usage
let exampleText = "ABABABABABABABABABABABABABABABABABABABABABABABABABABABABABABAB

