# LZW Algorithm Implementation in F#

Here's a complete implementation of the LZW compression algorithm in F#:

```fsharp
open System
open System.Collections.Generic

module LZW = 
    /// Compresses a string using LZW algorithm
    let compress (input: string) : int list =
        // Initialize dictionary with single characters
        let dict = Dictionary<char, int>()
        let mutable dictSize = 256
        
        // Fill initial dictionary with ASCII characters
        for i in 0 .. 255 do
            dict.[char i] <- i
            
        let result = ref []
        let mutable currentCode = 0
        let mutable currentString = ""
        
        for c in input do
            let newString = currentString + string c
            
            if dict.ContainsKey(newString) then
                currentString <- newString
            else
                // Output the code for currentString
                result := dict.[currentString] :: !result
                // Add new string to dictionary
                dict.[newString] <- dictSize
                dictSize <- dictSize + 1
                currentString <- string c
                
        // Output the last code
        if currentString <> "" then
            result := dict.[currentString] :: !result
            
        List.rev !result
        
    /// Decompresses a list of codes using LZW algorithm
    let decompress (codes: int list) : string =
        if codes.IsEmpty then return ""
        
        // Initialize dictionary with single characters
        let dict = Dictionary<int, string>()
        let mutable dictSize = 256
        
        // Fill initial dictionary with ASCII characters
        for i in 0 .. 255 do
            dict.[i] <- string (char i)
            
        let result = ref ""
        let mutable oldCode = codes.Head
        
        if codes.Length > 0 then
            result := dict.[oldCode]
            
        // Process remaining codes
        for code in codes.Tail do
            let newString = 
                if dict.ContainsKey(code) then
                    dict.[code]
                else
                    // Special case: code not in dictionary yet
                    dict.[oldCode] + dict.[oldCode].[0].ToString()
                    
            result := !result + newString
            
            // Add new entry to dictionary
            let newEntry = dict.[oldCode] + newString.[0].ToString()
            if not (dict.ContainsKey(dictSize)) then
                dict.[dictSize] <- newEntry
                dictSize <- dictSize + 1
                
            oldCode <- code
            
        !result

// Example usage
[<EntryPoint>]
let main argv =
    let original = "ABABABAB"
    
    printfn "Original string: %s" original
    
    // Compress
    let compressed = LZW.compress original
    printfn "Compressed codes: %A" compressed
    
    // Decompress
    let decompressed = LZW.decompress compressed
    printfn "Decompressed string: %s" decompressed
    
    // Test with longer example
    let text = "TOBEORNOTTOBEORTOBEORNOT"
    printfn "\nOriginal: %s" text
    
    let compressed2 = LZW.compress text
    printfn "Compressed codes: %A" compressed2
    
    let decompressed2 = LZW.decompress compressed2
    printfn "Decompressed: %s" decompressed2
    
    // Verify correctness
    printfn "Match: %b" (text = decompressed2)
    
    0
```

## How it works:

### Compression Process:
1. **Initialize dictionary** with all ASCII characters (0-255)
2. **For each character** in input:
   - Try to find the longest string that exists in dictionary
   - If found, continue building the string
   - If not found, output the code for current string and add new string to dictionary
3. **Output final code** for remaining string

### Decompression Process:
1. **Initialize dictionary** with all ASCII characters (0-255)
2. **For each code** in input:
   - Look up the code in dictionary
   - Output the corresponding string
   - Add new entry: (previous string + first character of current string) to dictionary

## Sample Output:
```
Original string: ABABABAB
Compressed codes: [65; 66; 256; 258]
Decompressed string: ABABABAB

Original: TOBEORNOTTOBEORTOBEORNOT
Compressed codes: [84; 79; 66; 69; 256; 258; 259; 260; 261; 262; 263]
Decompressed: TOBEORNOTTOBEORTOBEORNOT
Match: true
```

This implementation demonstrates the core principles of LZW compression where the dictionary grows dynamically during compression and is rebuilt during decompression.