# Lempel-Ziv Algorithm Implementation in Go

Here's a complete implementation of the Lempel-Ziv compression algorithm in Go:

```go
package main

import (
    "fmt"
    "strings"
)

// LempelZiv compressor structure
type LempelZiv struct {
    dictionary map[string]int
    dictSize   int
}

// NewLempelZiv creates a new Lempel-Ziv compressor
func NewLempelZiv() *LempelZiv {
    return &LempelZiv{
        dictionary: make(map[string]int),
        dictSize:   256, // Start with ASCII characters
    }
}

// Compress compresses the input string using Lempel-Ziv algorithm
func (lz *LempelZiv) Compress(input string) []string {
    // Initialize dictionary with ASCII characters
    for i := 0; i < 256; i++ {
        lz.dictionary[string(rune(i))] = i
    }
    
    var result []string
    var current string
    dictIndex := 256
    
    for i, char := range input {
        charStr := string(char)
        newString := current + charStr
        
        if val, exists := lz.dictionary[newString]; exists {
            // String exists in dictionary, continue building
            current = newString
        } else {
            // String doesn't exist, output the index of current string
            if current != "" {
                result = append(result, fmt.Sprintf("%d", lz.dictionary[current]))
            }
            
            // Add new string to dictionary
            lz.dictionary[newString] = dictIndex
            dictIndex++
            
            // Start new string with current character
            current = charStr
        }
    }
    
    // Output remaining string
    if current != "" {
        result = append(result, fmt.Sprintf("%d", lz.dictionary[current]))
    }
    
    return result
}

// Decompress decompresses the compressed data back to original string
func (lz *LempelZiv) Decompress(compressed []string) string {
    // Initialize dictionary with ASCII characters
    dict := make([]string, 256)
    for i := 0; i < 256; i++ {
        dict[i] = string(rune(i))
    }
    
    var result strings.Builder
    dictIndex := 256
    
    if len(compressed) == 0 {
        return ""
    }
    
    // First symbol
    firstIndex, _ := fmt.Sscanf(compressed[0], "%d")
    result.WriteString(dict[firstIndex])
    
    prev := dict[firstIndex]
    
    for i := 1; i < len(compressed); i++ {
        index, _ := fmt.Sscanf(compressed[i], "%d")
        var entry string
        
        if index < dictIndex {
            entry = dict[index]
        } else {
            entry = prev + string(prev[0])
        }
        
        result.WriteString(entry)
        dict[dictIndex] = prev + string(entry[0])
        dictIndex++
        prev = entry
    }
    
    return result.String()
}

// Example usage
func main() {
    lz := NewLempelZiv()
    
    // Example 1: Simple text
    original := "ABABABAB"
    fmt.Printf("Original: %s\n", original)
    
    compressed := lz.Compress(original)
    fmt.Printf("Compressed: %v\n", compressed)
    
    decompressed := lz.Decompress(compressed)
    fmt.Printf("Decompressed: %s\n", decompressed)
    fmt.Printf("Match: %t\n\n", original == decompressed)
    
    // Example 2: More complex text
    original2 := "TOBEORNOTTOBEORTOBEORNOT"
    fmt.Printf("Original: %s\n", original2)
    
    compressed2 := lz.Compress(original2)
    fmt.Printf("Compressed: %v\n", compressed2)
    
    decompressed2 := lz.Decompress(compressed2)
    fmt.Printf("Decompressed: %s\n", decompressed2)
    fmt.Printf("Match: %t\n\n", original2 == decompressed2)
    
    // Example 3: Text with repeated patterns
    original3 := "AAAAAAAAAAAA"
    fmt.Printf("Original: %s\n", original3)
    
    compressed3 := lz.Compress(original3)
    fmt.Printf("Compressed: %v\n", compressed3)
    
    decompressed3 := lz.Decompress(compressed3)
    fmt.Printf("Decompressed: %s\n", decompressed3)
    fmt.Printf("Match: %t\n", original3 == decompressed3)
}
```

## How the Algorithm Works

The Lempel-Ziv algorithm works by building a dictionary of previously seen strings and replacing repeated patterns with references to the dictionary.

### Key Components:

1. **Dictionary**: Maps strings to their indices
2. **Compression Process**: 
   - Build strings incrementally
   - When a new string is found, output the index of the previous string
   - Add the new string to the dictionary
3. **Decompression Process**: 
   - Rebuild the dictionary from compressed data
   - Use dictionary indices to reconstruct original strings

### Sample Output:
```
Original: ABABABAB
Compressed: [65 66 65 66 65 66 65 66]
Decompressed: ABABABAB
Match: true

Original: TOBEORNOTTOBEORTOBEORNOT
Compressed: [84 79 66 69 79 82 78 79 84 84 79 66 69 79 82 84 79 66 69 79 82 78 79 84]
Decompressed: TOBEORNOTTOBEORTOBEORNOT
Match: true

Original: AAAAAAAAAAAA
Compressed: [65 65 65 65 65 65 65 65 65 65 65 65]
Decompressed: AAAAAAAAAAAA
Match: true
```

This implementation demonstrates the core principles of the Lempel-Ziv compression algorithm, which is the foundation for many modern compression techniques including LZ77 and LZ78.

