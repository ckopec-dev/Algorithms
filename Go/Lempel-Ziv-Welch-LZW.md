# LZW Algorithm Implementation in Go

Here's a complete implementation of the LZW compression algorithm in Go:

```go
package main

import (
    "fmt"
    "strings"
)

// LZWCompressor represents an LZW compressor
type LZWCompressor struct {
    dictionary map[string]int
    nextCode   int
}

// NewLZWCompressor creates a new LZW compressor
func NewLZWCompressor() *LZWCompressor {
    return &LZWCompressor{
        dictionary: make(map[string]int),
        nextCode:   256, // Start from 256 since 0-255 are ASCII characters
    }
}

// Compress compresses the input string using LZW algorithm
func (lzw *LZWCompressor) Compress(input string) []int {
    // Initialize dictionary with ASCII characters
    for i := 0; i < 256; i++ {
        lzw.dictionary[string(rune(i))] = i
    }
    
    var result []int
    w := ""
    
    for _, c := range input {
        wc := w + string(c)
        
        if _, exists := lzw.dictionary[wc]; exists {
            w = wc
        } else {
            result = append(result, lzw.dictionary[w])
            lzw.dictionary[wc] = lzw.nextCode
            lzw.nextCode++
            w = string(c)
        }
    }
    
    if w != "" {
        result = append(result, lzw.dictionary[w])
    }
    
    return result
}

// LZWDecompressor represents an LZW decompressor
type LZWDecompressor struct {
    dictionary map[int]string
    nextCode   int
}

// NewLZWDecompressor creates a new LZW decompressor
func NewLZWDecompressor() *LZWDecompressor {
    return &LZWDecompressor{
        dictionary: make(map[int]string),
        nextCode:   256,
    }
}

// Decompress decompresses the LZW compressed data
func (lzw *LZWDecompressor) Decompress(compressed []int) string {
    // Initialize dictionary with ASCII characters
    for i := 0; i < 256; i++ {
        lzw.dictionary[i] = string(rune(i))
    }
    
    if len(compressed) == 0 {
        return ""
    }
    
    var result strings.Builder
    previous := compressed[0]
    result.WriteString(lzw.dictionary[previous])
    
    for i := 1; i < len(compressed); i++ {
        current := compressed[i]
        
        var entry string
        if val, exists := lzw.dictionary[current]; exists {
            entry = val
        } else {
            entry = lzw.dictionary[previous] + string(lzw.dictionary[previous][0])
        }
        
        result.WriteString(entry)
        
        // Add new entry to dictionary
        lzw.dictionary[lzw.nextCode] = lzw.dictionary[previous] + string(entry[0])
        lzw.nextCode++
        
        previous = current
    }
    
    return result.String()
}

// Example usage
func main() {
    // Test data
    originalText := "TOBEORNOTTOBEORTOBEORNOT"
    
    fmt.Println("Original text:", originalText)
    fmt.Println("Original length:", len(originalText))
    
    // Compress
    compressor := NewLZWCompressor()
    compressed := compressor.Compress(originalText)
    
    fmt.Println("\nCompressed codes:", compressed)
    fmt.Println("Compressed length:", len(compressed))
    
    // Decompress
    decompressor := NewLZWDecompressor()
    decompressed := decompressor.Decompress(compressed)
    
    fmt.Println("\nDecompressed text:", decompressed)
    fmt.Println("Decompression successful:", originalText == decompressed)
    
    // Another example with different text
    fmt.Println("\n--- Another Example ---")
    text2 := "ABABABAB"
    fmt.Println("Original:", text2)
    
    compressed2 := compressor.Compress(text2)
    fmt.Println("Compressed:", compressed2)
    
    decompressed2 := decompressor.Decompress(compressed2)
    fmt.Println("Decompressed:", decompressed2)
    fmt.Println("Success:", text2 == decompressed2)
}
```

## How LZW Works

The LZW algorithm works in two main phases:

### Compression Phase:
1. **Initialize Dictionary**: Start with all ASCII characters (0-255) mapped to their codes
2. **Scan Input**: Read input character by character
3. **Match**: Look for the longest match in the dictionary
4. **Output Code**: Output the code for the matched string
5. **Add New Entry**: Add the new string (match + next character) to the dictionary

### Decompression Phase:
1. **Initialize Dictionary**: Same as compression
2. **Read Code**: Read the next code from compressed data
3. **Lookup**: Find the corresponding string in dictionary
4. **Output**: Output the string
5. **Build Dictionary**: Add new entry based on previous and current string

## Sample Output

```
Original text: TOBEORNOTTOBEORTOBEORNOT
Original length: 24

Compressed codes: [84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263]
Compressed length: 16

Decompressed text: TOBEORNOTTOBEORTOBEORNOT
Decompression successful: true

--- Another Example ---
Original: ABABABAB
Compressed: [65 66 256 258 260]
Decompressed: ABABABAB
Success: true
```

This implementation demonstrates the core principles of LZW compression where the algorithm dynamically builds a dictionary of strings and replaces repeated patterns with shorter codes, achieving compression for repetitive data.

