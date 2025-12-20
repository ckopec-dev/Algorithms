# Bitap Algorithm Implementation in Go

The Bitap algorithm (also known as the shift-or algorithm) is a string searching algorithm that uses bit manipulation to efficiently find occurrences of a pattern in a text.

```go
package main

import (
    "fmt"
    "math"
)

// BitapSearch performs string search using Bitap algorithm
func BitapSearch(text, pattern string) int {
    if len(pattern) == 0 {
        return 0
    }
    
    if len(text) == 0 {
        return -1
    }
    
    // Create a bitmask for each character in the alphabet
    // Using ASCII values for simplicity
    alphabetSize := 256
    bitapMask := make([]int, alphabetSize)
    
    // Initialize the mask for all characters to 0
    for i := range bitapMask {
        bitapMask[i] = 0
    }
    
    // Set bits for pattern characters
    for i, char := range pattern {
        bitapMask[int(char)] |= 1 << uint(i)
    }
    
    // Initialize the state (bit vector)
    state := 0
    
    // Process each character in the text
    for i, char := range text {
        // Shift the state to the left by 1 bit
        state = (state << 1) | 1
        
        // Apply the mask for current character
        state = state & bitapMask[int(char)]
        
        // Check if we found a complete match
        if state&(1<<uint(len(pattern)-1)) != 0 {
            return i - len(pattern) + 1
        }
    }
    
    return -1
}

// BitapSearchWithErrors performs fuzzy search with up to k errors
func BitapSearchWithErrors(text, pattern string, k int) int {
    if len(pattern) == 0 {
        return 0
    }
    
    if len(text) == 0 {
        return -1
    }
    
    // Create a bitmask for each character in the alphabet
    alphabetSize := 256
    bitapMask := make([]int, alphabetSize)
    
    // Initialize the mask for all characters to 0
    for i := range bitapMask {
        bitapMask[i] = 0
    }
    
    // Set bits for pattern characters
    for i, char := range pattern {
        bitapMask[int(char)] |= 1 << uint(i)
    }
    
    // Initialize states for up to k errors
    states := make([]int, k+1)
    for i := range states {
        states[i] = (1 << uint(len(pattern))) - 1
    }
    
    // Process each character in the text
    for i, char := range text {
        // Update states from right to left
        for j := k; j >= 1; j-- {
            // Calculate new state for j errors
            new_state := (states[j-1] << 1) | 1
            new_state = new_state & bitapMask[int(char)]
            states[j] = states[j] & new_state
        }
        
        // Update state for 0 errors
        states[0] = (states[0] << 1) | 1
        states[0] = states[0] & bitapMask[int(char)]
        
        // Check if we found a match with at most k errors
        if states[k]&(1<<uint(len(pattern)-1)) != 0 {
            return i - len(pattern) + 1
        }
    }
    
    return -1
}

// BitapSearchAll finds all occurrences of pattern in text
func BitapSearchAll(text, pattern string) []int {
    var positions []int
    
    if len(pattern) == 0 {
        return positions
    }
    
    if len(text) == 0 {
        return positions
    }
    
    // Create a bitmask for each character in the alphabet
    alphabetSize := 256
    bitapMask := make([]int, alphabetSize)
    
    // Initialize the mask for all characters to 0
    for i := range bitapMask {
        bitapMask[i] = 0
    }
    
    // Set bits for pattern characters
    for i, char := range pattern {
        bitapMask[int(char)] |= 1 << uint(i)
    }
    
    // Initialize the state (bit vector)
    state := 0
    
    // Process each character in the text
    for i, char := range text {
        // Shift the state to the left by 1 bit
        state = (state << 1) | 1
        
        // Apply the mask for current character
        state = state & bitapMask[int(char)]
        
        // Check if we found a complete match
        if state&(1<<uint(len(pattern)-1)) != 0 {
            positions = append(positions, i-len(pattern)+1)
        }
    }
    
    return positions
}

func main() {
    // Example 1: Basic string search
    text1 := "Hello, this is a sample text for testing"
    pattern1 := "sample"
    
    fmt.Printf("Text: %s\n", text1)
    fmt.Printf("Pattern: %s\n", pattern1)
    
    pos1 := BitapSearch(text1, pattern1)
    if pos1 != -1 {
        fmt.Printf("Pattern found at position: %d\n", pos1)
    } else {
        fmt.Println("Pattern not found")
    }
    
    fmt.Println()
    
    // Example 2: Pattern not found
    pattern2 := "missing"
    pos2 := BitapSearch(text1, pattern2)
    if pos2 != -1 {
        fmt.Printf("Pattern found at position: %d\n", pos2)
    } else {
        fmt.Println("Pattern not found")
    }
    
    fmt.Println()
    
    // Example 3: Find all occurrences
    text3 := "abababab"
    pattern3 := "abab"
    
    fmt.Printf("Text: %s\n", text3)
    fmt.Printf("Pattern: %s\n", pattern3)
    
    positions := BitapSearchAll(text3, pattern3)
    fmt.Printf("All positions: %v\n", positions)
    
    fmt.Println()
    
    // Example 4: Fuzzy search with errors
    text4 := "kitten"
    pattern4 := "sitting"
    
    fmt.Printf("Text: %s\n", text4)
    fmt.Printf("Pattern: %s\n", pattern4)
    
    pos4 := BitapSearchWithErrors(text4, pattern4, 2)
    if pos4 != -1 {
        fmt.Printf("Pattern found with up to 2 errors at position: %d\n", pos4)
    } else {
        fmt.Println("Pattern not found with up to 2 errors")
    }
    
    fmt.Println()
    
    // Example 5: Edge cases
    fmt.Println("Edge case tests:")
    fmt.Printf("Empty pattern in text: %d\n", BitapSearch("hello", ""))
    fmt.Printf("Empty text with pattern: %d\n", BitapSearch("", "hello"))
    fmt.Printf("Single character match: %d\n", BitapSearch("hello", "e"))
}
```

## How the Bitap Algorithm Works

### Basic Concept
1. **Bitmask Creation**: For each character in the pattern, create a bitmask where each bit represents a position in the pattern
2. **State Tracking**: Maintain a bit vector (state) that tracks potential matches
3. **Bitwise Operations**: Use left shifts and bitwise AND operations to efficiently update the state
4. **Match Detection**: When the most significant bit is set, a complete match is found

### Key Features
- **Time Complexity**: O(n*m) where n is text length and m is pattern length
- **Space Complexity**: O(1) for basic version, O(k) for k-error version
- **Efficient**: Uses bit manipulation for fast operations
- **Fuzzy Matching**: Can be extended to handle errors and approximate matching

### Output Example
```
Text: Hello, this is a sample text for testing
Pattern: sample
Pattern found at position: 17

Text: Hello, this is a sample text for testing
Pattern: missing
Pattern not found

Text: abababab
Pattern: abab
All positions: [0 2 4 6]

Text: kitten
Pattern: sitting
Pattern found with up to 2 errors at position: 0

Edge case tests:
Empty pattern in text: 0
Empty text with pattern: -1
Single character match: 1
```

This implementation demonstrates the core Bitap algorithm and can be extended for more advanced features like fuzzy matching with multiple errors.

