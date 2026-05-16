# Bitap Algorithm Implementation in Go

The Bitap algorithm is a string searching algorithm that uses bitwise operations to find occurrences of a pattern in a text. Here's a complete implementation:

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
    
    // Create the alphabet table
    alphabet := make(map[rune]int)
    for i, char := range pattern {
        alphabet[char] = i
    }
    
    // Initialize the bit array
    patternLen := len(pattern)
    bitmask := 1
    shift := make([]int, patternLen+1)
    
    // Initialize shift array
    for i := 0; i <= patternLen; i++ {
        shift[i] = bitmask
        bitmask <<= 1
    }
    
    // Process each character in the text
    for i, char := range text {
        // Update the bitmask
        if val, exists := alphabet[char]; exists {
            shift[0] |= 1 << val
        }
        
        // Shift the bitmask
        for j := 1; j <= patternLen; j++ {
            if j <= len(pattern) {
                shift[j] = (shift[j] >> 1) | shift[0]
            }
        }
        
        // Check if we found a match
        if shift[patternLen] == 0 {
            return i - patternLen + 1
        }
    }
    
    return -1
}

// Alternative implementation with better bit manipulation
func BitapSearchOptimized(text, pattern string) int {
    if len(pattern) == 0 {
        return 0
    }
    
    if len(text) == 0 {
        return -1
    }
    
    patternLen := len(pattern)
    
    // Create the alphabet table
    alphabet := make(map[rune]int)
    for i, char := range pattern {
        alphabet[char] = 1 << i
    }
    
    // Initialize the bit array
    R := 0
    
    // Process each character in the text
    for i, char := range text {
        // Update R with the character's bit position
        if val, exists := alphabet[char]; exists {
            R |= val
        }
        
        // Shift R to the left
        R <<= 1
        
        // Clear the bit if we've matched the pattern
        if i >= patternLen-1 && R&1 == 0 {
            return i - patternLen + 1
        }
    }
    
    return -1
}

// Complete Bitap implementation with error handling
func BitapSearchComplete(text, pattern string) (int, error) {
    if text == "" {
        return -1, fmt.Errorf("text cannot be empty")
    }
    
    if pattern == "" {
        return 0, nil
    }
    
    patternLen := len(pattern)
    textLen := len(text)
    
    if patternLen > textLen {
        return -1, fmt.Errorf("pattern longer than text")
    }
    
    // Create the alphabet table
    alphabet := make(map[rune]int)
    for i, char := range pattern {
        alphabet[char] = i
    }
    
    // Initialize the bit array
    bitmask := 1
    shift := make([]int, patternLen+1)
    
    // Initialize shift array
    for i := 0; i <= patternLen; i++ {
        shift[i] = bitmask
        bitmask <<= 1
    }
    
    // Process each character in the text
    for i, char := range text {
        // Update the bitmask
        if val, exists := alphabet[char]; exists {
            shift[0] |= 1 << val
        }
        
        // Shift the bitmask
        for j := 1; j <= patternLen; j++ {
            if j <= len(pattern) {
                shift[j] = (shift[j] >> 1) | shift[0]
            }
        }
        
        // Check if we found a match
        if shift[patternLen] == 0 {
            return i - patternLen + 1, nil
        }
    }
    
    return -1, nil
}

func main() {
    // Test cases
    testCases := []struct {
        text    string
        pattern string
        expected int
    }{
        {"hello world", "world", 6},
        {"abcdefg", "cde", 2},
        {"programming", "gram", 4},
        {"hello", "xyz", -1},
        {"aaaaaa", "aa", 0},
        {"", "test", -1},
        {"test", "", 0},
    }
    
    fmt.Println("Bitap Algorithm Test Results:")
    fmt.Println("==============================")
    
    for i, tc := range testCases {
        result := BitapSearch(tc.text, tc.pattern)
        status := "✓"
        if result != tc.expected {
            status = "✗"
        }
        
        fmt.Printf("Test %d: %s\n", i+1, status)
        fmt.Printf("  Text: \"%s\"\n", tc.text)
        fmt.Printf("  Pattern: \"%s\"\n", tc.pattern)
        fmt.Printf("  Expected: %d, Got: %d\n", tc.expected, result)
        fmt.Println()
    }
    
    // Demonstrate the complete implementation
    fmt.Println("Complete Implementation Demo:")
    fmt.Println("============================")
    
    text := "The quick brown fox jumps over the lazy dog"
    pattern := "brown"
    
    result, err := BitapSearchComplete(text, pattern)
    if err != nil {
        fmt.Printf("Error: %v\n", err)
    } else {
        fmt.Printf("Text: \"%s\"\n", text)
        fmt.Printf("Pattern: \"%s\"\n", pattern)
        fmt.Printf("Found at position: %d\n", result)
    }
    
    // Test with no match
    pattern2 := "elephant"
    result2, err := BitapSearchComplete(text, pattern2)
    if err != nil {
        fmt.Printf("Error: %v\n", err)
    } else {
        fmt.Printf("\nText: \"%s\"\n", text)
        fmt.Printf("Pattern: \"%s\"\n", pattern2)
        fmt.Printf("Found at position: %d\n", result2)
    }
}
```

## How the Bitap Algorithm Works

The Bitap algorithm uses bitwise operations to efficiently search for patterns in text:

1. **Bitmask Representation**: Each character in the pattern is represented as a bit in a bitmask
2. **Shift Operations**: As we process each character in the text, we shift the bitmasks
3. **Match Detection**: When the final bitmask becomes zero, we've found a match

## Key Features

- **Time Complexity**: O(n × m) where n is text length and m is pattern length
- **Space Complexity**: O(m) for the bitmask arrays
- **Bitwise Operations**: Uses efficient bit manipulation for pattern matching
- **Error Handling**: Complete implementation with proper error checking

## Usage Examples

The algorithm can be used for:
- Text searching and pattern matching
- DNA sequence analysis
- File searching in large datasets
- Any application requiring efficient substring search

The implementation includes multiple versions to demonstrate different approaches to the Bitap algorithm, from basic to complete with error handling.

