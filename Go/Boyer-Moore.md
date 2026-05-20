# Boyer-Moore Algorithm Implementation in Go

Here's a complete implementation of the Boyer-Moore string search algorithm in Go:

```go
package main

import (
    "fmt"
    "strings"
)

// BadCharacterTable creates the bad character heuristic table
func badCharacterTable(pattern string) []int {
    table := make([]int, 256) // ASCII table
    
    // Initialize all positions to -1
    for i := range table {
        table[i] = -1
    }
    
    // Fill the table with last occurrence positions
    for i, char := range pattern {
        table[char] = i
    }
    
    return table
}

// GoodSuffixTable creates the good suffix heuristic table
func goodSuffixTable(pattern string) []int {
    m := len(pattern)
    table := make([]int, m+1)
    
    // Initialize all positions to m
    for i := range table {
        table[i] = m
    }
    
    // Build the table using the reverse suffix approach
    i := m
    j := m + 1
    table[i] = j
    
    for i > 0 {
        i--
        for j <= m && pattern[i] != pattern[j-1] {
            if table[j] == m {
                table[j] = j - i
            }
            j = table[j]
        }
        table[i] = j - 1
        j--
    }
    
    return table
}

// BoyerMooreSearch searches for pattern in text using Boyer-Moore algorithm
func BoyerMooreSearch(text, pattern string) int {
    if len(pattern) == 0 {
        return 0
    }
    
    if len(text) < len(pattern) {
        return -1
    }
    
    // Create bad character table
    badCharTable := badCharacterTable(pattern)
    
    // Create good suffix table
    goodSuffixTable := goodSuffixTable(pattern)
    
    n := len(text)
    m := len(pattern)
    i := 0
    
    for i <= n-m {
        j := m - 1
        
        // Match from right to left
        for j >= 0 && text[i+j] == pattern[j] {
            j--
        }
        
        // If pattern is found
        if j < 0 {
            return i
        }
        
        // Calculate shift using bad character rule
        badCharShift := j - badCharTable[text[i+j]]
        
        // Calculate shift using good suffix rule
        goodSuffixShift := goodSuffixTable[j+1]
        
        // Take maximum shift
        shift := max(badCharShift, goodSuffixShift)
        i += shift
    }
    
    return -1
}

// Helper function to find maximum of two integers
func max(a, b int) int {
    if a > b {
        return a
    }
    return b
}

func main() {
    // Test cases
    text := "ABAAABCDABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABC

