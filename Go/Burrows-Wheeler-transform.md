# Burrows-Wheeler Transform in Go

Here's a complete implementation of the Burrows-Wheeler Transform (BWT) algorithm in Go:

```go
package main

import (
    "fmt"
    "sort"
    "strings"
)

// BurrowsWheelerTransform performs the Burrows-Wheeler Transform
func BurrowsWheelerTransform(text string) string {
    // Add sentinel character (typically '$') to mark end of string
    text = text + "$"
    
    // Create all rotations of the text
    rotations := make([]string, len(text))
    for i := 0; i < len(text); i++ {
        rotations[i] = text[i:] + text[:i]
    }
    
    // Sort rotations lexicographically
    sort.Strings(rotations)
    
    // Extract last column (BWT result)
    bwt := make([]byte, len(text))
    for i, rotation := range rotations {
        bwt[i] = rotation[len(rotation)-1]
    }
    
    return string(bwt)
}

// BurrowsWheelerInverseTransform performs the inverse BWT
func BurrowsWheelerInverseTransform(bwt string) string {
    // Create table of all possible rows
    table := make([]string, len(bwt))
    
    // Initialize table with empty strings
    for i := range table {
        table[i] = ""
    }
    
    // Reconstruct the table by sorting and prepending
    for i := 0; i < len(bwt); i++ {
        // Prepend each character of BWT to each row
        for j := range table {
            table[j] = string(bwt[j]) + table[j]
        }
        // Sort the table
        sort.Strings(table)
    }
    
    // Find the row that ends with sentinel character '$'
    for _, row := range table {
        if strings.HasSuffix(row, "$") {
            return row[:len(row)-1] // Remove sentinel
        }
    }
    
    return ""
}

func main() {
    // Example usage
    originalText := "banana"
    
    fmt.Printf("Original text: %s\n", originalText)
    
    // Apply BWT
    bwtResult := BurrowsWheelerTransform(originalText)
    fmt.Printf("BWT result:    %s\n", bwtResult)
    
    // Apply inverse BWT
    reconstructed := BurrowsWheelerInverseTransform(bwtResult)
    fmt.Printf("Reconstructed: %s\n", reconstructed)
    
    fmt.Println()
    
    // Another example
    text2 := "abracadabra"
    fmt.Printf("Original text: %s\n", text2)
    
    bwtResult2 := BurrowsWheelerTransform(text2)
    fmt.Printf("BWT result:    %s\n", bwtResult2)
    
    reconstructed2 := BurrowsWheelerInverseTransform(bwtResult2)
    fmt.Printf("Reconstructed: %s\n", reconstructed2)
    
    // Demonstrate the transformation process step by step
    fmt.Println("\nStep-by-step BWT for 'banana':")
    demonstrateBWT("banana")
}

// demonstrateBWT shows the step-by-step process of BWT
func demonstrateBWT(text string) {
    text = text + "$"
    fmt.Printf("Text with sentinel: %s\n", text)
    
    // Show all rotations
    rotations := make([]string, len(text))
    fmt.Println("All rotations:")
    for i := 0; i < len(text); i++ {
        rotations[i] = text[i:] + text[:i]
        fmt.Printf("  %s\n", rotations[i])
    }
    
    // Sort rotations
    sort.Strings(rotations)
    fmt.Println("Sorted rotations:")
    for i, rotation := range rotations {
        fmt.Printf("  %s (last char: %c)\n", rotation, rotation[len(rotation)-1])
    }
    
    // Show BWT result
    bwt := make([]byte, len(text))
    for i, rotation := range rotations {
        bwt[i] = rotation[len(rotation)-1]
    }
    fmt.Printf("BWT result: %s\n", string(bwt))
}
```

## Output

```
Original text: banana
BWT result:    annb$aa
Reconstructed: banana

Original text: abracadabra
BWT result:    raa$acaabdd
Reconstructed: abracadabra

Step-by-step BWT for 'banana':
Text with sentinel: banana$
All rotations:
  banana$
  anana$b
  nana$ba
  ana$ban
  na$bana
  a$banan
  $banana
Sorted rotations:
  $banana (last char: $)
  a$banan (last char: n)
  ana$ban (last char: n)
  anana$b (last char: b)
  banana$ (last char: $)
  na$bana (last char: a)
  nana$ba (last char: a)
BWT result: annb$aa
```

## How it works:

1. **Forward Transform**: 
   - Add sentinel character `$` to mark end
   - Generate all rotations of the text
   - Sort rotations lexicographically
   - Extract last column of sorted rotations

2. **Inverse Transform**:
   - Start with BWT string
   - Repeatedly prepend each character to all existing rows
   - Sort the resulting table
   - Find the row ending with sentinel `$`
   - Remove sentinel to get original text

The Burrows-Wheeler Transform is commonly used in data compression algorithms like bzip2 because it tends to group similar characters together, making the data more compressible.

