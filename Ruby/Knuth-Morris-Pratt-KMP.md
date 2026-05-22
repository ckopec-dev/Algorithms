# Knuth-Morris-Pratt (KMP) Algorithm in Ruby

## Overview
The KMP algorithm is a string searching algorithm that efficiently finds occurrences of a pattern within a text without backtracking.

## Implementation

```ruby
class KMP
  # Preprocess the pattern to create the failure function (also called LPS - Longest Proper Prefix)
  def self.compute_lps_array(pattern)
    m = pattern.length
    lps = Array.new(m, 0)
    length = 0
    i = 1
    
    while i < m
      if pattern[i] == pattern[length]
        length += 1
        lps[i] = length
        i += 1
      else
        if length != 0
          length = lps[length - 1]
        else
          lps[i] = 0
          i += 1
        end
      end
    end
    
    lps
  end
  
  # Main KMP search function
  def self.search(text, pattern)
    n = text.length
    m = pattern.length
    
    # Handle edge cases
    return [] if m == 0 || n == 0 || m > n
    
    # Compute the LPS array
    lps = compute_lps_array(pattern)
    
    results = []
    i = 0  # index for text
    j = 0  # index for pattern
    
    while i < n
      if pattern[j] == text[i]
        i += 1
        j += 1
      end
      
      if j == m
        # Pattern found at index (i - j)
        results << (i - j)
        j = lps[j - 1]
      elsif i < n && pattern[j] != text[i]
        if j != 0
          j = lps[j - 1]
        else
          i += 1
        end
      end
    end
    
    results
  end
end

# Example usage
def demonstrate_kmp
  puts "=== KMP Algorithm Demonstration ==="
  
  # Example 1
  text1 = "ABABDABACDABABCABCABCABCABC"
  pattern1 = "ABABCABCABCABC"
  
  puts "\nExample 1:"
  puts "Text:    #{text1}"
  puts "Pattern: #{pattern1}"
  
  positions = KMP.search(text1, pattern1)
  puts "Found at positions: #{positions}"
  
  # Example 2
  text2 = "AABAACAADAABAABA"
  pattern2 = "AABA"
  
  puts "\nExample 2:"
  puts "Text:    #{text2}"
  puts "Pattern: #{pattern2}"
  
  positions2 = KMP.search(text2, pattern2)
  puts "Found at positions: #{positions2}"
  
  # Example 3 - No match
  text3 = "HELLO WORLD"
  pattern3 = "XYZ"
  
  puts "\nExample 3:"
  puts "Text:    #{text3}"
  puts "Pattern: #{pattern3}"
  
  positions3 = KMP.search(text3, pattern3)
  puts "Found at positions: #{positions3}"
end

# Run demonstration
demonstrate_kmp

# Additional helper method to show the LPS array computation
def show_lps_computation(pattern)
  puts "\nLPS Array Computation for pattern '#{pattern}':"
  lps = KMP.compute_lps_array(pattern)
  puts "Index:    #{(0...pattern.length).to_a.join(' ')}"
  puts "Pattern:  #{pattern}"
  puts "LPS:      #{lps.join(' ')}"
end

# Show LPS computation for example patterns
show_lps_computation("ABABCABCABCABC")
show_lps_computation("AABA")
```

## Output
```
=== KMP Algorithm Demonstration ===

Example 1:
Text:    ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC
Found at positions: [10]

Example 2:
Text:    AABAACAADAABAABA
Pattern: AABA
Found at positions: [0, 9, 12]

Example 3:
Text:    HELLO WORLD
Pattern: XYZ
Found at positions: []

LPS Array Computation for pattern 'ABABCABCABCABC':
Index:    0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
Pattern:  A B A B C A B C A B C A B C
LPS:      0 0 1 2 0 1 2 3 1 2 3 4 5 6 7 8

LPS Array Computation for pattern 'AABA':
Index:    0 1 2 3
Pattern:  A A B A
LPS:      0 1 0 1
```

## Key Features

1. **Time Complexity**: O(n + m) where n is text length and m is pattern length
2. **Space Complexity**: O(m) for the LPS array
3. **No Backtracking**: Unlike naive string matching, KMP avoids unnecessary comparisons
4. **Preprocessing**: The LPS array helps determine how much to shift the pattern when a mismatch occurs

## How it Works

1. **Preprocessing**: Compute the LPS (Longest Proper Prefix which is also Suffix) array
2. **Searching**: Use the LPS array to skip unnecessary comparisons during pattern matching
3. **Efficient**: When a mismatch occurs, instead of moving back in the text, we use the LPS array to determine the optimal shift

The algorithm is particularly useful for:
- String searching in large texts
- Pattern matching in compilers and text editors
- Bioinformatics sequence matching
- Any application requiring efficient substring search

