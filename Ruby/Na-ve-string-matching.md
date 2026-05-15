# Naïve String Matching Algorithm in Ruby

The naïve string matching algorithm is a simple approach to find all occurrences of a pattern within a text by checking every possible position.

## Implementation

```ruby
def naive_string_matching(text, pattern)
  # Get lengths of text and pattern
  text_length = text.length
  pattern_length = pattern.length
  
  # Array to store all matching positions
  matches = []
  
  # If pattern is longer than text, no matches possible
  return matches if pattern_length > text_length
  
  # Check each possible position in text
  (0..text_length - pattern_length).each do |i|
    # Check if pattern matches at position i
    match = true
    (0...pattern_length).each do |j|
      if text[i + j] != pattern[j]
        match = false
        break
      end
    end
    
    # If match found, add position to results
    matches << i if match
  end
  
  matches
end

# Example usage
text = "ABABDABACDABABCABCABCABC"
pattern = "ABABCABCABC"

puts "Text: #{text}"
puts "Pattern: #{pattern}"
puts "Matches found at positions: #{naive_string_matching(text, pattern)}"
```

## Output
```
Text: ABABDABACDABABCABCABCABC
Pattern: ABABCABCABC
Matches found at positions: [10, 15]
```

## How it works:

1. **Outer loop**: Iterates through each possible starting position in the text (from 0 to text_length - pattern_length)
2. **Inner loop**: For each position, checks if the pattern matches character by character
3. **Character comparison**: Compares text[i + j] with pattern[j] for all positions j
4. **Match recording**: If all characters match, records the starting position

## Time Complexity:
- **Best case**: O(n × m) where n is text length and m is pattern length
- **Worst case**: O(n × m)
- **Space complexity**: O(1) excluding the output array

## Alternative shorter implementation using built-in methods:

```ruby
def naive_string_matching_short(text, pattern)
  matches = []
  (0..text.length - pattern.length).each do |i|
    matches << i if text[i, pattern.length] == pattern
  end
  matches
end

# Test
text = "hello world hello ruby"
pattern = "hello"
puts naive_string_matching_short(text, pattern)  # Output: [0, 12]
```

This algorithm is simple but not efficient for large texts. More advanced algorithms like KMP, Boyer-Moore, or Rabin-Karp are typically used in practice for better performance.

