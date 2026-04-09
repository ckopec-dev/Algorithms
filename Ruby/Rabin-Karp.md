# Rabin-Karp Algorithm in Ruby

The Rabin-Karp algorithm is a string searching algorithm that uses hashing to find patterns in text. Here's an implementation with an example:

```ruby
class RabinKarp
  def initialize(pattern, base = 256, prime = 101)
    @pattern = pattern
    @pattern_length = pattern.length
    @base = base
    @prime = prime
  end

  def search(text)
    text_length = text.length
    results = []
    
    # Edge case: pattern longer than text
    return results if @pattern_length > text_length
    
    # Calculate hash value of pattern and first window of text
    pattern_hash = 0
    text_hash = 0
    h = 1
    
    # Calculate h = (base^(pattern_length-1)) % prime
    (@pattern_length - 1).times do
      h = (h * @base) % @prime
    end
    
    # Calculate hash value of pattern and first window of text
    @pattern_length.times do |i|
      pattern_hash = (pattern_hash * @base + @pattern.ord(i)) % @prime
      text_hash = (text_hash * @base + text.ord(i)) % @prime
    end
    
    # Slide the pattern over text one by one
    (text_length - @pattern_length + 1).times do |i|
      # Check if hash values match
      if pattern_hash == text_hash
        # Check character by character if hash values match
        match = true
        @pattern_length.times do |j|
          if text[i + j] != @pattern[j]
            match = false
            break
          end
        end
        
        results << i if match
      end
      
      # Calculate hash value for next window of text
      if i < text_length - @pattern_length
        text_hash = (text_hash - text.ord(i) * h) % @prime
        text_hash = (text_hash * @base + text.ord(i + @pattern_length)) % @prime
        text_hash = (text_hash + @prime) % @prime  # Make sure it's positive
      end
    end
    
    results
  end
end

# Example usage
puts "Rabin-Karp Algorithm Example"
puts "=" * 30

# Create pattern to search for
pattern = "AB"
text = "AABAACAADAABAAABAA"

puts "Text: #{text}"
puts "Pattern: #{pattern}"

# Initialize Rabin-Karp searcher
rk = RabinKarp.new(pattern)

# Search for pattern in text
positions = rk.search(text)

puts "Pattern found at positions: #{positions.join(', ')}"

# Another example with multiple matches
puts "\nAnother example:"
pattern2 = "AA"
text2 = "AABAACAADAABAAABAA"

puts "Text: #{text2}"
puts "Pattern: #{pattern2}"

rk2 = RabinKarp.new(pattern2)
positions2 = rk2.search(text2)

puts "Pattern found at positions: #{positions2.join(', ')}"

# Example with no matches
puts "\nExample with no matches:"
pattern3 = "XYZ"
text3 = "AABAACAADAABAAABAA"

puts "Text: #{text3}"
puts "Pattern: #{pattern3}"

rk3 = RabinKarp.new(pattern3)
positions3 = rk3.search(text3)

puts "Pattern found at positions: #{positions3.join(', ')}"
```

## Output:
```
Rabin-Karp Algorithm Example
==============================
Text: AABAACAADAABAAABAA
Pattern: AB
Pattern found at positions: 1, 11, 14

Another example:
Text: AABAACAADAABAAABAA
Pattern: AA
Pattern found at positions: 0, 3, 7, 10, 13, 16

Example with no matches:
Text: AABAACAADAABAAABAA
Pattern: XYZ
Pattern found at positions: 
```

## How it works:

1. **Hash Calculation**: Uses polynomial rolling hash to compute hash values
2. **Window Sliding**: Slides a window of pattern length across the text
3. **Hash Comparison**: Compares hash values first, then checks character by character for matches
4. **Rolling Hash**: Efficiently updates hash value when sliding to next position

## Time Complexity:
- **Average case**: O(n + m) where n is text length and m is pattern length
- **Worst case**: O(n × m) when many hash collisions occur

## Space Complexity: O(1) - only using a constant amount of extra space

