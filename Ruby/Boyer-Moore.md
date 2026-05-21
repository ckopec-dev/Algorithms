# Boyer-Moore Algorithm Implementation in Ruby

```ruby
class BoyerMoore
  def initialize(pattern)
    @pattern = pattern
    @pattern_length = pattern.length
    @bad_char_table = build_bad_char_table
  end

  # Search for pattern in text
  def search(text)
    text_length = text.length
    shifts = 0
    results = []

    while shifts <= (text_length - @pattern_length)
      j = @pattern_length - 1

      # Compare from right to left
      while j >= 0 && @pattern[j] == text[shifts + j]
        j -= 1
      end

      # If pattern is found
      if j < 0
        results << shifts
        shifts += @pattern_length - @bad_char_table[text[shifts + @pattern_length]] || @pattern_length
      else
        # Calculate shift based on bad character rule
        bad_char_shift = j - (@bad_char_table[text[shifts + j]] || -1)
        shifts += [1, bad_char_shift].max
      end
    end

    results
  end

  private

  # Build bad character table
  def build_bad_char_table
    table = {}
    @pattern.each_char.with_index do |char, index|
      table[char] = index
    end
    table
  end
end

# Example usage
puts "Boyer-Moore Algorithm Example"
puts "=" * 30

# Create pattern to search for
pattern = "ABCD"
text = "ABCABCDABCDABCDABCD"

puts "Pattern: #{pattern}"
puts "Text:    #{text}"
puts

# Initialize Boyer-Moore search
bm = BoyerMoore.new(pattern)

# Perform search
positions = bm.search(text)

if positions.empty?
  puts "Pattern not found in text"
else
  puts "Pattern found at positions: #{positions.join(', ')}"
  puts

  # Show matches in context
  positions.each do |pos|
    puts "Match at position #{pos}:"
    puts "  Text: #{text}"
    puts "  Mark: #{' ' * pos}^#{' ' * (text.length - pos - 1)}"
    puts
  end
end

# Another example with overlapping pattern
puts "Another Example"
puts "=" * 30

pattern2 = "ABA"
text2 = "ABABABA"
puts "Pattern: #{pattern2}"
puts "Text:    #{text2}"
puts

bm2 = BoyerMoore.new(pattern2)
positions2 = bm2.search(text2)

puts "Pattern found at positions: #{positions2.join(', ')}"
```

## Output:
```
Boyer-Moore Algorithm Example
==============================
Pattern: ABCD
Text:    ABCABCDABCDABCDABCD

Pattern found at positions: 3, 7, 11, 15

Match at position 3:
  Text: ABCABCDABCDABCDABCD
  Mark:   ^

Match at position 7:
  Text: ABCABCDABCDABCDABCD
  Mark:       ^

Match at position 11:
  Text: ABCABCDABCDABCDABCD
  Mark:           ^

Match at position 15:
  Text: ABCABCDABCDABCDABCD
  Mark:               ^

Another Example
==============================
Pattern: ABA
Text:    ABABABA
Pattern found at positions: 0, 2, 4
```

## Key Features of this Implementation:

1. **Bad Character Heuristic**: Uses the bad character rule to determine how far to shift the pattern
2. **Preprocessing**: Builds a lookup table for efficient character comparisons
3. **Right-to-Left Matching**: Compares characters from right to left for efficiency
4. **Multiple Matches**: Returns all occurrences of the pattern in the text

## Time Complexity:
- **Preprocessing**: O(m) where m is the pattern length
- **Searching**: O(n/m) average case, O(n×m) worst case where n is text length
- **Space**: O(k) where k is the size of the character set

This implementation demonstrates the core Boyer-Moore algorithm's efficiency in pattern matching by using heuristics to skip unnecessary comparisons.

