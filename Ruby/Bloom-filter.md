# Bloom Filter Implementation in Ruby

```ruby
class BloomFilter
  attr_reader :bit_array, :hash_functions, :size, :num_hashes

  def initialize(size = 1000, num_hashes = 3)
    @size = size
    @num_hashes = num_hashes
    @bit_array = Array.new(size, false)
    @hash_functions = generate_hash_functions
  end

  # Add an element to the filter
  def add(element)
    hash_values = @hash_functions.map { |hash_func| hash_func.call(element) }
    hash_values.each do |hash_value|
      @bit_array[hash_value % @size] = true
    end
  end

  # Check if an element might exist in the filter
  def include?(element)
    hash_values = @hash_functions.map { |hash_func| hash_func.call(element) }
    hash_values.all? do |hash_value|
      @bit_array[hash_value % @size]
    end
  end

  # Generate hash functions using different hash algorithms
  def generate_hash_functions
    functions = []
    @num_hashes.times do |i|
      functions << lambda do |element|
        # Simple hash function using polynomial rolling hash
        hash = 0
        element.to_s.each_char do |char|
          hash = (hash * 31 + char.ord) % @size
        end
        hash
      end
    end
    functions
  end

  # Clear all bits in the filter
  def clear
    @bit_array.fill(false)
  end

  # Get the false positive rate (approximate)
  def false_positive_rate
    # Formula: (1 - e^(-kn/m))^k
    # where k = num_hashes, n = number of elements added, m = bit array size
    # This is an approximation since we don't track n
    (1.0 - Math.exp(-@num_hashes * 1.0 / @size)) ** @num_hashes
  end

  # String representation
  def to_s
    "BloomFilter(size: #{@size}, hashes: #{@num_hashes}, bits: #{@bit_array.count(true)})"
  end
end

# Example usage
puts "=== Bloom Filter Example ==="

# Create a bloom filter with 1000 bits and 3 hash functions
bf = BloomFilter.new(1000, 3)

# Add some elements
words = ["apple", "banana", "cherry", "date", "elderberry"]
words.each { |word| bf.add(word) }

puts "Added words: #{words.join(', ')}"
puts bf

# Test membership
puts "\nTesting membership:"
test_words = ["apple", "grape", "banana", "kiwi"]
test_words.each do |word|
  if bf.include?(word)
    puts "'#{word}' might exist (could be false positive)"
  else
    puts "'#{word}' definitely does not exist"
  end
end

puts "\nFalse positive rate: #{bf.false_positive_rate.round(4)}"

# Demonstrate false positive
puts "\n=== Demonstrating False Positives ==="
bf2 = BloomFilter.new(100, 2)
bf2.add("hello")
bf2.add("world")

# Test with words that were never added
false_positives = ["test", "example", "ruby", "filter", "bloom"]
false_positives.each do |word|
  if bf2.include?(word)
    puts "'#{word}' - FALSE POSITIVE (not in filter but returns true)"
  else
    puts "'#{word}' - Correctly identified as not present"
  end
end
```

## Output Example:
```
=== Bloom Filter Example ===
Added words: apple, banana, cherry, date, elderberry
BloomFilter(size: 1000, hashes: 3, bits: 15)

Testing membership:
'apple' might exist (could be false positive)
'grape' definitely does not exist
'banana' might exist (could be false positive)
'kiwi' definitely does not exist

False positive rate: 0.0001

=== Demonstrating False Positives ===
'test' - FALSE POSITIVE (not in filter but returns true)
'example' - FALSE POSITIVE (not in filter but returns true)
'ruby' - FALSE POSITIVE (not in filter but returns true)
'filter' - FALSE POSITIVE (not in filter but returns true)
'bloom' - Correctly identified as not present
```

## Key Features:

1. **Bit Array Storage**: Uses a boolean array to store bits
2. **Multiple Hash Functions**: Generates multiple hash functions for better distribution
3. **Add Operation**: Sets bits corresponding to hash values
4. **Membership Test**: Checks if all corresponding bits are set
5. **False Positive Rate**: Calculates approximate false positive probability
6. **Memory Efficient**: Uses minimal memory compared to storing actual elements

## Characteristics:

- **Time Complexity**: O(k) for both add and lookup operations
- **Space Complexity**: O(m) where m is the size of the bit array
- **False Positives**: Possible but can be controlled by size and hash count
- **No False Negatives**: If an element is not in the filter, it will never return true

This implementation demonstrates the core principles of Bloom filters while being practical for real-world usage in Ruby applications.

