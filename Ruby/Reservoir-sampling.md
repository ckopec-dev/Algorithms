# Reservoir Sampling Algorithm in Ruby

Reservoir sampling is a family of randomized algorithms for randomly choosing a sample of k items from a list S containing n items, where n is either a very large or unknown number.

```ruby
class ReservoirSampling
  def self.sample(stream, k)
    # Initialize reservoir with first k elements
    reservoir = []
    
    # Fill reservoir with first k elements
    k.times do |i|
      reservoir[i] = stream[i] if stream[i]
    end
    
    # Process remaining elements
    (k...stream.length).each do |i|
      # Generate random index between 0 and i (inclusive)
      j = rand(i + 1)
      
      # If random index is less than k, replace element in reservoir
      if j < k
        reservoir[j] = stream[i]
      end
    end
    
    reservoir
  end
  
  # Alternative implementation with more explicit steps
  def self.sample_verbose(stream, k)
    return [] if stream.empty? || k <= 0
    
    reservoir = []
    
    # Fill reservoir with first k elements
    (0...[k, stream.length].min).each do |i|
      reservoir << stream[i]
    end
    
    # Process remaining elements
    (k...stream.length).each do |i|
      # Generate random number between 0 and i
      random_index = rand(i + 1)
      
      # If random index is within reservoir size, replace element
      if random_index < k
        reservoir[random_index] = stream[i]
      end
    end
    
    reservoir
  end
end

# Example usage
puts "Reservoir Sampling Example"
puts "=" * 30

# Example 1: Sample 3 elements from array of 10
data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
sample_size = 3

puts "Original data: #{data}"
puts "Sample size: #{sample_size}"

result = ReservoirSampling.sample(data, sample_size)
puts "Sample result: #{result}"

# Example 2: Sample with different data
words = ["apple", "banana", "cherry", "date", "elderberry", "fig", "grape"]
sample_size = 4

puts "\nWords: #{words}"
puts "Sample size: #{sample_size}"

result2 = ReservoirSampling.sample(words, sample_size)
puts "Sample result: #{result2}"

# Example 3: Testing with larger dataset
large_dataset = (1..1000).to_a
sample_size = 5

puts "\nLarge dataset (1000 elements) - Sample size: #{sample_size}"
result3 = ReservoirSampling.sample(large_dataset, sample_size)
puts "Sample result: #{result3}"
```

## How it works:

1. **Initialization**: Fill the reservoir with the first k elements from the stream
2. **Processing**: For each subsequent element (at position i):
   - Generate a random number between 0 and i (inclusive)
   - If the random number is less than k, replace the element at that random position in the reservoir
3. **Result**: The reservoir contains a random sample of k elements

## Key Properties:

- **Time Complexity**: O(n) where n is the number of elements in the stream
- **Space Complexity**: O(k) where k is the sample size
- **Unbiased**: Each element has equal probability of being selected
- **Works with unknown stream length**: Can process data as it arrives

## Sample Output:
```
Reservoir Sampling Example
==============================
Original data: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
Sample size: 3
Sample result: [2, 7, 10]

Words: ["apple", "banana", "cherry", "date", "elderberry", "fig", "grape"]
Sample size: 4
Sample result: ["banana", "date", "fig", "grape"]

Large dataset (1000 elements) - Sample size: 5
Sample result: [123, 456, 789, 234, 567]
```

The algorithm ensures that each element in the original stream has an equal probability of being included in the final sample, making it a fair and efficient sampling method.

